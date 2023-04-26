##' Construct and apply the OmicSense model with your own data
##'
##' @title Construct and apply the OmicSense model with your own data
##' @param x A data matrix (row: samples, col: predictors).
##' @param y A vector of target value.
##' @param newx A data matrix (row: samples, col: predictors).
##' @param method A string to specify the regression function for calculating R-squared values.
##' "linear" (default), "quadratic" or "cubic" function can be specified.
##' @param n.pred The number of candidate predictors to be leaved in prediction model (default: 30).
##' @param thresh The lower threshold of R-squared value to be leaved in prediction model (default: 0).
##' @importFrom stats lm dnorm residuals dnorm predict quantile
##' @importFrom utils capture.output
##' @importFrom kernlab gausspr
##' @return A vector of the environment in which the samples of newx were collected
##' @examples
##' data(Pinus)
##' train <- os.clean(Pinus$train)
##' test <- Pinus$test
##' test <- test[, colnames(train)]
##' target <- Pinus$target
##' cor(target, os.pred(train, target, newx = test, method = "cubic"))
##' @author Takahiko Koizumi
##' @export
os.pred <- function(x, y, newx = x, method = "linear", thresh = 0, n.pred = 0){
  degree <- switch(method,
                   "linear" = 1,
                   "quadratic" = 2,
                   "cubic" = 3,
                   "mix" = 0,
                   "gaussian" = -1,
                   stop("Select the methods linear, quadratic, cubic, mix, or gaussian")
  )

  if(thresh > 0 & n.pred > 0){
    stop("Don't specify <n.pred> and <thresh> at a time")
  }
  if(n.pred == 0){
    n.pred <- 30
  }


  ## definitions
  ## adjust errors
  adj <- function(xa, ya, xb){
    cf <- summary(lm(ya ~ xa))$coefficients
    xb * cf[2, 1] + cf[1, 1]
  }

  ## interquartile range
  quant <- function(xa){
    q <- quantile(xa)
    mn <- q[2] - 1.5 * (q[4] - q[2])
    mx <- q[4] + 1.5 * (q[4] - q[2])
    (1:length(xa))[xa >= mn & xa <= mx]
  }

  ## OmicSense with polynomial regression
  sense.poly <- function(x.train, y.train, newx,
                         method = method, n.pred = n.pred, threshold = thresh){

    degree <- switch(method,
                     "linear" = 1,
                     "quadratic" = 2,
                     "cubic" = 3,
                     stop("Select the methods linear, quadratic, or cubic")
    )

    ## calculate R2 values
    result <- rep(NA, ncol(x.train))
    for(i in 1:ncol(x.train)){
      result[i] <- summary(lm(y.train ~ poly(x.train[, i], degree = degree, raw = TRUE)))$r.squared
    }

    ## sort predictors in descending order of R2
    x.train <- x.train[, order(result, decreasing = TRUE)]
    n.pred.thr <- length(result[result >= threshold])
    n.pred <- min(n.pred, n.pred.thr)
    x.train <- x.train[, 1:n.pred]
    x.test <- newx[, colnames(x.train)]

    ## calculate parameters for OmicSense modeling
    coef <- data.frame(matrix(rep(NA, ncol(x.train) * (degree + 1)), ncol(x.train), degree + 1))
    sd <- rep(NA, ncol(x.train))
    for(i in 1:ncol(x.train)){
      reg <- lm(y.train ~ poly(x.train[, i], degree = degree, raw = TRUE))
      coef[i, ] <- summary(reg)$coefficients[, 1]
      sd[i] <- sqrt(mean(residuals(reg)^2))
    }

    ## prediction using training data
    p1 <- data.frame(matrix(rep(NA, nrow(x.train) * n.pred), nrow(x.train), n.pred))
    for(i in 1:nrow(x.train)){
      for(j in 1:n.pred){
        x2 <- 0
        for(k in 1:(degree + 1)){
          x2 <- x2 + coef[j, k] * (x.train[i, j] ^ (k - 1))
        }
        p1[i, j] <- x2
      }
    }
    p1 <- apply(p1, 1, mean, na.rm = TRUE)

    ## prediction using test data
    p2 <- data.frame(matrix(rep(NA, nrow(x.test) * n.pred), nrow(x.test), n.pred))
    for(i in 1:nrow(x.test)){
      for(j in 1:n.pred){
        x2 <- 0
        for(k in 1:(degree + 1)){
          x2 <- x2 + coef[j, k] * (x.test[i, j] ^ (k - 1))
        }
        p2[i, j] <- x2
      }
    }
    q.in <- apply(p2, 1, quant)

    mn <- min(y.train)
    mx <- max(y.train)
    dd <- 2
    l <- round(mn - (mx-mn)/2, digits = dd)
    u <- round(mx + (mx-mn)/2, digits = dd)
    range <- ((l * (10^dd)):(u * (10^dd)))/(10^dd)

    p2[p2 <= l | p2 >= u] <- NA
    stack <- array(rep(NA, nrow(x.test) * n.pred * length(range)), c(nrow(x.test), n.pred, length(range)))
    for(j in 1:n.pred){
      stack[, j, ] <- dnorm(rep(range, each = nrow(x.test)), p2[, j], sd[j])
    }
    stack[, apply(x.test, 2, sum) == 0, ] <- NA #missing values
    for(i in 1:nrow(x.test)){
      stack[i, -q.in[[i]], ] <- NA #outliers
    }
    stk <- apply(stack, c(1, 3), mean, na.rm = TRUE)

    p2 <- rep(0, nrow(x.test))
    for (i in 1:nrow(x.test)){
      p2[i] <- range[which.max(stk[i, ])]
    }

    pred.data <- adj(p1, y.train, p2)
    names(pred.data) <- row.names(x.test)
    pred.data
  }



  #sense.mix
  sense.mix <- function(x.train, y.train, newx, n.pred = n.pred, threshold = thresh){

    dim <- rep(NA, ncol(x.train))
    aic.rec <- rep(10^5, ncol(x.train))
    r.rec <- rep(0, ncol(x.train))
    for(i in 1:ncol(x.train)){
      for(j in 1:3){
        lm <- lm(y.train ~ poly(x.train[, i], degree = j, raw = TRUE))
        rss <- sqrt(sum((y.train - predict(lm, data.frame(x.train[, i])))^2))
        aic <- nrow(x.train)*(log(2 * pi * rss/nrow(x.train)) + 1) + 2 * (j + 2)
        r <- summary(lm)$r.squared
        if(aic.rec[i] > aic){
          dim[i] <- j
          aic.rec[i] <- aic
          r.rec[i] <- r
        }
      }
    }

    ord <- order(r.rec, decreasing = TRUE)
    x.train <- x.train[, ord]
    dim <- dim[ord]
    n.pred.thr <- length(r.rec[r.rec >= threshold])
    n.pred <- min(n.pred, n.pred.thr)
    x.train <- x.train[, 1:n.pred]
    x.test <- newx[, colnames(x.train)]

    coef <- data.frame(matrix(rep(NA, ncol(x.train) * 4), ncol(x.train), 4))
    sd <- rep(NA, ncol(x.train))
    for(i in 1:ncol(x.train)){
      reg <- lm(y.train ~ poly(x.train[, i], degree = dim[i], raw = TRUE))
      coef[i, 1:(dim[i] + 1)] <- summary(reg)$coefficients[, 1]
      sd[i] <- sqrt(mean(residuals(reg)^2))
    }

    p1 <- data.frame(matrix(rep(NA, nrow(x.train) * n.pred), nrow(x.train), n.pred))
    for(i in 1:nrow(x.train)){
      for(j in 1:n.pred){
        x2 <- 0
        for(k in 1:(dim[j] + 1)){
          x2 <- x2 + coef[j, k] * (x.train[i, j] ^ (k - 1))
        }
        p1[i, j] <- x2
      }
    }
    p1 <- apply(p1, 1, mean, na.rm = TRUE)

    p2 <- data.frame(matrix(rep(NA, nrow(x.test) * n.pred), nrow(x.test), n.pred))
    for(i in 1:nrow(x.test)){
      for(j in 1:n.pred){
        x2 <- 0
        for(k in 1:(dim[j] + 1)){
          x2 <- x2 + coef[j, k] * (x.test[i, j] ^ (k - 1))
        }
        p2[i, j] <- x2
      }
    }
    q.in <- apply(p2, 1, quant)

    mn <- min(y.train)
    mx <- max(y.train)
    dd <- 2
    l <- round(mn - (mx-mn)/2, digits = dd)
    u <- round(mx + (mx-mn)/2, digits = dd)
    range <- ((l * (10^dd)):(u * (10^dd)))/(10^dd)

    p2[p2 <= l | p2 >= u] <- NA
    stack <- array(rep(NA, nrow(x.test) * n.pred * length(range)), c(nrow(x.test), n.pred, length(range)))
    for(j in 1:n.pred){
      stack[, j, ] <- dnorm(rep(range, each = nrow(x.test)), p2[, j], sd[j])
    }
    stack[, apply(x.test, 2, sum) == 0, ] <- NA #missing values
    for(i in 1:nrow(x.test)){
      stack[i, -q.in[[i]], ] <- NA #outliers
    }
    stk <- apply(stack, c(1, 3), mean, na.rm = TRUE)

    p2 <- rep(0, nrow(x.test))
    for (i in 1:nrow(x.test)){
      p2[i] <- range[which.max(stk[i, ])]
    }

    pred.data <- adj(p1, y.train, p2)
    names(pred.data) <- row.names(x.test)
    pred.data
  }



  #sense.gauss
  sense.gauss <- function(x.train, y.train, newx, n.pred = n.pred){

    result <- rep(NA, ncol(x.train))
    for(i in 1:ncol(x.train)){
      result[i] <- summary(lm(y.train ~ poly(x.train[, i], degree = 3, raw = TRUE)))$r.squared
    }

    x.train <- x.train[, order(result, decreasing = TRUE)]
    x.train <- x.train[, 1:n.pred]
    x.test <- newx[, colnames(x.train)]

    mn <- min(y.train)
    mx <- max(y.train)
    dd <- 2 #decimal digits
    l <- round(mn - (mx-mn)/2, digits = dd)
    u <- round(mx + (mx-mn)/2, digits = dd)
    range <- ((l * (10^dd)):(u * (10^dd)))/(10^dd)

    stack1 <- array(rep(NA, nrow(x.train) * n.pred), c(nrow(x.train), n.pred))
    stack2 <- array(rep(NA, nrow(x.test) * n.pred * length(range)),
                    c(nrow(x.test), n.pred, length(range)))
    y.est <- array(rep(NA, nrow(x.test) * n.pred), c(nrow(x.test), n.pred))
    for(j in 1:n.pred){
      invisible(capture.output(fit <- gausspr(x.train[, j], y.train, variance.model = TRUE)))
      stack1[, j] <- predict(fit, x.train[, j])
      y.val <- predict(fit, x.test[, j])
      y.val[y.val <= l | y.val >= u] <- NA
      y.est[, j] <- y.val
      sd.val <- predict(fit, x.test[, j], type = "sdeviation")
      for(i in 1:nrow(x.test)){
        stack2[i, j, ] <- dnorm(range, y.val[i], sd.val[i])
      }
    }
    q.in <- apply(y.est, 1, quant)

    p1 <- apply(stack1, 1, mean)
    stack2[, apply(x.test, 2, sum) == 0, ] <- NA #missing values
    for(i in 1:nrow(x.test)){
      stack2[i, -q.in[[i]], ] <- NA #outliers
    }
    stk <- apply(stack2, c(1, 3), mean, na.rm = TRUE)

    p2 <- rep(0, nrow(x.test))
    for (i in 1:nrow(x.test)){
      p2[i] <- range[which.max(stk[i, ])]
    }

    pred.data <- adj(p1, y.train, p2)
    names(pred.data) <- row.names(x.test)
    pred.data
  }

  #OmicSense function
  if(degree > 0){
    pred.data <- sense.poly(x, y, newx, method = method, n.pred = n.pred, threshold = thresh)
  }else if(degree == 0){
    pred.data <- sense.mix(x, y, newx, n.pred = n.pred, threshold = thresh)
  }else{
    pred.data <- sense.gauss(x, y, newx, n.pred = n.pred)
  }
  pred.data #return
}


