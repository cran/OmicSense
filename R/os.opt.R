##' Estimate the optimal number of predictors to construct OmicSense prediction model
##'
##' @title Estimate the optimal number of predictors to construct OmicSense prediction model
##' @param x A data matrix (row: samples, col: predictors).
##' @param y A vector of target value.
##' @param range A sequence of numbers of predictors to be tested for MAE calculation (default: 5:50).
##' @param method A string to specify the regression function for calculating R-squared values.
##' "linear" (default), "quadratic" or "cubic" function can be specified.
##' @param rep The number of replications for each case set by range (default: 1).
##' @importFrom ggplot2 ggplot aes geom_point stat_function geom_hline geom_text theme element_rect element_blank
##' @importFrom stats rnorm nls
##' @return A sample-MAE curve
##' @examples
##' data(Pinus)
##' train <- os.clean(Pinus$train)
##' target <- Pinus$target
##' os.opt(train[1:10, ], target[1:10], range = 5:15)
##' @author Takahiko Koizumi
##' @export
os.opt <- function(x, y, range = 5:50, method = "linear", rep = 1) {
  npred <- MAE <- NULL

  degree <- switch(method,
                   "linear" = 1,
                   "quadratic" = 2,
                   "cubic" = 3,
                   stop("Select the methods from linear, quadratic, or cubic")
  )

  if(min(range) < 0){
    stop("<range> must not include a negative value")
  }else if(max(range) > ncol(x)){
    stop(paste("<range> must not exceed", ncol(x), sep = " "))
  }
  if(rep <= 0){
    stop("<rep> must be a natural number")
  }

  g <- range
  unit <- rep(NA, length(g) * rep)
  comp <- data.frame(
    npred = unit,
    MAE = unit
  )

  ## sort predictors in descending order of R2
  x.ps <- os.sort(x, y, method = method)
  x.ps <- x.ps[, 1:max(g)]
  message("Data prepared")

  ## calculate MAE
  total <- length(range) * rep
  perc <- trunc(10 * c(1:total) / total)

  counter <- 1
  proc <- perc[1]
  for(i in 1:length(g)){
    for(j in 1:rep){
      x.p <- x.ps[, 1:g[i]]
      x.p.test <- x.p
      ## add noise
      for(k in 1:g[i]){
        p <- x.p[, k]
        x.p.test[, k] <- rnorm(length(p), p, p/2)
      }

      ## run OmicSense
      pc <- os.pred(x.p, y, newx = x.p.test, method = method)
      comp[counter, ] <- c(g[i], mean(abs(y - pc)))

      if(proc != perc[counter]){
        message(paste((proc + 1) * 10, "% completed", sep = ""))
        proc <- perc[counter]
      }
      counter <- counter + 1
    }
  }

  ## estimate the minimum MAE
  fit <- nls(MAE ~ a + b * exp(- c * npred), start = c (a = 1, b = 1, c = 0.1), data = comp)
  a <- fit$m$getPars()[[1]]
  b <- fit$m$getPars()[[2]]
  c <- fit$m$getPars()[[3]]

  ggplot(comp, aes(x = npred, y = MAE)) +
    geom_point(aes(colour = "MAE", fill = "MAE"), shape = 21) +
    stat_function(fun = function(x) a + b * exp(- c * x)) +
    geom_hline(aes(yintercept = a), colour = "red", linetype = "dotted") +
    geom_text(aes(0, a, label = format(round(a, 3), nsmall = 3), vjust = - 1)) +
    xlab("Number of variables") +
    theme(panel.background = element_rect(fill = "transparent", colour = "black"),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.position = "none")
}



