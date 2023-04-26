##' Visualize R-squared value distribution in target-predictor relationship
##'
##' @title Visualize R-squared value distribution in target-predictor relationship
##' @param x A data matrix (row: samples, col: predictors).
##' @param y A vector of target value.
##' @param method A string to specify the regression function for calculating R-squared values.
##' "linear" (default), "quadratic" or "cubic" function can be specified.
##' @param n.pred The number of predictors to be leaved in prediction model (default: ncol(x)).
##' @param thresh The lower threshold of R-squared value to be leaved in prediction model (default: 0).
##' @param upper.xlim The upper limitation of x axis (i.e., the number of predictors) in the resulted figure (default: ncol(x)).
##' @importFrom ggplot2 ggplot aes geom_line geom_area xlim ylim xlab ylab
##' geom_hline geom_text geom_vline theme element_rect element_blank
##' @importFrom stats lm
##' @return A rank order plot
##' @examples
##' data(Pinus)
##' train <- os.clean(Pinus$train)
##' target <- Pinus$target
##' train <- os.sort(train, target)
##' os.rank(train, target)
##' @author Takahiko Koizumi
##' @export
os.rank <- function(x, y, method = "linear", thresh = 0, n.pred = ncol(x), upper.xlim = ncol(x)){
  R2 <- NULL
  degree <- switch(method,
                   "linear" = 1,
                   "quadratic" = 2,
                   "cubic" = 3,
                   stop("Select the <method> linear, quadratic, or cubic")
  )

  if(thresh > 0 & n.pred != ncol(x)){
    stop("Don't specify <n.pred> and <thresh> at a time")
  }
  if(thresh < 0 | thresh > 1){
    stop("<thresh> should be within the range of 0-1")
  }
  if(n.pred < 0){
    stop("<n.pred> should not be a negative value")
  }else if(n.pred > ncol(x)){
    stop(paste("<n.pred> must not exceed", ncol(x), sep = " "))
  }


  ## calculate R2 values
  result <- rep(NA, ncol(x))
  for(i in 1:ncol(x)){
    result[i] <- summary(lm(y ~ poly(x[, i], degree = degree, raw = TRUE)))$r.squared
  }

  h <- data.frame(
    rank = rep(NA, ncol(x)),
    R2 = result
  )

  ## sort predictors in descending order of R2
  h <- h[order(h$R2, decreasing = TRUE), ]
  h$rank <- 1:nrow(h)

  ## set the number of predictors to be used
  if(thresh != 0){
    n.pred <- length(result[result >= thresh])
  }

  if(thresh > 0){
    rankplot <- ggplot() +
      geom_line(data = h, aes(x = rank, y = R2), colour = "black") +
      geom_area(data = h, aes(x = rank, y = R2), fill = "gray") +
      geom_area(data = h[1:n.pred, ], aes(x = rank, y = R2), fill = "red") +
      xlim(0, upper.xlim) +
      ylim(0, 1) +
      xlab("Rank") +
      ylab("R-squared value") +
      geom_hline(aes(yintercept = thresh), colour = "black", linetype = "dotted") +
      geom_text(aes(0, thresh, label = format(round(thresh, 2), nsmall = 2), hjust = -0.2, vjust = 1.5)) +
      geom_vline(aes(xintercept = n.pred), colour = "black", linetype = "dotted") +
      geom_text(aes(n.pred, 0.03, label = n.pred, hjust = -0.5)) +
      theme(panel.background = element_rect(fill = "transparent", colour = "black"),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.position = "none")
  }else{
    R2val <- h[n.pred, "R2"]
    rankplot <- ggplot() +
      geom_line(data = h, aes(x = rank, y = R2), colour = "black") +
      geom_area(data = h, aes(x = rank, y = R2), fill = "gray") +
      geom_area(data = h[1:n.pred, ], aes(x = rank, y = R2), fill = "red") +
      xlim(0, upper.xlim) +
      ylim(0, 1) +
      xlab("Rank") +
      ylab("R-squared value") +
      geom_hline(aes(yintercept = R2val), colour = "black", linetype = "dotted") +
      geom_text(aes(0, R2val, label = format(round(R2val, 2), nsmall = 2), hjust = -0.2, vjust = 1.5)) +
      geom_vline(aes(xintercept = n.pred), colour = "black", linetype = "dotted") +
      geom_text(aes(n.pred, 0.03, label = n.pred, hjust = -0.5)) +
      theme(panel.background = element_rect(fill = "transparent", colour = "black"),
            panel.grid = element_blank(),
            strip.background = element_blank(),
            legend.position = "none")
  }
  suppressWarnings(rankplot)
}



