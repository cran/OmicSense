##' Sort and select predictors according to the strength of target-predictor relationship
##'
##' @title Sort and select predictors according to the strength of target-predictor relationship
##' @param x A data matrix (raw: samples, col: predictors).
##' @param y A vector of target value.
##' @param method A string to specify the regression function for calculating R-squared values.
##' "linear" (default), "quadratic" or "cubic" function can be specified.
##' @param n.pred The number of predictors to be leaved in prediction model (default: ncol(x)).
##' @param thresh The lower threshold of R-squared value to be leaved in prediction model (default: 1).
##' @importFrom stats lm
##' @return A data matrix (raw: samples, col: sorted predictors)
##' @examples
##' data(Pinus)
##' train <- os.clean(Pinus$train)
##' target <- Pinus$target
##' cor(target, train[, 1])
##'
##' train <- os.sort(train, target, thresh = 0.5)
##' cor(target, train[, 1])
##' @author Takahiko Koizumi
##' @export
os.sort <- function(x, y, method = "linear", n.pred = ncol(x), thresh = 1){
  degree <- switch(method,
                   "linear" = 1,
                   "quadratic" = 2,
                   "cubic" = 3,
                   stop("Select the <method> linear, quadratic, or cubic")
  )

  if(n.pred < ncol(x) & thresh < 1){
    stop("Don't specify <n.pred> and <thresh> at a time")
  }
  if(n.pred < 0){
    stop("<n.pred> should not be a negative value")
  }else if(n.pred > ncol(x)){
    stop(paste("<n.pred> must not exceed", ncol(x), sep = " "))
  }
  if(thresh < 0 | thresh > 1){
    stop("<thresh> should be within the range of 0-1")
  }

  ## calculate R2 values
  result <- rep(NA, ncol(x))
  for(i in 1:ncol(x)){
    result[i] <- summary(lm(y ~ poly(x[, i], degree = degree, raw = TRUE)))$r.squared
  }
  ## sort predictors in descending order of R2
  x <- x[, order(result, decreasing = TRUE)]

  ## extract predictors with higher R2
  if(n.pred <= ncol(x)){
    x <- x[, 1:n.pred]
  }else if(thresh < 1){
    x <- x[, 1:length(result[result >= thresh])]
  }else{
    x <- x
  }
  return(x)
}
