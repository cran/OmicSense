##' Clean data by eliminating predictors with many missing values
##'
##' @title Clean data by eliminating predictors with many missing values
##' @param x A data matrix (raw: samples, col: predictors).
##' @param missing A ratio of missing values in each column allowed to be remained in the data.
##' @param lowest The lowest value to be leaved in the data.
##' @return A data matrix (raw: samples, col: qualified predictors)
##' @examples
##' data(Pinus)
##' train.raw <- Pinus$train
##' ncol(train.raw)
##'
##' train <- os.clean(train.raw)
##' ncol(train)
##' @author Takahiko Koizumi
##' @export
os.clean <- function(x, missing = 0.1, lowest = 10){
  if(missing < 0 | missing > 1){
    stop("<missing> should be within the range of 0-1")
  }
  if(lowest < 0){
    stop("<lowest> should not be a negative value")
  }else if(lowest > ncol(x)){
    stop(paste("<lowest> must not exceed", max(x, na.rm = TRUE), sep = " "))
  }

  r <- function(z) length(z[z != 0]) / length(z)
  ## handle predictors with missing values
  x <- x[, apply(x, 2, r) >= (1 - missing)]
  ## handle predictors with low expression
  x <- x[, apply(x, 2, max) >= lowest]
  return(x)
}
