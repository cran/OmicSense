% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os.sort.R
\name{os.sort}
\alias{os.sort}
\title{Sort and select predictors according to the strength of target-predictor relationship}
\usage{
os.sort(x, y, method = "linear", n.pred = ncol(x), thresh = 1)
}
\arguments{
\item{x}{A data matrix (raw: samples, col: predictors).}

\item{y}{A vector of target value.}

\item{method}{A string to specify the regression function for calculating R-squared values.
"linear" (default), "quadratic" or "cubic" function can be specified.}

\item{n.pred}{The number of predictors to be leaved in prediction model (default: ncol(x)).}

\item{thresh}{The lower threshold of R-squared value to be leaved in prediction model (default: 1).}
}
\value{
A data matrix (raw: samples, col: sorted predictors)
}
\description{
Sort and select predictors according to the strength of target-predictor relationship
}
\examples{
data(Pinus)
train <- os.clean(Pinus$train)
target <- Pinus$target
cor(target, train[, 1])

train <- os.sort(train, target, thresh = 0.5)
cor(target, train[, 1])
}
\author{
Takahiko Koizumi
}
