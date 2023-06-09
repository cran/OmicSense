% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/os.opt.R
\name{os.opt}
\alias{os.opt}
\title{Estimate the optimal number of predictors to construct OmicSense prediction model}
\usage{
os.opt(x, y, range = 5:50, method = "linear", rep = 1)
}
\arguments{
\item{x}{A data matrix (row: samples, col: predictors).}

\item{y}{A vector of target value.}

\item{range}{A sequence of numbers of predictors to be tested for MAE calculation (default: 5:50).}

\item{method}{A string to specify the regression function for calculating R-squared values.
"linear" (default), "quadratic" or "cubic" function can be specified.}

\item{rep}{The number of replications for each case set by range (default: 1).}
}
\value{
A sample-MAE curve
}
\description{
Estimate the optimal number of predictors to construct OmicSense prediction model
}
\examples{
data(Pinus)
train <- os.clean(Pinus$train)
target <- Pinus$target
os.opt(train[1:10, ], target[1:10], range = 5:15)
}
\author{
Takahiko Koizumi
}
