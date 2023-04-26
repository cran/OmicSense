
# OmicSense

<!-- badges: start -->
<!-- badges: end -->

The goal of OmicSense is to provide the functions to construct a prediction model of environments using noisy omics data linked with the environments based on OmicSense algorithm. 

## Installation
You can install the development version of OmicSense from [GitHub](https://github.com/takakoizumi/OmicSense) with:    
#install.packages("devtools")   
devtools::install_github("takakoizumi/OmicSense") 

## Example    
install.packages("OmicSense")   
library(OmicSense)    
# basic example code   
data(Pinus)   
x.train <- Pinus[[1]]   
x.test <- Pinus[[2]]    
y <- Pinus[[3]]   
cor(y, os.pred(x.train, y, newx = x.test, n.pred = 100))
