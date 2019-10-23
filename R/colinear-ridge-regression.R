# Colinear ridge regression
#
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(readr)
library(dplyr)

#----------------------------------------------------------------------
# The following will perform a ridge regression
#----------------------------------------------------------------------

easy_ridge <- function(form, data, lambda = 0){
  #Build a model matrix
  X <- model.matrix(form, data)
  #Build the corresponding y, removing anything with NAs
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]

  #find the rank and take QR decomposition in order to determine colinearity
  qr_data <- qr(X)
  rank <- qr_data$rank

  #Take singular value decomposition
  x_svd <- svd(X)


  if(rank==ncol(X)){
    #solve directly
    r <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
    ret <- list(coefficients = r)
  }else{
    #QR decomposition
    Q <- qr.Q(qr_data)
  #  R <- qr.R(qr_data)[,1:rank]
  #  QT <- crossprod(Q,Y)
  #  r  <- backsolve(R,QT)
  #  ret <- list(coefficients = c(r,NA))
  #}

  return(r)
}

#---------------------------------------------------------
#the following will optimize the ridge parameter, lambda
#---------------------------------------------------------

lambda_finder <- function(form, data){
  #first, make a small test and larger training data set
  test_rows <- sample.int(nrow(data), nrow(data)/10)
  test_set <- iris[test_rows,]
  print(test_rows)
  train_rows <- setdiff(nrow(data), test_rows)
  train_set <- iris[train_rows,]
  print(train_rows)
  f <- formula(form)
  #choose a set of lambdas to test
  p <- seq(from = 0, to = 10, by = 0.1)
  browser()
  #for(lambda in p){
  #  test_coeffs <- easy_ridge(test, f, lambda = lambda)
  #  train_coeffs <- easy_ridge(train, f, lambda = lambda)
  #  print(test_coeffs)
  #}
}
}
