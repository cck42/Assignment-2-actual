#' Ridge regression function
#'
#' Assignment 2 for BIS 557
#'
#'
#' Goal: compute a matrix of regression values for a numeric data set (X) using ridge regression
#'
#' @param form, an R  formula to construct the regression
#' @param data, the data to use for the regression
#' @param lambda, an optional set of penalties to use for regression
#'
#'

#------------------------------------------------------------------------------------------------------------
#The following returns a matrix consisting of beta values at different lambdas
#If no values are entered for lambda, it will return values for lambdas between 0 and 10 in increments of .1
#------------------------------------------------------------------------------------------------------------



ridge <- function(X, y, lambdas){
  svd_X <- svd(X)
  U <- svd_X$u
  V <- svd_X$v
  svals <- svd_X$d

  k <- length(lambdas)
  ridge <- matrix(nrow = k, ncol = ncol(X))

  ridge <- matrix(nrow = k, ncol = ncol(X))
  for(j in seq_len(k)){
    D <- diag(svals / (svals^2 + lambdas[j]))
    ridge[j,] <- V %*% D %*% t(U) %*% y
  }

  ridge

}
#------------------------------------------------------------------------------------------------------------
#The following function is actually called
#It takes a formula, data set, and optional set of lambdas to check and produces an optimal ridge regression
#It uses 90% of the data for training and the other 10% for cross-validation to determine the best lambda
#-------------------------------------------------------------------------------------------------------------
ridge_regress <- function(form, data, lambda = NULL){
  #make data into model matrix
  X <- model.matrix(form, data,  contrasts.arg = contrasts)
  Y <- matrix(data[,1],ncol = 1)
  #separate into test and training data sets
  test_rows <- sample.int(nrow(X), floor(nrow(X)/10))
  test_set <- X[test_rows,]
  train_rows <- setdiff(seq_len(nrow(X)), test_rows)
  train_set <- X[train_rows,]
  y_train_set <- Y[train_rows]
  y_test_set <- Y[test_rows]

  if(is.null(lambda)){
    lambda <- seq(from = 0, to = 10, by = 0.1)
  }

  regressions <- ridge(train_set, y_train_set, lambdas = lambda)
  y_hat <- tcrossprod(test_set, regressions)
  mse <- apply((y_hat-y_test_set)^2,2,mean)

  best_L <- lambda[which.min(mse)]
  betas <- regressions[(best_L*10+1),]
  #browser()
  #return(list(best_L, regressions[(best_L*10+1),]))
  output <- list(coef = betas, lambda = best_L)
  return(output)
}

