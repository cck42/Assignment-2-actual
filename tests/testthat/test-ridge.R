library(glmnet)
library(MASS)

#test_that("basic regression works",{
#  data(iris)

#  fit_ridge <- ridge(Sepal.Length ~ ., iris)

#  fit_lm <- lm(Sepal.Length  ~ ., iris)

#  expect_equivalent(fit_lm$coefficients, fit_linear_model$coefficients,
                    tolerance = 1e-5)
#})

#the following is mostly taken from CASL 3.2
test_that("works for a made up, colinear-ish set",{
  n <- 20; p<-4; N<- 500; M<-20
  beta <- c(1, -1, 0.5, 0)
  mu <- rep (0,p)
  Sigma <- matrix(0.9, nrow = p, ncol = p)
  diag(Sigma) <-1
  X <- MASS::mvrnorm(n, mu, Sigma)
  y <- X %*% beta + rnorm(n, sd = 5)

  X_test <- MASS::mvrnorm(n,mu, Sigma)
  y_test <- X_test %*% beta + rnorm(n, sd = 5)
  y_test <- as.numeric(y_test)

  fit_ridge <- ridge(X_test, y_test, lambda = lambdas)
  fit_glm <- lm.ridge(X_test, y_test, alpha = 0, lambda = lambdas)

  expect_equivalent(fit_ridge, fit_glm)
})
