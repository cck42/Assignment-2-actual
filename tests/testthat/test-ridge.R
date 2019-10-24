library(glmnet)
library(Matrix)
library(MASS)
library(testthat)
library(devtools)
library(knitr)
library(roxygen2)

context("Test the output of a ridge regression function")

test_that("Works in easy case",{
  data(mtcars)

  my_fit <- ridge_regress(mpg ~ ., mtcars, lambda = 0)

  fit_lm <- lm.ridge(mpg  ~ ., mtcars)
  browser()
  expect_equivalent(my_fit$coef[2:length(my_fit$coef)],fit_lm$coef, tolerance = .5)
})
