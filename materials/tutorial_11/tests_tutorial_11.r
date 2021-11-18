# ---
# jupyter:
#   jupytext:
#     formats: r:light
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.11.4
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(testthat)
library(digest)

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "housing_inference"', {
    expect_true(exists("housing_inference"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(housing_inference))
  })

  expected_colnames <- c('Id','LotFrontage','LotArea','MasVnrArea','TotalBsmtSF','GrLivArea','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','Fireplaces','GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ScreenPorch','PoolArea','ageSold','SalePrice')
  given_colnames <- colnames(housing_inference)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(housing_inference))), "7f46bf5c34b2dd47e708eff9d637f492")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(housing_inference$Fireplaces))), "953f677c38770987a07207c47a6a9d2f")
  })
    
  test_that('Did not assign answer to an object called "housing_selection"', {
    expect_true(exists("housing_selection"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(housing_selection))
  })

  expected_colnames <- c('Id','LotFrontage','LotArea','MasVnrArea','TotalBsmtSF','GrLivArea','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','Fireplaces','GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ScreenPorch','PoolArea','ageSold','SalePrice')
  given_colnames <- colnames(housing_selection)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(housing_selection))), "3cc274d9c6bbba07d856a39a3b974042")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(housing_selection$Fireplaces))), "3bb12916e7f6fda4645dd4ecaedb76b9")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "lasso_model"', {
    expect_true(exists("lasso_model"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_model$lambda) * 100)), "804a6401dd764159b52bf7a65c63fca6")
  })
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(length(lasso_model$lambda)), "70907d9a2fe63b90036adda611231960")
  })
    
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_model$lambda.min) * 1000)), "e370841e14d49a0cea811beb047bafce")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "beta_lasso"', {
    expect_true(exists("beta_lasso"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(beta_lasso) * 1000)), "1fa086ea48169a5a5f992192e172ba7c")
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "lasso_selected_covariates"', {
    expect_true(exists("lasso_selected_covariates"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(lasso_selected_covariates), "9298a039f9cb22b8d0c095ca8233b7a0")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "lasso_variables_vif"', {
    expect_true(exists("lasso_variables_vif"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_variables_vif) * 100000)), "108915f1e83d4223eb48376b5dc6d6fa")
  })

  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "answer1.6"', {
    expect_true(exists("answer1.6"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.6, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "inference_model"', {
    expect_true(exists("inference_model"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(inference_model$coefficients)*10000)), "03d259f177482467ab8468091d623ef2")
    expect_equal(digest(as.integer(sum(inference_model$df.residual))), "55269b4e7d9278de60ddfbf412dffc93")
  })

  print("Success!")
}
