# ---
# jupyter:
#   jupytext:
#     formats: r:light
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.12.0
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "model_matrix_X_train"', {
    expect_true(exists("model_matrix_X_train"))
  })

  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(model_matrix_X_train))
  })
  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(matrix_Y_train))
  })

  expected_colnames <- c('mean_radius','mean_texture','mean_perimeter','mean_smoothness','mean_compactness','mean_concavity','mean_concave_points','mean_symmetry','mean_fractal_dimension','radius_error','texture_error','perimeter_error','smoothness_error','compactness_error','symmetry_error','fractal_dimension_error')
  given_colnames <- colnames(model_matrix_X_train)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(model_matrix_X_train))), "e1ccdeeda146ea6a2b9098eac7f58ac2")
  })
  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(matrix_Y_train))), "e1ccdeeda146ea6a2b9098eac7f58ac2")
  })

  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(model_matrix_X_train[,"mean_radius"]) * 10e4)), "da0c890b39f1f7a79777df921f405a41")
  })
  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(matrix_Y_train))), "6ab59a5dc548cdbe65a353f73043f412")
  })
  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_cv_lambda_ridge"', {
    expect_true(exists("breast_cancer_cv_lambda_ridge"))
  })

  test_that("Solution should be a cv.glmnet object", {
    expect_true("cv.glmnet" %in% class(breast_cancer_cv_lambda_ridge))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(breast_cancer_cv_lambda_ridge$index[1,]), "c6df9ff55bfad3fa7254de0d17b5a7f5")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(breast_cancer_cv_lambda_ridge$cvm[97]*10e6)), "58664065b5f1854c8e2a89bc43a79959")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_lambda_max_AUC_ridge"', {
    expect_true(exists("breast_cancer_lambda_max_AUC_ridge"))
  })

  answer_as_numeric <- as.numeric(breast_cancer_lambda_max_AUC_ridge)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "d40f426836915bf80aad44792e069c0b")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_ridge_max_AUC"', {
    expect_true(exists("breast_cancer_ridge_max_AUC"))
  })

  test_that("Solution should be a glmnet object", {
    expect_true("glmnet" %in% class(breast_cancer_ridge_max_AUC))
  })

  test_that("Sultion does not contain the correct number of rows", {
    expect_equal(digest(as.integer(breast_cancer_ridge_max_AUC$lambda*10e3)), "3e58fec15b97b4b65a18dd280f434516")
  })

  test_that("Solution does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_ridge_max_AUC$beta)*10e5)), "e362f8ba11af909bd5dc45d9642efc7a")
  })

  print("Success!")
}

#---------------
#deleted from current version

# test_1.6 <- function() {
#   test_that('Did not assign answer to an object called "breast_cancer_cv_ordinary"', {
#     expect_true(exists("breast_cancer_cv_ordinary"))
#   })

#   test_that("Solution should be a data frame", {
#     expect_true("cv.glmnet" %in% class(breast_cancer_cv_ordinary))
#   })


#   test_that("Solution does not contain the correct number of rows", {
#     expect_equal(digest(as.integer(breast_cancer_cv_ordinary$lambda[2])), "1473d70e5646a26de3c52aa1abd85b1f")
#   })

#   test_that("Solution does not contain the correct data", {
#     expect_equal(digest(as.integer(breast_cancer_cv_ordinary$cvm[2]*10e6)), "685d8a3a85fdc1b00f0cce6597291ea3")
#   })

#   print("Success!")
# }

#-----------

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_AUC_models"', {
    expect_true(exists("breast_cancer_AUC_models"))
  })

# test_that("Solution should be a data frame", {
#    expect_true("data.frame" %in% class(breast_cancer_AUC_models))
#  })

  expected_colnames <- c("model", "auc")
  given_colnames <- colnames(breast_cancer_AUC_models)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer_AUC_models))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_AUC_models$auc) * 10e6)), "5631701a7b5ca282c043fe1af5ce9022")
  })

  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_cv_lambda_LASSO"', {
    expect_true(exists("breast_cancer_cv_lambda_LASSO"))
  })

  test_that("Solution should be a cv.glmnet object", {
    expect_true("cv.glmnet" %in% class(breast_cancer_cv_lambda_LASSO))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(breast_cancer_cv_lambda_LASSO$index[1,]), "cac17b80df37171f02a533a0962e81ec")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(breast_cancer_cv_lambda_LASSO$cvm[97]*10e6)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_lambda_1se_AUC_LASSO"', {
    expect_true(exists("breast_cancer_lambda_1se_AUC_LASSO"))
  })

  answer_as_numeric <- as.numeric(breast_cancer_lambda_1se_AUC_LASSO)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "3a2209228b4a81256404f5ad50412e01")
  })

  print("Success!")
}

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_LASSO_1se_AUC"', {
    expect_true(exists("breast_cancer_LASSO_1se_AUC"))
  })

  test_that("Solution should be a glmnet object", {
    expect_true("glmnet" %in% class(breast_cancer_LASSO_1se_AUC))
  })

  test_that("Sultion does not contain the correct number of rows", {
    expect_equal(digest(as.integer(breast_cancer_LASSO_1se_AUC$lambda*10e3)), "4abb356c7b8460ebf96ff801d6539873")
  })

  test_that("Solution does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_LASSO_1se_AUC$beta)*10e5)), "e8e1e9814f4b16d7df4e4aec6551a55b")
  })

  print("Success!")
}

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "answer1.10"', {
    expect_true(exists("answer1.10"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.10, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.10))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "f960eee34a9ca222e49c0ae4da40d639")
  })

  print("Success!")
}

test_1.11 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_AUC_models"', {
    expect_true(exists("breast_cancer_AUC_models"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer_AUC_models))
  })

  expected_colnames <- c("model", "auc")
  given_colnames <- colnames(breast_cancer_AUC_models)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer_AUC_models))), "11946e7a3ed5e1776e81c0f0ecd383d0")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_AUC_models$auc) * 10e6)), "6ffe4702e283001ffa5f6625e55c30ed")
  })

  print("Success!")
}

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "ROC_lasso"', {
    expect_true(exists("ROC_lasso"))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(ROC_lasso$auc * 10e6)), "5521678a8e26ba545b30889d5438dc16")
  })

  print("Success!")
}
