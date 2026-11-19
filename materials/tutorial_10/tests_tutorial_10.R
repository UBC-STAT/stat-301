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
    expect_equal(digest(as.integer(nrow(model_matrix_X_train))), "ef4f10b68e5756cc88f2982bacb039d3")
  })
  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(matrix_Y_train))), "ef4f10b68e5756cc88f2982bacb039d3")
  })

  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(model_matrix_X_train[,"mean_radius"]) * 10e4)), "cab3eff6a9ccea7bc083064ad9a94f06")
  })
  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(matrix_Y_train))), "d541e22dc903b2f1eb25cbd14ff722ee")
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
    expect_equal(digest(breast_cancer_cv_lambda_ridge$index[1,]), "8636eeb2cc941c94ef05c8b79cde15fa")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(breast_cancer_cv_lambda_ridge$cvm[breast_cancer_cv_lambda_ridge$index["min", 1]]*10e6)), "a737438b1236ae027916f3a5b91eb95f")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "lambda_max_AUC_ridge"', {
    expect_true(exists("lambda_max_AUC_ridge"))
  })

  answer_as_numeric <- as.numeric(lambda_max_AUC_ridge)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "f6d8b4925f25baab45b9235fc7d5027e")
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
    expect_equal(digest(as.integer(breast_cancer_ridge_max_AUC$lambda*10e3)), "35d85f177f4c2c3ec5d930fb563f02c9")
  })

  test_that("Solution does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_ridge_max_AUC$beta)*10e5)), "aa79031c4c7bfd338a4f8cf42bc66208")
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
    expect_equal(digest(as.integer(nrow(breast_cancer_AUC_models[1:2, , drop = FALSE]))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_AUC_models$auc[1:2]) * 10e6)), "739c877e5def74f3a603886ea4f0c6d7")
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
    expect_equal(digest(breast_cancer_cv_lambda_LASSO$index[1,]), "16071ab8270571c6c83d682892e00ea5")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(breast_cancer_cv_lambda_LASSO$cvm[breast_cancer_cv_lambda_LASSO$index["1se", 1]]*10e6)), "abf05cba46b5ecbfa6790bf3f3ae92ef")
  })

  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "lambda_1se_AUC_LASSO"', {
    expect_true(exists("lambda_1se_AUC_LASSO"))
  })

  answer_as_numeric <- as.numeric(lambda_1se_AUC_LASSO)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "b18136e322a1c8dc4e733ebad231fca9")
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
    expect_equal(digest(as.integer(breast_cancer_LASSO_1se_AUC$lambda*10e3)), "7a384bdc8d30cf813e78d9d64947d5c9")
  })

  test_that("Solution does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_LASSO_1se_AUC$beta)*10e5)), "533956b037380e02cf3c45ac19d7dd51")
  })

  print("Success!")
}

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "answer1.10"', {
    expect_true(exists("answer1.10"))
  })

  test_that('Solution should be an alphabetically ordered string of letters A-P', {
    expect_match(answer1.10, "^[A-P]+$", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.10))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "72d65d774e8e682d6cb23b551a2c9d36")
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
    expect_equal(digest(as.integer(sum(breast_cancer_AUC_models$auc) * 10e6)), "9c20e1b6c5d474ef862382598c3ccf5d")
  })

  print("Success!")
}

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "ROC_lasso"', {
    expect_true(exists("ROC_lasso"))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(ROC_lasso$auc * 10e6)), "20c5422f644b782b31dfb4c9506fdbb0")
  })

  print("Success!")
}
