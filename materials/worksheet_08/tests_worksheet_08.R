library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer"', {
    expect_true(exists("breast_cancer"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer))
  })

  expected_colnames <- c('ID','mean_radius','mean_texture','mean_perimeter','mean_smoothness','mean_compactness','mean_concavity','mean_concave_points','mean_symmetry','mean_fractal_dimension','radius_error','texture_error','perimeter_error','smoothness_error','compactness_error','symmetry_error','fractal_dimension_error','target')
  given_colnames <- colnames(breast_cancer)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer))), "19074fefec837da04af22bb345a7a8cb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer$target))), "9487172f214b56a933ed69dff6a5e934")
  })

  print("Success!")
}

test_1.1_partI <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_train"', {
    expect_true(exists("breast_cancer_train"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer_train))
  })

  expected_colnames <- c('ID','mean_radius','mean_texture','mean_perimeter','mean_smoothness','mean_compactness','mean_concavity','mean_concave_points','mean_symmetry','mean_fractal_dimension','radius_error','texture_error','perimeter_error','smoothness_error','compactness_error','symmetry_error','fractal_dimension_error','target')
  given_colnames <- colnames(breast_cancer_train)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer_train))), "e1ccdeeda146ea6a2b9098eac7f58ac2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_train$target))), "6ab59a5dc548cdbe65a353f73043f412")
  })

  print("Success!")
}

test_1.1_partII <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_test"', {
    expect_true(exists("breast_cancer_test"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer_test))
  })

  expected_colnames <- c('ID','mean_radius','mean_texture','mean_perimeter','mean_smoothness','mean_compactness','mean_concavity','mean_concave_points','mean_symmetry','mean_fractal_dimension','radius_error','texture_error','perimeter_error','smoothness_error','compactness_error','symmetry_error','fractal_dimension_error','target')
  given_colnames <- colnames(breast_cancer_test)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer_test))), "830e946fdd8037512e29e4c2fe7ca93f")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_test$target))), "e15efc0db45ebdee4bebbca843987014")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_logistic_model"', {
    expect_true(exists("breast_cancer_logistic_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(breast_cancer_logistic_model))
  })

  test_that("glm does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_logistic_model$residuals) * 10e4)), "da183a051133e37770ad36364eee5b12")
    expect_equal(digest(as.integer(sum(breast_cancer_logistic_model$coefficients) * 10e4)), "90febdf4a553fd9c6de9b48a2a19cd21")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "error_rate_train"', {
    expect_true(exists("error_rate_train"))
  })

  answer_as_numeric <- as.numeric(error_rate_train)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "f51a3aeedc75bef6e1b38c004d292995")
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "cv_logistic$delta[1]"', {
    expect_true(exists("cv_logistic"))
  })

  answer_as_numeric <- as.numeric(cv_logistic$delta[1])
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "b4ca80995940aeb7627eba0cfd9f20fe")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.5, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.5))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
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
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "DATAFRAME_NAME"', {
    expect_true(exists("breast_cancer_pred_class"))
  })

  test_that("Solution should be an array", {
    expect_true("numeric" %in% class(breast_cancer_pred_class))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(breast_cancer_pred_class)), "a87fde528b14e12c62d84fd70a657e5f")
  })

  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_confusion_matrix"', {
    expect_true(exists("breast_cancer_confusion_matrix"))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_confusion_matrix$overall) * 10e6)), "0ba9b49cc7afdb11352636d2edc2b1ef")
  })

  print("Success!")
}

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "answer1.9"', {
    expect_true(exists("answer1.9"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.9, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.9))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "confusion_matrix_threshold_0.3"', {
    expect_true(exists("confusion_matrix_threshold_0.3"))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(confusion_matrix_threshold_0.3$overall) * 10e6)), "360110f35b0505d2f4434c8e223817bc")
  })

  print("Success!")
}

test_1.11 <- function() {
  test_that('Did not assign answer to an object called "ROC_full_log"', {
    expect_true(exists("ROC_full_log"))
  })

  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(ROC_full_log$auc * 10e6)), "495e49bf2c96f840d4b6615086e14743")
  })

  print("Success!")
}
