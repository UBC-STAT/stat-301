library(digest)
library(testthat)

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "default.df"', {
    expect_true(exists("default.df"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(default.df))
  })

  expected_colnames <- c("default", "student", "balance", "income")
  given_colnames <- colnames(default.df)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(default.df))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default.df$balance))), "fe77e9bf661be6a426411dc60fe6b6cd")
  })

  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "Default_SLR_plot"', {
    expect_true(exists("Default_SLR_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(Default_SLR_plot))
  })

  properties <- c(Default_SLR_plot$layers[[1]]$mapping, Default_SLR_plot$mapping)

  test_that("Plot should have balance on the x-axis", {
    expect_true("balance" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(Default_SLR_plot$layers[[1]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(Default_SLR_plot$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(Default_SLR_plot$data$balance))), "3267136924eba9966e7fa0114b916d95")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(Default_SLR_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(Default_SLR_plot$labels))
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "logistic_curve"', {
    expect_true(exists("logistic_curve"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(logistic_curve))
  })

  properties <- c(logistic_curve$layers[[1]]$mapping, logistic_curve$mapping)

  test_that("Plot should have z on the x-axis", {
    expect_true("z" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(logistic_curve$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(logistic_curve$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(logistic_curve$data)), "cada6c6b62b103dca67f23bd7d60ac3c")
    expect_equal(digest(round(sum(logistic_curve$data$z))), "908d1fd10b357ed0ceaaec823abf81bc")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(logistic_curve$labels))
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  answer_as_numeric <- as.numeric(answer1.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "9ce8f1f78c56eccda919dc6404297331")
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "Default_SLR_plot"', {
    expect_true(exists("Default_SLR_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(Default_SLR_plot))
  })

  properties <- c(Default_SLR_plot$layers[[1]]$mapping, Default_SLR_plot$mapping)

  test_that("Plot should have balance on the x-axis", {
    expect_true("balance" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(Default_SLR_plot$layers[[1]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(Default_SLR_plot$data)), "f1a30baa3072c1aad822c059f35c6841")
    expect_equal(digest(round(sum(Default_SLR_plot$data$balance))), "3267136924eba9966e7fa0114b916d95")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(Default_SLR_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(Default_SLR_plot$labels))
  })

  print("Success!")
}

#-----
test_1.5 <- function() {
  test_that('Did not assign answer to an object called "Default_binary_log_student"', {
    expect_true(exists("Default_binary_log_student"))
  })
  
  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(Default_binary_log_student))
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(Default_binary_log_student$residuals) * 10e4)), 
                 "1473d70e5646a26de3c52aa1abd85b1f")
  })
  
  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "answer1.6"', {
    expect_true(exists("answer1.6"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.6, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.6))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}


test_1.7 <- function() {
  test_that('Did not assign answer to an object called "Default_binary_log_model"', {
    expect_true(exists("Default_binary_log_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(Default_binary_log_model))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(Default_binary_log_model$residuals) * 10e4)), "4cd0d70c6d524273008afa4703cd9df0")
  })

  print("Success!")
}


test_1.8 <- function() {
  test_that('Did not assign answer to an object called "answer1.8"', {
    expect_true(exists("answer1.8"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.8, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.8))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  
  print("Success!")
}

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "answer1.9"', {
    expect_true(exists("answer1.9"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.9, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.9))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "Default_binary_log_model_results"', {
    expect_true(exists("Default_binary_log_model_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(Default_binary_log_model_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')


  given_colnames <- colnames(Default_binary_log_model_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(Default_binary_log_model_results))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(Default_binary_log_model_results$conf.low) * 10e6)), "ba24456096c2af163742607b3e494ef7")
    expect_equal(digest(as.integer(sum(Default_binary_log_model_results$conf.high) * 10e6)), "f2c22cf099c18a563f722da22c975d80")
    expect_equal(digest(as.integer(sum(Default_binary_log_model_results$statistic) * 10e6)), "273c6d0ba411836667be62df7641a7b4")
  })

  print("Success!")
}

test_1.11 <- function() {
  test_that('Did not assign answer to an object called "Default_binary_odds_model_results"', {
    expect_true(exists("Default_binary_odds_model_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(Default_binary_odds_model_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')

  given_colnames <- colnames(Default_binary_odds_model_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(Default_binary_odds_model_results))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(Default_binary_odds_model_results$estimate) * 10e6)), "7a4bb9d19684e603bffc6f1961a7eda1")
    expect_equal(digest(as.integer(sum(Default_binary_odds_model_results$conf.low) * 10e6)), "55679ab470e07149e5a38ce2213a15df")
    expect_equal(digest(as.integer(sum(Default_binary_odds_model_results$conf.high) * 10e6)), "bdccf6329a8314620c8305cc31634021")
  })

  print("Success!")
}

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "answer1.12"', {
    expect_true(exists("answer1.12"))
  })

  answer_hash <- digest(tolower(answer1.12))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "8b63e49106226d2a45958a7e24c97c37")
  })

  print("Success!")
}


test_1.13 <- function() {
  test_that('Did not assign answer to an object called "answer1.13"', {
    expect_true(exists("answer1.13"))
  })

  answer_as_numeric <- as.numeric(answer1.13)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "7ea6059985f925469f09bb3af79c7dc1")
  })

  print("Success!")
}

test_1.14 <- function() {
  test_that('Did not assign answer to an object called "answer1.14"', {
    expect_true(exists("answer1.14"))
  })

  answer_as_numeric <- as.numeric(answer1.14)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "26b7f981cf15e1b5f55fa6a141d8a3a0")
  })

  print("Success!")
}
