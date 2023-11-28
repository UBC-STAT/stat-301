library(digest)
library(testthat)

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "Crabs_vs_width_scatterplot"', {
    expect_true(exists("Crabs_vs_width_scatterplot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(Crabs_vs_width_scatterplot))
  })

  properties <- c(Crabs_vs_width_scatterplot$layers[[1]]$mapping, Crabs_vs_width_scatterplot$mapping)

  test_that("Plot should have width on the x-axis", {
    expect_true("width" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(Crabs_vs_width_scatterplot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(Crabs_vs_width_scatterplot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(Crabs_vs_width_scatterplot$data)), "8c2afe893b01f3a8c63e1aef3b5aad9e")
    expect_equal(digest(round(sum(Crabs_vs_width_scatterplot$data$width))), "17a701ffb51e7429bcd57678dd80b402")

    # If width is not known:
    # expect_equal(digest(round(sum(pull(Crabs_vs_width_scatterplot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(Crabs_vs_width_scatterplot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(Crabs_vs_width_scatterplot$labels))
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "Crabs_group_avg_width"', {
    expect_true(exists("Crabs_group_avg_width"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(Crabs_group_avg_width))
  })

  expected_colnames <- c("width_intervals", "mean_n_males")
  given_colnames <- colnames(Crabs_group_avg_width)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(Crabs_group_avg_width))), "71db8a6cad03244e6e50f0ad8bc95a65")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(Crabs_group_avg_width$mean_n_males) * 10e4)), "12273d78c0faff1ea67c8f8e9c42edd0")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "Crabs_avg_width_scatterplot"', {
    expect_true(exists("Crabs_avg_width_scatterplot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(Crabs_avg_width_scatterplot))
  })

  properties <- c(Crabs_avg_width_scatterplot$layers[[1]]$mapping, Crabs_avg_width_scatterplot$mapping)

  test_that("Plot should have width_intervals on the x-axis", {
    expect_true("width_intervals" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(Crabs_avg_width_scatterplot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(Crabs_avg_width_scatterplot$layers[[2]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(Crabs_avg_width_scatterplot$data)), "71db8a6cad03244e6e50f0ad8bc95a65")
    expect_equal(digest(round(sum(Crabs_avg_width_scatterplot$data$mean_n_males))), "e1d9279a9999b2d3bb972ab3267b49c7")

    # If width_intervals is not known:
    # expect_equal(digest(round(sum(pull(Crabs_avg_width_scatterplot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(Crabs_avg_width_scatterplot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(Crabs_avg_width_scatterplot$labels))
  })

  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer1.4, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.4))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "Crabs_vs_width_scatterplot"', {
    expect_true(exists("Crabs_vs_width_scatterplot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(Crabs_vs_width_scatterplot))
  })

  properties <- c(Crabs_vs_width_scatterplot$layers[[1]]$mapping, Crabs_vs_width_scatterplot$mapping)

  test_that("Plot should have width on the x-axis", {
    expect_true("width" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(Crabs_vs_width_scatterplot$layers[[1]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(Crabs_vs_width_scatterplot$data)), "8c2afe893b01f3a8c63e1aef3b5aad9e")
    expect_equal(digest(round(sum(Crabs_vs_width_scatterplot$data$width))), "17a701ffb51e7429bcd57678dd80b402")

    # If width is not known:
    # expect_equal(digest(round(sum(pull(Crabs_vs_width_scatterplot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(Crabs_vs_width_scatterplot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(Crabs_vs_width_scatterplot$labels))
  })

  print("Success!")
}


test_1.6 <- function() {
  test_that('Did not assign answer to an object called "crabs_Poisson_model"', {
    expect_true(exists("crabs_Poisson_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(crabs_Poisson_model))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(crabs_Poisson_model$residuals) * 10e4)), "5ab35ebc157c4f75476569c445d5a1cc")
  })

  print("Success!")
}

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "crabs_Poisson_model_results"', {
    expect_true(exists("crabs_Poisson_model_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(crabs_Poisson_model_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')


  given_colnames <- colnames(crabs_Poisson_model_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(crabs_Poisson_model_results))), "dd4ad37ee474732a009111e3456e7ed7")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$conf.low) * 10e6)), "79cef645627327fe2632a29728949fa8")
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$conf.high) * 10e6)), "b2f64ff96ebb309861a640e306397224")
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$statistic) * 10e6)), "09ad422316ee480535d0735aedfb2543")
  })

  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "crabs_Poisson_model_results"', {
    expect_true(exists("crabs_Poisson_model_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(crabs_Poisson_model_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high', 'exp.estimate', 'exp.conf.low', 'exp.conf.high')

  given_colnames <- colnames(crabs_Poisson_model_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(crabs_Poisson_model_results))), "dd4ad37ee474732a009111e3456e7ed7")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$exp.estimate) * 10e6)), "dd50342cffd6d0cde245787cbdd97021")
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$exp.conf.low) * 10e6)), "5aa9e6879f68968bdb958bdc1a4cdf32")
    expect_equal(digest(as.integer(sum(crabs_Poisson_model_results$exp.conf.high) * 10e6)), "b6866c669fd240e038b16d7e35b90592")
  })

  print("Success!")
}

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "answer1.9"', {
    expect_true(exists("answer1.9"))
  })

  answer_hash <- digest(tolower(answer1.9))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "fd4d64bc84d8d1ac10b94c23bda1a016")
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
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

test_1.11 <- function() {
  test_that('Did not assign answer to an object called "answer1.11"', {
    expect_true(exists("answer1.11"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.11, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.11))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "answer1.12"', {
    expect_true(exists("answer1.12"))
  })

  answer_as_numeric <- as.numeric(answer1.12)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "07660fdc17d69e9b645c10b4a1f810be")
  })

  print("Success!")
}
