library(digest)
library(testthat)

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "species_elevation_plot"', {
    expect_true(exists("species_elevation_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(species_elevation_plot))
  })

  properties <- c(species_elevation_plot$layers[[1]]$mapping, species_elevation_plot$mapping)

  test_that("Plot should have width on the x-axis", {
    expect_true("elevation" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(species_elevation_plot$layers[[1]]$geom))

  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(species_elevation_plot$data)), "7d2842cab7725fd8f382293e410d42b2")
    expect_equal(digest(round(sum(species_elevation_plot$data$elevation))), "76a7984ab96ad5a3939c1961ddb57c79")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(species_elevation_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(species_elevation_plot$labels))
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be a single character ("A", "B", or "C")', {
    expect_match(answer1.2, "a|b|c", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "species_elevation_plot"', {
    expect_true(exists("species_elevation_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(species_elevation_plot))
  })

  properties <- c(species_elevation_plot$layers[[1]]$mapping, species_elevation_plot$mapping)

  test_that("Plot should have width on the x-axis", {
    expect_true("elevation" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(species_elevation_plot$layers[[1]]$geom))
  })


  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(species_elevation_plot$data)), "7d2842cab7725fd8f382293e410d42b2")
    expect_equal(digest(round(sum(species_elevation_plot$data$elevation))), "76a7984ab96ad5a3939c1961ddb57c79")

  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(species_elevation_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(species_elevation_plot$labels))
  })

  print("Success!")
}


test_1.4 <- function() {
  test_that('Did not assign answer to an object called "species_elevation_model"', {
    expect_true(exists("species_elevation_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(species_elevation_model))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_elevation_model$residuals) * 10e4)), "11e5f205b72c3e6b094d1dd28d5727df")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "species_elevation_model_results"', {
    expect_true(exists("species_elevation_model_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(species_elevation_model_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')


  given_colnames <- colnames(species_elevation_model_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(species_elevation_model_results))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_elevation_model_results$conf.low) * 10e6)), "310e1f2205650978751088ab3169f112")
    expect_equal(digest(as.integer(sum(species_elevation_model_results$conf.high) * 10e6)), "d923bccb00ae6a09fc6fa9ec9dbc2efa")
    expect_equal(digest(as.integer(sum(species_elevation_model_results$statistic) * 10e6)), "4da94290b770f99b2202c3d3e6f2e0b6")
  })

  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "species_elevation_exp_results"', {
    expect_true(exists("species_elevation_exp_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(species_elevation_exp_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')

  given_colnames <- colnames(species_elevation_exp_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(species_elevation_exp_results))), "c01f179e4b57ab8bd9de309e6d576c48")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_elevation_exp_results$estimate) * 10e6)), "2f57c4303d74b14ecb9449886776b395")
    expect_equal(digest(as.integer(sum(species_elevation_exp_results$conf.low) * 10e6)), "d195a6a4053c85c4e3d96d0d51d8af93")
    expect_equal(digest(as.integer(sum(species_elevation_exp_results$conf.high) * 10e6)), "28deebacbc2f5a95d446f0b9a811b1e2")
  })

  print("Success!")
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "answer1.8"', {
    expect_true(exists("answer1.8"))
  })

  answer_hash <- digest(tolower(answer1.8))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

test_1.9 <- function() {
  test_that('Did not assign answer to an object called "species_poisson_model"', {
    expect_true(exists("species_poisson_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(species_poisson_model))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_poisson_model$residuals) * 10e4)), "8dc3a4c25f1c75b2c7f339ee63bdfb18")
  })

  print("Success!")
}

test_1.10 <- function() {
  test_that('Did not assign answer to an object called "species_poisson_exp_results"', {
    expect_true(exists("species_poisson_exp_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(species_poisson_exp_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')

  given_colnames <- colnames(species_poisson_exp_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(species_poisson_exp_results))), "25e6a154090e35101d7678d6f034353a")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_poisson_exp_results$estimate) * 10e6)), "1e64c39ee96ad3fcda4675b0d579bec6")
    expect_equal(digest(as.integer(sum(species_poisson_exp_results$conf.low) * 10e6)), "08192881dd631b1ddcaa9c2bd4b0387a")
    expect_equal(digest(as.integer(sum(species_poisson_exp_results$conf.high) * 10e6)), "80f7aa182fc144e2dd8fba5e927a1dcc")
  })

  print("Success!")
}

test_1.12 <- function() {
  test_that('Did not assign answer to an object called "answer1.12"', {
    expect_true(exists("answer1.12"))
  })


  answer_hash <- digest(tolower(answer1.12))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "0aa9c59ea893e51a8cc55e8ea353e592")
  })

  print("Success!")
}

test_1.13 <- function() {
  test_that('Did not assign answer to an object called "species_poisson_residuals"', {
    expect_true(exists("species_poisson_residuals"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(species_poisson_residuals))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(species_poisson_residuals))), "7d2842cab7725fd8f382293e410d42b2")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(species_poisson_residuals$resid_raw)* 10e6)), "1473d70e5646a26de3c52aa1abd85b1f")
    expect_equal(digest(as.integer(sum(species_poisson_residuals$resid_pearson)* 10e6)), "8ebc60c26d2b01347effcbfff1586be1")
    expect_equal(digest(as.integer(sum(species_poisson_residuals$pearson_byhand)* 10e6)), "8ebc60c26d2b01347effcbfff1586be1")
  })

  print("Success!")
}


test_1.14 <- function() {
  test_that('Did not assign answer to an object called "answer1.14"', {
    expect_true(exists("answer1.14"))
  })


  answer_hash <- digest(tolower(answer1.14))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

test_1.15 <- function() {
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
