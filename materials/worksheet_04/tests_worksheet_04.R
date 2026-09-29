library(digest)
library(testthat)

check_MC <- function(answerX.X, choiceList, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })

  test_that(paste('Solution should be a single character ', toString(choiceList)), {
    expect_true(tolower(answerX.X) %in% tolower(choiceList))
  })

  answer_hash <- digest(tolower(answerX.X))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, expectedHash)
  })

  print("Success!")
}

getPermutations <- function(vec) {
    rsf <- c()
    for (i in 1:length(vec)) {
        for (j in i:length(vec)) {
            temp <- vec[i:j] 
            rsf <- c(rsf, paste(temp, collapse= ''))
            if (i < j) {
                for (k in i:j) {
                    rsf <- c(rsf, paste(temp[-k], collapse=''))
                }
            }
        }
    }
    return(unique(rsf))
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "airbnb_slr_plot"', {
    expect_true(exists("airbnb_slr_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(airbnb_slr_plot))
  })

  properties <- c(airbnb_slr_plot$layers[[1]]$mapping, airbnb_slr_plot$mapping)

  test_that("Plot should have price on the x-axis", {
    expect_true("price" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(airbnb_slr_plot$layers[[1]]$geom))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(nrow(airbnb_slr_plot$data), 4852)
    expect_equal(round(sum(airbnb_slr_plot$data$price)), 1994976)
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(airbnb_slr_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(airbnb_slr_plot$labels))
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  answer_as_numeric <- as.numeric(answer1.2)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "9ce8f1f78c56eccda919dc6404297331")
  })

  print("Success!")
}

test_1.3 <- function() {
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

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "airbnb_slr_plot"', {
    expect_true(exists("airbnb_slr_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(airbnb_slr_plot))
  })

  properties <- c(airbnb_slr_plot$layers[[1]]$mapping, airbnb_slr_plot$mapping)

  test_that("Plot should have price on the x-axis", {
    expect_true("price" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(airbnb_slr_plot$layers[[1]]$geom))
    expect_true(any(vapply(airbnb_slr_plot$layers, function(x) "GeomSmooth" %in% class(x$geom), logical(1))))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(nrow(airbnb_slr_plot$data), 4852)
    expect_equal(round(sum(airbnb_slr_plot$data$price)), 1994976)
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(airbnb_slr_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(airbnb_slr_plot$labels))
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "model_airbnb_logistic_area"', {
    expect_true(exists("model_airbnb_logistic_area"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(model_airbnb_logistic_area))
  })

  test_that("Model should use the correct data and variables", {
    expect_equal(nobs(model_airbnb_logistic_area), 4852)
    expect_equal(names(coef(model_airbnb_logistic_area)),
                 c("(Intercept)", "areaEast", "areaSouth", "areaWest"))
    expect_equal(unname(coef(model_airbnb_logistic_area)),
                 c(0.92224313, 0.16942951, -0.20578487, -0.03403537),
                 tolerance = 1e-5)
  })

  print("Success!")
}

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "answer1.6"', {
    expect_true(exists("answer1.6"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E", or "F")', {
    expect_match(answer1.6, "a|b|c|d|e|f", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

test_1.7 <- function() {
    check_MC(answer1.7, getPermutations(LETTERS[1:5]), '95767987b2037a2f09c4e5c0997ec206')
}

test_1.8 <- function() {
  test_that('Did not assign answer to an object called "model_airbnb_logistic_multiple"', {
    expect_true(exists("model_airbnb_logistic_multiple"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(model_airbnb_logistic_multiple))
  })

  test_that("Model should use the correct data and variables", {
    expect_equal(nobs(model_airbnb_logistic_multiple), 4852)
    expect_equal(names(coef(model_airbnb_logistic_multiple)),
                 c("(Intercept)", "areaEast", "areaSouth", "areaWest", "price"))
    expect_equal(unname(coef(model_airbnb_logistic_multiple)),
                 c(0.67204372, 0.20815111, -0.18052895, -0.06622520, 0.00066046),
                 tolerance = 1e-5)
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
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
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
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

#Question 1.11
test_1.11 <- function() {
  test_that('Did not assign answer to an object called "answer1.11"', {
    expect_true(exists("answer1.11"))
  })

  answer_as_numeric <- as.numeric(answer1.11)

  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(answer_as_numeric, 6.6, tolerance = 1e-8)
  })

  print("Success!")
}
