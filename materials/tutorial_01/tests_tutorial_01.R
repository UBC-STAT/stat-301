library(digest)
library(testthat)


# Question 1.0

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "click_through"', {
    expect_true(exists("click_through"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(click_through))
  })

  expected_colnames <- c("webpage", "adjusted_clicks", "target_clicks", "click_rate")
  given_colnames <- colnames(click_through)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(click_through))), "dd4ad37ee474732a009111e3456e7ed7")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(click_through$click_rate) * 1e5)), "4fb26da672e2215197c3e498d40ef845")
  })

  print("Success!")
}
# -


# Q 1.1
test_1.1 <- function() {
  test_that('Did not assign answer to an object called "click_through"', {
    expect_true(exists("click_through"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(click_through))
  })

  expected_colnames <- c("webpage",	"adjusted_clicks",	"target_clicks",	"click_rate",	"lower_ci",	"upper_ci")
  given_colnames <- colnames(click_through)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(click_through))), "dd4ad37ee474732a009111e3456e7ed7")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(click_through$lower_ci) * 10000)), "35ffe21abbcfd5c798d71a4d213565b9")
    expect_equal(digest(as.integer(sum(click_through$upper_ci) * 10000)), "594e16dba1ef086cfaa5933544ac1dde")
  })

  print("Success!")
}

# +
# Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "CIs_click_through_rates"', {
    expect_true(exists("CIs_click_through_rates"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(CIs_click_through_rates))
  })

  properties <- c(CIs_click_through_rates$layers[[1]]$mapping, CIs_click_through_rates$mapping)

  test_that("Plot should have X_AXIS_VAR on the x-axis", {
    expect_true("webpage" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(CIs_click_through_rates$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(CIs_click_through_rates$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", CIs_click_through_rates$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(CIs_click_through_rates$data)), "dd4ad37ee474732a009111e3456e7ed7")
    #expect_equal(digest(round(sum(CIs_click_through_rates$data$X_AXIS_VAR))), "HASH_HERE")

    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(CIs_click_through_rates$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(CIs_click_through_rates$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(CIs_click_through_rates$labels))
  })

  print("Success!")
}


# +
# Question 1.3

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  test_that('Solution should be a single character ("A" or "B")', {
    expect_match(answer1.3, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.3))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

# -

# Question 1.4
test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer1.4, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.4))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}


# +
# Question 1.5

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.5, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.5))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}


# +
# Question 1.6

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "pairwise_comparisons"', {
    expect_true(exists("pairwise_comparisons"))
  })

  test_that("Solution should be a pairwise.htest", {
    expect_true("pairwise.htest" %in% class(pairwise_comparisons))
  })
    
  pairwise_comparisons_df <- as.data.frame(pairwise_comparisons$p.value)
  expected_colnames <- c('Interact','Connect','Learn','Help')
  given_colnames <- colnames(pairwise_comparisons_df)
  test_that("p.value data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("p.value data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(pairwise_comparisons_df))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("p.value data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(pairwise_comparisons_df$Interact[!is.na(pairwise_comparisons_df$Interact)]) * 1e5)),"52a09758fe74224408f3370adde49650")
    expect_equal(digest(as.integer(sum(pairwise_comparisons_df$Connect[!is.na(pairwise_comparisons_df$Connect)]) * 1e5)),"e23f3b70b25aedb7b4e430ad53739836")
    expect_equal(digest(as.integer(sum(pairwise_comparisons_df$Learn[!is.na(pairwise_comparisons_df$Learn)]) * 1e5)),"11e7fecf5a45ab86af9ea5081f609319")
    expect_equal(digest(as.integer(sum(pairwise_comparisons_df$Help[!is.na(pairwise_comparisons_df$Help)]) * 1e5)),"4fd46b219231bc3f244c9cda8499aa45")
  })

  print("Success!")
}


# +
# Question 1.7

test_1.7 <- function() {
  test_that('Did not assign answer to an object called "answer1.7"', {
    expect_true(exists("answer1.7"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.7, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.7))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "5569ed730134a270952e469c2be78612")
  })

  print("Success! You've finished week 1!!")
}

