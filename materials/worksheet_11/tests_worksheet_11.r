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

library(digest)
library(testthat)

test_2.1.1 <- function(){
  test_that('Did not assign answer to an object called "full_models"', {
    expect_true(exists("full_models"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(full_models))
  })

  expected_colnames <- c("replicate", "data", "models")
  given_colnames <- colnames(full_models)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(full_models))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(full_models %>% 
    select(models) %>%
    mutate(stats = map_dbl(.x = models, ~glance(.x) %>% pull(statistic))) %>% 
    select(stats) %>% pull(stats)) * 10e4)), "850b432be015f637c2fc8ed08bf24141")
  })

  print("Success!")
}

test_2.1.2 <- function(){
  test_that('Did not assign answer to an object called "forward_selection_F"', {
    expect_true(exists("forward_selection_F"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(forward_selection_F))
  })

  expected_colnames <- c("replicate", "data", "fs_model", "F")
  given_colnames <- colnames(forward_selection_F)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(forward_selection_F))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(forward_selection_F %>% 
    select(fs_model) %>%
    mutate(stats = map_dbl(.x = fs_model, ~glance(.x) %>% pull(statistic))) %>% 
    select(stats) %>% pull(stats)) * 10e4)), "3357a762e776dbfb959887159a6d0f25")
  })

  print("Success!")    
}

test_2.1.3 <- function(){
  test_that('Did not assign answer to an object called "F_critical"', {
    expect_true(exists("F_critical"))
  })

  answer_as_numeric <- as.numeric(F_critical)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "24e611550490afbf2982b38e8736c408")
  })

  print("Success!")
}

test_2.1.4 <- function(){
  test_that('Did not assign answer to an object called "nominal_type_I_error"', {
    expect_true(exists("nominal_type_I_error"))
  })

  answer_as_numeric <- as.numeric(nominal_type_I_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "13dff35616f15788a3612d24ae7e23c5")
  })

  print("Success!")    
}

test_2.1.5 <- function(){
  test_that('Did not assign answer to an object called "forward_selection_type_I_error"', {
    expect_true(exists("forward_selection_type_I_error"))
  })

  answer_as_numeric <- as.numeric(forward_selection_type_I_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "678adea993dab35bb4717c2751c8b8e6")
  })

  print("Success!")     
}

test_2.2 <- function(){
  test_that('Did not assign answer to an object called "fs_error_split"', {
    expect_true(exists("fs_error_split"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(fs_error_split))
  })

  expected_colnames <- c("replicate", "data", "fs_model", "F_fs", "inference_model", "F_inference")
  given_colnames <- colnames(fs_error_split)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(fs_error_split))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(fs_error_split$F_fs) * 10e4)), "a3bc77cdb5ee4f2da345388210bc0c61")
      expect_equal(digest(as.integer(sum(fs_error_split$F_inference) * 10e4)), "32adeb88b01454791004c471309c80c1")
  })

  print("Success!")    
}

test_2.3 <- function(){
  test_that('Did not assign answer to an object called "fs_split_type_I_error"', {
    expect_true(exists("fs_split_type_I_error"))
  })

  answer_as_numeric <- as.numeric(fs_split_type_I_error)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "9c5b3c56a4d13828ce9c13ac77bfd5e0")
  })

  print("Success!")       
}

test_2.4 <- function(){
  test_that('Did not assign answer to an object called "answer2.4"', {
    expect_true(exists("answer2.4"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.4, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.4))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")    
}

test_2.5 <- function(){
  test_that('Did not assign answer to an object called "lasso_study"', {
    expect_true(exists("lasso_study"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(lasso_study))
  })

  expected_colnames <- c("replicate", "data", "lasso_model")
  given_colnames <- colnames(lasso_study)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(lasso_study))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_study %>% 
    select(lasso_model) %>%
    mutate(stats = map_dbl(.x = lasso_model, ~ coef(.x) %>% sum())) %>% 
    select(stats) %>% pull(stats) * 10e2))), "53d4d975104faa0e2c0c4d7ebbc79e33")
  })

  print("Success!")    
}

test_2.6 <- function(){
  test_that('Did not assign answer to an object called "lasso_study"', {
    expect_true(exists("lasso_study"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(lasso_study))
  })

  expected_colnames <- c("replicate", "data", "lasso_model", "lasso_beta1")
  given_colnames <- colnames(lasso_study)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(lasso_study))), "b6a6227038bf9be67533a45a6511cc7e")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_study$lasso_beta1) * 10e2)), "767144b4de709770e931ff9a1f4321d6")
  })

  print("Success!")    
}

test_2.7 <- function(){
  test_that('Did not assign answer to an object called "lasso_beta1_sampling_dist"', {
    expect_true(exists("lasso_beta1_sampling_dist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(lasso_beta1_sampling_dist))
  })

  properties <- c(lasso_beta1_sampling_dist$layers[[1]]$mapping, lasso_beta1_sampling_dist$mapping)

  test_that("Plot should have lasso_beta1 on the x-axis", {
    expect_true("lasso_beta1" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(lasso_beta1_sampling_dist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(lasso_beta1_sampling_dist$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", lasso_beta1_sampling_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(lasso_beta1_sampling_dist$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(lasso_beta1_sampling_dist$data$lasso_beta1))), "069b2c0f2b86c3c21fd96d6bc07a7f02")

    # If lasso_beta1 is not known:
    # expect_equal(digest(round(sum(pull(lasso_beta1_sampling_dist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(lasso_beta1_sampling_dist$labels))
  })

  print("Success!")    
}

test_2.8 <- function(){
  test_that('Did not assign answer to an object called "answer2.8"', {
    expect_true(exists("answer2.8"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.8, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.8))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")    
}

test_2.9 <- function(){
  test_that('Did not assign answer to an object called "post_lasso_lm_beta1_sampling_dist"', {
    expect_true(exists("post_lasso_lm_beta1_sampling_dist"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(post_lasso_lm_beta1_sampling_dist))
  })

  properties <- c(post_lasso_lm_beta1_sampling_dist$layers[[1]]$mapping, post_lasso_lm_beta1_sampling_dist$mapping)

  test_that("Plot should have ls_beta1 on the x-axis", {
    expect_true("ls_beta1" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(post_lasso_lm_beta1_sampling_dist$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(post_lasso_lm_beta1_sampling_dist$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", post_lasso_lm_beta1_sampling_dist$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(post_lasso_lm_beta1_sampling_dist$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(post_lasso_lm_beta1_sampling_dist$data$ls_beta1))), "f0ed27c1d9ffd091cf537a386e2d0036")

    # If ls_beta1 is not known:
    # expect_equal(digest(round(sum(pull(post_lasso_lm_beta1_sampling_dist$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(post_lasso_lm_beta1_sampling_dist$labels))
  })

  print("Success!")        
}
