library(digest)
library(testthat)

# +
#abstraction templates

check_TF <- function(answerX.X, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answerX.X, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answerX.X))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, expectedHash)
  })
  
  print("Success!")
}

check_MC <- function(answerX.X, choiceList, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that(paste('Solution should be a single character ', toString(choiceList)), {
    expect_true(tolower(answerX.X) %in% tolower(choiceList))
  })
  
  answer_hash <- digest(tolower(answerX.X))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, expectedHash)
  })
  
  print("Success!")
}



# dataCheckTuples is data.frame(c(colnames), c(scale factor), c(expectedHash))
check_DF <- function(answerX.X, expected_colnames, hashNRows, cols_to_check, precision_list, expectedHashes) {
  dataCheckTuples <- data.frame(cols_to_check, precision_list, expectedHashes) 
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answerX.X))
  })
  
  given_colnames <- colnames(answerX.X)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answerX.X))), hashNRows)
  })
  
  
  
  apply(dataCheckTuples, 1, function(tuple) {
    test_that(paste(tuple[[1]], " does not contain the correct data"), {
      expect_equal(digest(as.integer(sum(answerX.X[tuple[[1]]]) * as.double(tuple[[2]]))),
                   tuple[[3]])
    })
  })
  
  
  
  
  print("Success!")
}





# dataCheckTuples is data.frame(c(colnames), c(scale factor), c(expectedHash))
check_DF_overflow <- function(answerX.X, expected_colnames, hashNRows, cols_to_check, precision_list, expectedHashes) {
  dataCheckTuples <- data.frame(cols_to_check, precision_list, expectedHashes) 
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(answerX.X))
  })
  
  given_colnames <- colnames(answerX.X)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(answerX.X))), hashNRows)
  })
  
  
  
  apply(dataCheckTuples, 1, function(tuple) {
    test_that(paste(tuple[[1]], " does not contain the correct data"), {
      expect_equal(digest(trunc(sum(answerX.X[tuple[[1]]]) * as.double(tuple[[2]]))),
                   tuple[[3]])
    })
  })
  
  
  
  
  print("Success!")
}




check_numeric <- function(answerX.X, precision, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  answer_as_numeric <- as.numeric(answerX.X)
  test_that(paste(var_name, " should be a number"), {
    expect_false(is.na(answer_as_numeric))
  })
  
  test_that(paste(var_name, " value is incorrect"), {
    expect_equal(digest(as.integer(answer_as_numeric * precision)), expectedHash)
  })
  
  print("Success!")
}



check_numeric_element <- function(answerX.X, precision, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  
  answer_as_numeric <- as.numeric(answerX.X)
  test_that(paste(var_name, " should be a number"), {
    expect_false(is.na(answer_as_numeric))
  })
  
  test_that(paste(var_name, " value is incorrect"), {
    expect_equal(digest(as.integer(answer_as_numeric * precision)), expectedHash)
  })
  
  print("Success!")
}




check_plot <- function(answerX.X, x_axis_var, geom_type, hasVline, bin_width_hash, nrow_hash, x_axis_var_hash, hasTitle) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(answerX.X))
  })
  
  properties <- c(answerX.X$layers[[1]]$mapping, answerX.X$mapping)
  
  test_that(paste("Plot should have ", x_axis_var," on the x-axis"), {
    expect_true(x_axis_var == rlang::get_expr(properties$x))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true(geom_type %in% class(answerX.X$layers[[1]]$geom))
    
    if(hasVline) {
      expect_true("GeomVline" %in% class(answerX.X$layers[[2]]$geom))
    }
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", answerX.X$layers[[1]])[["stat_params"]][["binwidth"]])),
      bin_width_hash)
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(answerX.X$data)), nrow_hash)
    expect_equal(digest(round(sum(answerX.X$data[x_axis_var]))), x_axis_var_hash)
    
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(answerX.X$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(answerX.X$labels$x == toString(rlang::get_expr(properties$x)))
  })
  
  if(hasTitle){
    
    test_that("Plot should have a title", {
      expect_true("title" %in% names(answerX.X$labels))
    })
  }
  
  
  print("Success!")
}




#REFACTOR FOR LATER WORKSHEETS
check_plot_factor <- function(answerX.X, x_axis_var, geom_type, hasVline, bin_width_hash, nrow_hash, hasTitle) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(answerX.X))
  })
  
  properties <- c(answerX.X$layers[[1]]$mapping, answerX.X$mapping)
  
  test_that(paste("Plot should have ", x_axis_var," on the x-axis"), {
    expect_true(x_axis_var == rlang::get_expr(properties$x))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true(geom_type %in% class(answerX.X$layers[[1]]$geom))
    
    if(hasVline) {
      expect_true("GeomVline" %in% class(answerX.X$layers[[2]]$geom))
    }
  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", answerX.X$layers[[1]])[["stat_params"]][["binwidth"]])),
      bin_width_hash)
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(answerX.X$data)), nrow_hash)
    #expect_equal(digest(round(sum(answerX.X$data[x_axis_var]))), x_axis_var_hash)
    
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(answerX.X$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(answerX.X$labels$x == toString(rlang::get_expr(properties$x)))
  })
  
  if(hasTitle){
    
    test_that("Plot should have a title", {
      expect_true("title" %in% names(answerX.X$labels))
    })
  }
  
  
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

# +
# Question 1.0

test_1.0 <- function() {
  check_TF(answer1.0, "d2a90307aac5ae8d0ef58e2fe730d38b")
}


# +
# Question 1.1

test_1.1 <- function() {
    check_TF(answer1.1, "d2a90307aac5ae8d0ef58e2fe730d38b")
}

# +
# Question 1.2

test_1.2 <- function() {
    check_TF(answer1.2, "d2a90307aac5ae8d0ef58e2fe730d38b")
}

# +
# Question 1.3

test_1.3 <- function() {
    check_TF(answer1.3, "05ca18b596514af73f6880309a21b5dd")
}

# +
# Question 1.4

test_1.4 <- function() {
    check_TF(answer1.4, "05ca18b596514af73f6880309a21b5dd")
}

# +
# Question 1.5

test_1.5 <- function() {
    check_TF(answer1.5, "d2a90307aac5ae8d0ef58e2fe730d38b")
}

# +
# Question 2.0

test_2.0 <- function() {
    check_DF(sample_model_1,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("abc3c32f137f778a61e835a3cdbd886b",
            "24739af1cf6a4cd57c893df135e0d6a6"))
}

# +
# Question 2.1

test_2.1 <- function() {
    check_DF(model_1_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("fd4bf0cc10c8f68e115bbad8811488b8",
              "80b0ae73fe0e882b0a24973e4e2c8203",
              "52cc5618c657e449125cd997cc304ad7",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "afe11f97a06d0b0ae51ca6e509aee06e",
              "2e83e6e2e40c264b0e566c2c727f56d5"))
}

# +
# Question 2.2

test_2.2 <- function() {
    check_DF(sample_model_2,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("d7ee30e8c0cd41015eab2bde35f92e6c",
            "80a5ed6ab7d175dd0a33984174587be8"))
}

# +
# Question 2.3

test_2.3 <- function() {
    check_DF(model_2_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("db64edbcb997dc203ad6d57aa44a3b7f",
              "41710a1acb82ca367c04178de2b76c2a",
              "b5a78594381998e33550e962b0c1dfdb",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "81ba7b4d8dddb45e1642351f83dc83d2",
              "4c092dd7d8497e5c2a8aa32fafba5626"))
}

# +
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, LETTERS[1:2],"127a2ec00989b9f7faf671ed470be7f8")
}

# +
# Question 2.5

test_2.5 <- function() {
    check_MC(answer2.5, LETTERS[1:3],"ddf100612805359cd81fdc5ce3b9fbba")
}

# +
# Question 2.6

test_2.6 <- function() {
    check_DF(sample_model_3,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("0d985258b89066a3033378bb3a004169",
            "36db37b9aa2772116b92014ab65ded17"))
}

# +
# Question 2.7

test_2.7 <- function() {
    check_DF(model_3_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("beea233343af0cec984b2cddeefbd75e",
              "319a0ebaed1dd330838238a077d60c6d",
              "e37c5f2b59fb6ec2e5bb3c780f5d6dd6",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "c494f6f961fc113291305d47a8ed0fb9",
              "91d6e8498d941c4ad039b279ab4bc3e2"))
}

# +
# Question 2.8

test_2.8 <- function() {
    check_MC(answer2.8, LETTERS[1:4],'6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 2.9

test_2.9 <- function() {
    check_DF(bivariate_normal_sample,
            c("x_1","x_2"),
            "5d6e7fe43b3b73e5fd2961d5162486fa",
            c("x_1","x_2"),
            c("1e4","1e4"),
            c("e567ba7a4032de033c11daccee250041",
              "ac0e5e9fc22ef83294c518e30d16bc50"))
}

# +
# Question 2.10

test_2.10 <- function() {
    check_DF(lm_multicollinearity,
             c("intercept",  "beta_1_hat", "beta_2_hat"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("intercept",  "beta_1_hat", "beta_2_hat"),
            c("1e4","1e4","1e4"),
            c("622b645c99b31db59c8d5d9248796879",
              "3190f750af0a8f3c77730a17b2a8c30f",
              "70cd29ce0dfec7f1e457f934d60f683b"))
}

# +
# Question 2.11

test_2.11 <- function() {
    check_DF(lm_no_multicollinearity,
             c("intercept",  "beta_1_hat", "beta_2_hat"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("intercept",  "beta_1_hat", "beta_2_hat"),
            c("1e4","1e4","1e4"),
            c("4cb7eb252fabbedf665ca371ce7e0a04",
              "c6f488919530850d21150f6d17f547d7",
              "73a0b5017d714c3c1589231a93c44b31"))
}

# +
# Question 2.12

test_2.12.0 <- function() {
    check_plot(hist_multicollinearity_slope_x_1,
               "beta_1_hat",
               "GeomBar",
               TRUE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "b6a6227038bf9be67533a45a6511cc7e",
               "6e26e0ac98bf6e4f5e02637a77f96006",
               TRUE)
}

test_2.12.1 <- function() {
    check_plot(hist_no_multicollinearity_slope_x_1,
               "beta_1_hat",
               "GeomBar",
               TRUE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "b6a6227038bf9be67533a45a6511cc7e",
               "6e26e0ac98bf6e4f5e02637a77f96006",
               TRUE)
}

# +
# Question 2.13

test_2.13 <- function() {
    check_MC(answer2.13, LETTERS[1:3],'ddf100612805359cd81fdc5ce3b9fbba')
}

