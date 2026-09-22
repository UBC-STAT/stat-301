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


# + -------------------------------------------
# Question 1.0

test_1.0 <- function() {
    check_DF(sample_model_1,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("f29e1853df1168481f6821edb2740a6f",
            "43041dcf84ed7ef84fca8aedb683cb9c"))
}

# +
# Question 1.1

test_1.1 <- function() {
    check_DF(model_1_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("0d81857ffc6d5db1c852d0a738cfd576",
              "22b237f09483940655e05981a6c3105e",
              "bf3c23cd615ba6be57dbf62e3ab52b2c",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "bdc073fa004828fea53f656fbd697dd1",
              "b0458e02f315090aa9272b4b6a3d22f3"))
}

# +
# Question 1.2
test_1.2 <- function() {
    check_DF(
        se_homoscedastic,
        c("SD of slope estimates", "Average reported SE"),
        "4b5630ee914e848e8d07221556b0a2fb",
        c("SD of slope estimates", "Average reported SE"),
        c(1000, 1000),
        c("09b7c5f2db0f3079ce4979b5a75e4a45",
            "09b7c5f2db0f3079ce4979b5a75e4a45"
        )
    )
}

# +
# Question 1.3

test_1.3 <- function() {
    check_MC(answer1.3, LETTERS[1:3],"127a2ec00989b9f7faf671ed470be7f8")
}

# +
# Question 2.0

test_2.0 <- function() {
    check_DF(sample_model_2,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("7e063343df8cf96ca84cd816c94681e0",
            "b5b951725f5f4955fde997a9b00ca749"))
}

# +
# Question 2.1

test_2.1 <- function() {
    check_DF(model_2_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("16b4a2550c8f3c736feeb35b4545e8ba",
              "22b237f09483940655e05981a6c3105e",
              "0ab7efa2d834db8768c3a66fe070f82d",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "afe11f97a06d0b0ae51ca6e509aee06e",
              "c510e5a4d3b7718bdbf4d989bbbbb2be"))
}


# +
# Question 2.2


test_2.2 <- function() {
    
    check_DF(
        se_comparison,
        c("model", "SD of slope estimates", "Average reported SE"),
        "c01f179e4b57ab8bd9de309e6d576c48",
        c("SD of slope estimates", "Average reported SE"),
        c(1000, 1000),
        c(
            "f645a9e29d1a7807fcc38e249ebd7e25",
            "0aff7df375643a94b1b39f2210e32224"
        )
    )
    
    test_that("model does not contain the correct values", {
        expect_equal(
            digest(sort(as.character(se_comparison$model))),
            "2e44fd72b04304e0f0dbe376a1f14274"
        )
    })
}

# +
# Question 2.3

test_2.3 <- function() {
    check_MC(answer2.3, LETTERS[1:2],"127a2ec00989b9f7faf671ed470be7f8")
}

# +
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, LETTERS[1:3],"ddf100612805359cd81fdc5ce3b9fbba")
}

# +
# Question 3.1

test_3.1 <- function() {
    check_DF(sample_model_3,
            c("x_1", "x_2", "y"),
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1", "y"  ),
            c(1e4,1e4),
            c("ecdea215a08b5df9cc6112021a77c587",
            "7c327b0aaf16db22f2d349c501d771df"))
}

# +
# Question 3.2

test_3.2 <- function() {
    check_DF(model_3_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("7dc81f42de9970af4cb6d6c37e1d2299",
              "58358dd2dcc33f743f22d1a7753b7ec0",
              "1beb34ed8639cf3069a9d4b21bf49b4c",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "19d958113c5ec8abd30cec8eed3b1f24",
              "ff89d0af9fe4846b755a48d9216e230d"))
}

# +
# Question 3.3

test_3.3 <- function() {

    check_DF(
        normality_assumption,
        c(
            "model",
            "Average of slope estimates",
            "SD of slope estimates",
            "Average reported SE"
        ),
        "c01f179e4b57ab8bd9de309e6d576c48",
        c(
            "Average of slope estimates",
            "SD of slope estimates",
            "Average reported SE"
        ),
        c(1000, 1000, 1000),
        c(
            "cc79a98dfe28274e276977087077c54d",
            "14e2b9d8a5572dc9fb8a2ca2c9d8b373",
            "78277fc86e8edd5ddd92b780ef1c9844"
        )
    )
}

# +
# Question 3.4
test_3.4 <- function() {
    check_MC(answer3.4, LETTERS[1:3],'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 3.5
test_3.5 <- function() {
    check_MC(answer3.5, LETTERS[1:4],'6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 4.0

test_4.0 <- function() {
    check_DF(bivariate_normal_sample,
            c("x_1","x_2"),
            "5d6e7fe43b3b73e5fd2961d5162486fa",
            c("x_1","x_2"),
            c("1e4","1e4"),
            c("e567ba7a4032de033c11daccee250041",
              "ac0e5e9fc22ef83294c518e30d16bc50"))
}

# +
# Question 4.1

test_4.1 <- function() {
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
# Question 4.2

test_4.2 <- function() {
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
# Question 4.3

test_4.3.0 <- function() {
    check_plot(hist_multicollinearity_slope_x_1,
               "beta_1_hat",
               "GeomBar",
               TRUE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "b6a6227038bf9be67533a45a6511cc7e",
               "6e26e0ac98bf6e4f5e02637a77f96006",
               TRUE)
}

test_4.3.1 <- function() {
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
# Question 4.4

test_4.4 <- function() {
    check_MC(answer4.4, LETTERS[1:3],'ddf100612805359cd81fdc5ce3b9fbba')
}

