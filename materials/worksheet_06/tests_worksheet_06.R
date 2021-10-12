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
            "0ea85a4938a6bd2922e0756f96fd366c"))
}

# +
# Question 2.7

test_2.7 <- function() {
    check_DF(model_3_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("68b05229828fa6d7dcfa911368299c6e",
              "13218f6add6ccbc4f58971e9ec8a1801",
              "d6ca323718b7c30dc11bccf7f3228078",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "51bb6dae856f88ada82c256a214f83eb",
              "2c7b2209d53a02474ac34103f509b6e7"))
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
            "b6a6227038bf9be67533a45a6511cc7e",
            c("x_1","x_2"),
            c("1e4","1e4"),
             c("d3b84b697a7f16b54e5da3cd29e803ec",
               "0cea6d69a2dc5a3525954d8211015962"))
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

# +
# Question 3.0 

test_3.0 <- function() {
    check_MC(answer3.0, LETTERS[1:3], "ddf100612805359cd81fdc5ce3b9fbba")
}

# +
# Question 3.1

test_3.1 <- function() {
    check_MC(answer3.1, LETTERS[1:3], "127a2ec00989b9f7faf671ed470be7f8")
}

# +
# Question 3.2

test_3.2 <- function() {
    check_TF(answer3.2, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 3.3

test_3.3 <- function() {
    
    check_numeric(pop.pool.Y, 1,"13dff35616f15788a3612d24ae7e23c5")
}

# +
# Question 3.4


test_3.4 <- function() {
    check_DF_overflow(pop.pool,
                     c("athlete","y_current_ad", "y_new_ad" ),
                     "a7a1a6213fbb1a97362d5a9ff37cafc4",
                     c("y_current_ad", "y_new_ad" ),
                     c(1e4,1e4),
                     c("dee1c433799ced445e1e58d91ddb3ffa",
                       "064a8cfdfbb06cb4205a6aa45a1a0b06"))
}

# +
# Question 3.5


test_3.5 <- function() {
    check_DF(sample_TikTok,
                     c("replicate","athlete","y_current_ad","y_new_ad","x_self_choice","y_obs"),
                     "b6a6227038bf9be67533a45a6511cc7e",
                     c("replicate", "y_obs" ),
                     c(1,1e4),
                     c("b6a6227038bf9be67533a45a6511cc7e",
                       "f5cd9cd8b89f942161f1c187b5ea4d46"))
}

# +
# Question 3.6

test_3.6 <- function() {
    check_plot_factor(obs_dwell_time_boxplots,
                     "x_self_choice",
                      "GeomBoxplot",
                      FALSE,
                      "3e2e4a08c44d0224de5b7e668c75ace3",
                      "b6a6227038bf9be67533a45a6511cc7e",
                      TRUE)
}
# -

# Question 3.7
test_3.7 <- function() {
    check_DF(conf_obs_study_TikTok_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "c01f179e4b57ab8bd9de309e6d576c48",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("e5f641b8b36acfdf17185a0cfd492264",
              "7d2842cab7725fd8f382293e410d42b2",
              "0e5702e520327ea5a15fec70319c7598",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "39423526bb50f888ced71b7b6fe8eee3",
              "a557dae246c6d4f57eba2444b4938f3d"))
}

# +
# Question 3.8

test_3.8 <- function() {
    check_MC(answer3.8, LETTERS[1:4],'127a2ec00989b9f7faf671ed470be7f8' )
}

# +
# Question 3.9

test_3.9 <- function() {
    check_DF(MLR_obs_study_TikTok_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("dc154a9012120d92e7a7b57bd5d30a6c",
              "1b0ed73227e2e7826da63b2b356975e0",
              "82ef18400c40b2923b8ce97f346774f7",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "d1f5eaa8dc87a470b2553d1b39978ae8",
              "5bed3f912bd3a02da1cba0611651347a"))    
}

# +
# Question 3.10

test_3.10 <- function() {
    check_MC(answer3.10, LETTERS[1:4], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 3.11

test_3.11 <- function() {
    check_DF(sample_TikTok,
            c("replicate", "athlete", "y_current_ad", "y_new_ad", "x_self_choice", "y_obs", "x_randomized", "y_exp"),
             "b6a6227038bf9be67533a45a6511cc7e",
             c("y_exp"),
             c(1e4),
             c("e32bccda1eb36b43ead1201a79c6aa83")
            )
}
# -

# Question 3.12
test_3.12 <- function() {
    check_plot_factor(exp_dwell_time_boxplots,
                     "x_randomized",
                      "GeomBoxplot",
                      FALSE,
                      "3e2e4a08c44d0224de5b7e668c75ace3",
                      "b6a6227038bf9be67533a45a6511cc7e",
                      TRUE)
}

# +
# Question 3.13

test_3.13 <- function() {
    check_DF(exp_study_TikTok_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            "c01f179e4b57ab8bd9de309e6d576c48",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
            c(100, 100, 100,   1, 100, 100),
            c("8e8ba5b2107ac8ecbcbf5651370caee8",
              "5e9df39bb0ad592a23f9d09fd6673704",
              "2f76f30294d72a2d252c8f4aafaee698",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "76c7d4c2f1b5253b564ca48ce74fe890",
              "ac7329c10590742d9f1ff20331821bad"))
}

# +
# Question 3.14

test_3.14 <- function() {
    check_MC(answer3.14, LETTERS[1:4], 'ddf100612805359cd81fdc5ce3b9fbba')
}
