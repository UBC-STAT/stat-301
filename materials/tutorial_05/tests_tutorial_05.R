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




check_plot <- function(answerX.X, x_axis_var, geom_type, hasVline, bin_width_hash, nrow_hash,
                       x_axis_var_hash, hasTitle, isFactor, shouldRename) {
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
    if (!isFactor) {
      expect_equal(digest(round(sum(answerX.X$data[x_axis_var]))), x_axis_var_hash)
    }
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(answerX.X$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
    if (shouldRename) {
          test_that("x-axis label should be descriptive and human readable", {
            expect_false(answerX.X$labels$x == toString(rlang::get_expr(properties$x)))
          })
        }
  
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
# Question 1.1


test_1.1 <- function() {
    temp <<- CASchools_pair_plots[1,1]
    check_plot(temp, 
              "calworks",
               "GeomDensity",         
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "d6af036ffbdd7ccfc34dc7862b0e50d3",
               "1a7aa4dc92e6fbe06d29794cb5777924",
               FALSE,
               FALSE,
              FALSE)
}

# +
# Question 1.3

test_1.3 <- function() {
    check_DF(corr_matrix_CASchools,
            c("var1", "var2", "corr"),
            "754bc61780613e9dbb928b88e1e3d6f5",
             c("corr"),
             c(10000),
             c("3e44bce2c3b9c956cf6c6a3883f0cbef"))
}

# +
# Question 1.4

test_1.4 <- function() {
    check_plot(plot_corr_matrix_CASchools,
              "var1",
               "GeomTile",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "754bc61780613e9dbb928b88e1e3d6f5",
               "none",
               FALSE,
               TRUE,
               TRUE)
}

# +
# Question 1.5

test_1.5 <- function() {
    check_DF(MLR_CASchools_results,
            c("term", "estimate",  "std.error", "statistic", "p.value"),  
             "9d08099943f8627959cfb8ecee0d2f5d",
             c("estimate",  "std.error", "statistic", "p.value"),  
             c(100, 100, 100,   1),
             c("984f423693c72ee0e5a7e8eedf47deeb",
               "9f1dae3619860a497fda8a269d91e70b",
               "f812d63cfcf68da8e63cb04531f4fd6f",
               "4b5630ee914e848e8d07221556b0a2fb"))
}

# +
# Question 1.6


test_1.6 <- function() {
    
    check_numeric_element(VIF_MLR_CASchools[1],1e3,"e48abb05652496fb753bde5f168ac169")
}

# +
# Question 1.7

test_1.7.0 <- function() {
    check_MC(answer1.7.0, getPermutations(LETTERS[1:7]),'8b63e49106226d2a45958a7e24c97c37')
}


test_1.7.1 <- function() {
    check_MC(answer1.7.1, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.8

test_1.8 <- function() {
    check_DF(red_MLR_CASchools_results,
            c("term", "estimate",  "std.error", "statistic", "p.value"),  
             "7c7124efff5c7039a1b1e7cba65c5379",
             c("estimate",  "std.error", "statistic", "p.value"),  
             c(100, 100, 100,   1),
             c("d4beb42a135c421b1dd3d4eb331e5bdb",
               "75552ea47a39b5896014b674f97f9849",
               "d2192d973d9361d9018ad1df3155ee24",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}

# +
# Question 1.10


test_1.10 <- function() {
    
    check_numeric_element(VIF_red_MLR_CASchools[1],1e3,"97401718fa863418de072406b979c7db")
}

# +
# Question 1.11

test_1.11 <- function() {
    check_MC(answer1.11, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 2.0

test_2.0 <- function() {
  check_MC(answer2.0, LETTERS[1:3], "ddf100612805359cd81fdc5ce3b9fbba")
}

# Question 2.1

test_2.1 <- function() {
  check_MC(answer2.1, LETTERS[1:3], "127a2ec00989b9f7faf671ed470be7f8")
}

# +
# Question 2.2

test_2.2 <- function() {
  check_TF(answer2.2, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 2.3

test_2.3 <- function() {
  
  check_numeric(pop.pool.Y, 1,"13dff35616f15788a3612d24ae7e23c5")
}

# +
# Question 2.4


test_2.4 <- function() {
  check_DF_overflow(pop.pool,
                    c("athlete","y_current_ad", "y_new_ad" ),
                    "a7a1a6213fbb1a97362d5a9ff37cafc4",
                    c("y_current_ad", "y_new_ad" ),
                    c(1e4,1e4),
                    c("dee1c433799ced445e1e58d91ddb3ffa",
                      "064a8cfdfbb06cb4205a6aa45a1a0b06"))
}

# +
# Question 2.5


test_2.5 <- function() {
  check_DF(sample_TikTok,
           c("replicate","athlete","y_current_ad","y_new_ad","x_self_choice","y_obs"),
           "b6a6227038bf9be67533a45a6511cc7e",
           c("replicate", "y_obs" ),
           c(1,1e4),
           c("b6a6227038bf9be67533a45a6511cc7e",
             "8b20b9383d5077c07873c905568e6e15"))
}

# +
# Question 2.6

test_2.6 <- function() {
  check_plot_factor(obs_dwell_time_boxplots,
                    "x_self_choice",
                    "GeomBoxplot",
                    FALSE,
                    "3e2e4a08c44d0224de5b7e668c75ace3",
                    "b6a6227038bf9be67533a45a6511cc7e",
                    TRUE)
}
# -

# Question 2.7
test_2.7 <- function() {
  check_DF(conf_obs_study_TikTok_results,
           c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
           "c01f179e4b57ab8bd9de309e6d576c48",
           c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
           c(100, 100, 100,   1, 100, 100),
           c("ac7329c10590742d9f1ff20331821bad",
             "263b9db4d09244c0e0ff3f826caa8d2e",
             "1f3b4d86534afbad2593c187e4c2c7f7",
             "1473d70e5646a26de3c52aa1abd85b1f",
             "5b2e99fdd726bddde47277b86fab1d17",
             "31fd027580dba8ebac53dc84697709c6"))
}

# +
# Question 2.8

test_2.8 <- function() {
  check_MC(answer2.8, LETTERS[1:4],'127a2ec00989b9f7faf671ed470be7f8' )
}

# +
# Question 2.9

test_2.9 <- function() {
  check_DF(MLR_obs_study_TikTok_results,
           c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
           "11946e7a3ed5e1776e81c0f0ecd383d0",
           c("estimate",  "std.error", "statistic", "p.value",   "conf.low" , "conf.high"),
           c(100, 100, 100,   1, 100, 100),
           c("dc154a9012120d92e7a7b57bd5d30a6c",
             "be3c152f6f6bcd5f85f9e4cba49b1e48",
             "2099142b74c3debf8dedee87e78f44ac",
             "1473d70e5646a26de3c52aa1abd85b1f",
             "ea578796f641198055e6e5a42a2061da",
             "23345462bd191f717ce512aeb3fd5fbe"))    
}

# +
# Question 2.11

test_2.11 <- function() {
  check_DF(sample_TikTok,
           c("replicate", "athlete", "y_current_ad", "y_new_ad", "x_self_choice", "y_obs", "x_randomized", "y_exp"),
           "b6a6227038bf9be67533a45a6511cc7e",
           c("y_exp"),
           c(1e4),
           c("e32bccda1eb36b43ead1201a79c6aa83")
  )
}
# -

# Question 2.12
test_2.12 <- function() {
  check_plot_factor(exp_dwell_time_boxplots,
                    "x_randomized",
                    "GeomBoxplot",
                    FALSE,
                    "3e2e4a08c44d0224de5b7e668c75ace3",
                    "b6a6227038bf9be67533a45a6511cc7e",
                    TRUE)
}

# +
# Question 2.13

test_2.13 <- function() {
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
# Question 2.14

test_2.14 <- function() {
  check_MC(answer2.14, LETTERS[1:4], 'ddf100612805359cd81fdc5ce3b9fbba')
}

