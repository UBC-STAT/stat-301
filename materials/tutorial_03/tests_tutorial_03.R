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
            
# +
# Question 1.0

test_1.0 <- function() {
  check_TF(answer1.0, "05ca18b596514af73f6880309a21b5dd")
}
# -


# Question 1.1
test_1.1 <- function() {
  check_TF(answer1.1, 'd2a90307aac5ae8d0ef58e2fe730d38b')
}


# Question 1.2
test_1.2 <- function() {
  
  check_TF(answer1.2, '05ca18b596514af73f6880309a21b5dd')
}


# Question 1.3
test_1.3 <- function() {
  check_TF(answer1.3, 'd2a90307aac5ae8d0ef58e2fe730d38b')
}

# Question 2.0

test_2.0 <- function() {
  check_DF(caschools_head,
          c("grades","income","english","read"),
          "11946e7a3ed5e1776e81c0f0ecd383d0",
          c("income","english","read"),
          c(10,10,10),
          c("5ef84c68f1ccff7f06870d41626122bb",
            "5074d25502bb4eb28a2513d75ece147f",
            "0be3be21683085ed6ca55bc92dd03ceb"
            )
  )
}

# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })
  
  answer_as_numeric <- as.numeric(answer2.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })
  
  test_that("Solution should be an integer", {
    expect_true(answer_as_numeric %% 1 == 0)
  })
  
  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "d6af036ffbdd7ccfc34dc7862b0e50d3")
  })
  
  print("Success!")
}

# Question 2.2

test_2.2 <- function() {
  check_DF(caschools_stats, 
           c("key", "mean", "sd", "max", "min"),
           '11946e7a3ed5e1776e81c0f0ecd383d0',
           c("mean", "sd", "max", "min"),
           c(1e4,1e4,10,10),
           c("eef029906ce99f824335eba60434dec8",
             "9b8d9791ba142ecb8755e1e3a5a35b75",
             "21d6519b1cf71b78bdeb305b70b29f08",
             "f4bf4315e435dbc2d1b1192511f766fa"))
  
}

# +
# Question 2.3

test_2.3 <- function() {
    test_that(paste('Did not assign answer to an object called "caschools_pair_plots"', ""), {
        expect_true(exists('caschools_pair_plots'))
    })
    
    test_that("Solution should be a ggplot object", {
        expect_true('ggmatrix' %in% class(caschools_pair_plots))
    })
    
    test_that("caschools_pair_plots$data should be a dataframe object", {
        expect_true('data.frame' %in% class(caschools_pair_plots$data))
    })
    
    given_colnames <- colnames(caschools_pair_plots$data)
    expected_colnames <- c("grades", "income", "english", "read")
    
    test_that("caschools_pair_plots$data does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
    test_that("Incorrect caschools_pair_plots$data", {
        expect_equal(digest(sum(caschools_pair_plots$data[,4]) * 1e4), '085774ddd48610eed6eceb169341a7fa')
    })
    
    print("Success!")
}
# -

# Question 2.4
test_2.4 <- function() {
    check_MC(answer2.4, LETTERS[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# Question 3.0
test_3.0 <- function() {
  check_MC(answer3.0, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}


# +
# Question 3.1
test_3.1.0 <- function() {
    check_MC(answer3.1.0, c("read","income"),
             '5aef5bcbb5ac582272cbbb8e75b50902')
}

test_3.1.1 <- function() {
    check_MC(answer3.1.1, c("read","income"), '86ceffa2bf0fe2928a32579194f25acc')
}

# +
# Question 3.2


test_3.2 <- function() {
    check_numeric_element(caschools_SLR$coefficients[1], 1e4, "bc0f332fea1be41d6ee716bbe3489eeb")
    check_numeric_element(caschools_SLR$coefficients[2], 1e4, "9216ceef274bb0c87ae07f0a1dbbf848")
}

# +
# Question 3.3

test_3.3 <- function() {
    check_DF(caschools_SLR_results,
            c("term", "estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("bcc99cccd06bd8e8bf00aeb62f750dc7",
               "ed195206b77fdf687961e3fa4d671e19",
               "93ad3994836a029ca9277c2b15fa4ce8",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "f7a87364dccc87d6c9c89c3ba80d78c9",
               "37d43bd2521873e6df7f15621ce70d70"))
}

# +
# Question 3.4

test_3.4 <- function() {
    check_plot(caschools_scatterplot, "income", "GeomPoint", F,
               "3e2e4a08c44d0224de5b7e668c75ace3", 'd6af036ffbdd7ccfc34dc7862b0e50d3', 
               "13742f581e47f00fd8beeade76db838c", F)
}

# +
# Question 3.5

#manual graded

# +
# Question 4.0

test_4.0 <- function() {
   check_TF(answer4.0, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 4.1

test_4.1 <- function() {
    check_MC(answer4.1, LETTERS[1:3],'127a2ec00989b9f7faf671ed470be7f8')
}


# +
# Question 4.3

test_4.3 <- function() {
    check_MC(answer4.3, LETTERS[1:4], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 4.4

test_4.4 <- function() {
    check_DF(lm_boot, 
            c("boot_intercept", "boot_slope"),    
             "b6a6227038bf9be67533a45a6511cc7e",
             c("boot_intercept", "boot_slope"),
             c(100, 10000),
             c("2f101fa21c201ade58ac9a03d9b5c829",
               "a1b11d35248c89c76694773fe754ee38"))
    
}

# +
# Question 4.5

test_4.5 <- function() {
    check_plot(slope_sampling_dist_boot, "boot_slope",
               "GeomBar", F, "3e2e4a08c44d0224de5b7e668c75ace3", "b6a6227038bf9be67533a45a6511cc7e",
               "2eb9b2370204e9a3e69eb9dfcce47c82", T)
}

# +
# Question 4.6

test_4.6 <- function() {
    check_plot(slope_sampling_dist_boot_limits, "boot_slope",
               "GeomBar", T, "3e2e4a08c44d0224de5b7e668c75ace3", "b6a6227038bf9be67533a45a6511cc7e",
               "2eb9b2370204e9a3e69eb9dfcce47c82", T)
}
