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
  check_DF(facebook_data_head,
          c("total_engagement_percentage", "page_engagement_percentage", "share_percentage", "comment_percentage"),
          "11946e7a3ed5e1776e81c0f0ecd383d0",
          c("total_engagement_percentage", "page_engagement_percentage", "share_percentage", "comment_percentage"),
          c(10,10,10,10),
          c("727b6cd45f0340de38d1cfe8403adb3e",
            "770c9b629f9fed7198967988a0e61b19",
            "852d15f063d8b09281dbefd5e4631cd9",
            "cc18eca3d186eb8772b9768990dcd2ea"
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
    expect_equal(digest(as.integer(answer_as_numeric)), "a026d6d420e9adc250cb3d47fed00e84")
  })
  
  print("Success!")
}

# Question 2.2

test_2.2 <- function() {
  check_DF(facebook_data_stats, 
           c("key", "mean", "sd", "max", "min"),
           '234a2a5581872457b9fe1187d1616b13',
           c("mean", "sd", "max", "min"),
           c(1e4,1e4,10,10),
           c("ab05a05055f2f5f6e583cc8f2c0bd3d9",
             "0914892912f1b3be5d4f1030d901e773",
             "f4d8409b0355a3d794f3b5a38187f8d1",
             "7eeda5fe3e5d82c2168536f9459170dd"))
  
}

# +
# Question 2.7

test_2.7 <- function() {
    test_that(paste('Did not assign answer to an object called "facebook_pair_plots"', ""), {
        expect_true(exists('facebook_pair_plots'))
    })
    
    test_that("Solution should be a ggplot object", {
        expect_true('ggmatrix' %in% class(facebook_pair_plots))
    })
    
    test_that("facebook_pair_plots$data should be a dataframe object", {
        expect_true('data.frame' %in% class(facebook_pair_plots$data))
    })
    
    given_colnames <- colnames(facebook_pair_plots$data)
    expected_colnames <- c('total_engagement_percentage','page_engagement_percentage','share_percentage','comment_percentage')
    
    test_that("facebook_pair_plots$data does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
    test_that("Incorrect facebook_pair_plots$data", {
        expect_equal(digest(as.integer(sum(facebook_pair_plots$data[,1]) * 1e4)), 'b4fbd83c73486b97bed1112466c99f84')
    })
    
    print("Success!")
}
# -

# Question 2.8
test_2.8 <- function() {
    check_MC(answer2.8, LETTERS[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# Question 3.0
test_3.0 <- function() {
  check_MC(answer3.0, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}


# +
# Question 3.1
test_3.1.0 <- function() {
    check_MC(answer3.1.0, c('total_engagement_percentage',
                            "page_engagement_percentage"),
             '43f33097b14c863532a59c4455ce0920')
}

test_3.1.1 <- function() {
    check_MC(answer3.1.1, c('total_engagement_percentage',
                            "page_engagement_percentage"), 'e7745c976ab233122190edd27a94cefc')
}

# +
# Question 3.2


test_3.2 <- function() {
    check_numeric_element(facebook_SLR$coefficients[1], 1e4, "82a8e025a7949fb6c40a567e6a3d2f1f")
    check_numeric_element(facebook_SLR$coefficients[2], 1e4, "07ae3649e2376d43b4b8b544086e6481")
}

# +
# Question 3.3

test_3.3 <- function() {
    check_DF(facebook_SLR_results,
            c("term", "estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("b3f52af93509f5f31091c155fe25b6d0",
               "cac17b80df37171f02a533a0962e81ec",
               "2569848fabd00ccf0ee02d0cece019f9",
               "234a2a5581872457b9fe1187d1616b13",
               "025e3a01f7b9d117f15433ab1a11c916",
               "bd15e8575c605a44f323d2c67cbfd7f1"))
}

# +
# Question 3.4

test_3.4 <- function() {
    check_plot(facebook_scatterplot, "page_engagement_percentage", "GeomPoint", F,
               "3e2e4a08c44d0224de5b7e668c75ace3", 'a026d6d420e9adc250cb3d47fed00e84', 
               "d4847fc7ad8586ea2772cffffb6758d5", F)
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
             c(10000, 10000),
             c("419c13ecc6b25ea09283dadbe65ac8c8",
               "51574d3615d6cb1deb002ac47103aeda"))
    
}

# +
# Question 4.5

test_4.5 <- function() {
    check_plot(slope_sampling_dist_boot, "boot_slope",
               "GeomBar", F, "3e2e4a08c44d0224de5b7e668c75ace3", "b6a6227038bf9be67533a45a6511cc7e",
               "a38f647b06f430773f9bdc8263735d5e", T)
}

# +
# Question 4.6

test_4.6 <- function() {
    check_plot(slope_sampling_dist_boot_limits, "boot_slope",
               "GeomBar", T, "3e2e4a08c44d0224de5b7e668c75ace3", "b6a6227038bf9be67533a45a6511cc7e",
               "a38f647b06f430773f9bdc8263735d5e", T)
}
