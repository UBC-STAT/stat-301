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
    check_MC(answer1.0, LETTERS[1:3], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.1

test_1.1 <- function() {
    check_MC(answer1.1, LETTERS[1:4], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.2

test_1.2 <- function() {
    check_MC(answer1.2, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.3

test_1.3 <- function() {
    check_MC(answer1.3, LETTERS[1:6], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 1.4

test_1.4.0 <- function() {
    check_numeric_element(caschools_MLR_add$coefficients[1],1e4,"7d5fdc4b1617f1213563cf12b0be1c68")
}

test_1.4.1 <- function() {
    check_numeric_element(caschools_MLR_add$coefficients[2],1e4,"1cf3a32c120bd3f6faddc6ae470fd2b8")
}

test_1.4.2 <- function() {
    check_numeric_element(caschools_MLR_add$coefficients[3],1e4,"8873eb87d376c1c05700be1435ff22dc")
}


# +
# Question 1.5

test_1.5 <- function() {
    check_plot(caschools_MLR_add_plot,
               "income",
               "GeomPoint" ,
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "d6af036ffbdd7ccfc34dc7862b0e50d3",
               "13742f581e47f00fd8beeade76db838c",
               TRUE)
}

# +
# Question 1.6

test_1.6 <- function() {
    check_DF(caschools_MLR_add_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "11946e7a3ed5e1776e81c0f0ecd383d0",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("3f2caceee58bc5e5092ab875c8066a9b",
               "d3c5ba4fef4e46e63e649dc573977aad",
               "6e4a7f58d3c649fe8d8313435565c283",
               "569ddf8dcb5af0cbc7177be6a8b9700d",
               "fd34f0720893972017cb2261e9353fb2",
               "ee86da303eb6b2854286c84802788768"))
}

# +
# Question 1.7

# manual

# +
# Question 1.8

test_1.8 <- function() {
    check_MC(answer1.8, getPermutations(LETTERS[1:4]), "6e7a8c1c098e8817e3df3fd1b21149d1")
}

# +
# Question 2.0

test_2.0 <- function() {
    check_MC(answer2.0, LETTERS[1:6], 'd110f00cfb1b248e835137025804a23b')
}

# +
# Question 2.1

test_2.1.0 <- function() {
    check_numeric_element(caschools_MLR_int$coefficients[1], 1e4, "080bbbf3c10c028f53b1901022933269")
}
test_2.1.1 <- function() {
    check_numeric_element(caschools_MLR_int$coefficients[2], 1e4, "6750c14599866d62ee03d881a88d4737")
}
test_2.1.2 <- function() {
    check_numeric_element(caschools_MLR_int$coefficients[3], 1e4, "9c3c926f355b55c26682a59a66d621e3")
}
test_2.1.3 <- function() {
    check_numeric_element(caschools_MLR_int$coefficients[4], 1e4, "491174b285abb2888e9e602eecfb6037")
}


# +
# Question 2.2

test_2.2 <- function() {
    check_plot(caschools_MLR_int_plot,
               "income",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "d6af036ffbdd7ccfc34dc7862b0e50d3",
               "13742f581e47f00fd8beeade76db838c",
               TRUE)
}

# +
# Question 2.3

test_2.3 <- function() {
    check_DF(caschools_MLR_int_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("8adf1c6033759946124174b1ffecc9d1",
               "2109c9c0db840505d06b32da37f3261e",
               "6f11355413a4aa7c96bf7412868a7e7f",
               "242f3fe311473815db79235ff6708bde",
               "ec9d7c4ea6c09c65a368c4865e500886",
               "8fcc24b94746de0d011202126d17c4e0"))
}

# +
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, getPermutations(LETTERS[1:5]), '8310e591706d1e38cdbfd4e26f17a274')
}

# +
# Question 2.5

# manual

# +
# Quesiton 2.6


test_2.6.0 <- function() {
        check_DF(caschools_SLR_kk06_results,
            c("term","estimate",  "std.error", "statistic", "p.value"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100, 100, 100, 100),
             c("82798d4574cf47d5cb838e4aca470ed8",
               "9a1e47f252c2f2aa62cab1323c75885b",
               "878fe51e9e4a668776d114d06fd00cc9",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}



test_2.6.1 <- function() {
        check_DF(caschools_SLR_kk08_results,
            c("term","estimate",  "std.error", "statistic", "p.value"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100, 100, 100, 100),
             c("8adf1c6033759946124174b1ffecc9d1",
               "525d73b4c5528794642ceb4dd9f987c2",
               "eda26e24431618557efc7037b20d0568",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}


test_2.6.2 <- function() {
        check_DF(caschools_MLR_int_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("8adf1c6033759946124174b1ffecc9d1",
               "2109c9c0db840505d06b32da37f3261e",
               "6f11355413a4aa7c96bf7412868a7e7f",
               "242f3fe311473815db79235ff6708bde",
               "ec9d7c4ea6c09c65a368c4865e500886",
               "8fcc24b94746de0d011202126d17c4e0"))
}

# +
# Question 2.7

# manual
