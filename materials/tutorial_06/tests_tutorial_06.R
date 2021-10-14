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
    check_DF(US_county_sample,
            c("incidenceRate",      "povertyPercent",     "MedianAge", "PctPrivateCoverage", "medIncome",   "TARGET_deathRate"),  
             "906679c1f1b0a7c8e05a254c9e04f3d2",
             c("incidenceRate",      "povertyPercent",     "MedianAge", "PctPrivateCoverage", "medIncome",   "TARGET_deathRate"),
             c(10, 10, 10, 10,  1, 10),
             c("bcb1399be0cd13aaf73fa7998bbd4593",
               "74a13bfa30b13f832c8a3251817c877e",
               "6bef2ffd745d05a360e90bbe5bd9426e",
               "1d903f0b9aeba16b595d8c4fdbe702c6",
               "a4dc1046ae299230d7dc4a2237cce9ed",
               "9d51261a96864ae531c54fdca637d4f8"))
}

# +
# Question 1.1


test_1.1 <- function() {
    temp <<- US_cancer_pair_plots[1,1]
    check_plot(temp, 
              "incidenceRate",
               "GeomDensity",         
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "906679c1f1b0a7c8e05a254c9e04f3d2",
               "c2fb1b26e1860283638339fe2dfbfd72",
               FALSE,
               FALSE,
              FALSE)
}

# +
# Question 1.3

test_1.3 <- function() {
    check_DF(corr_matrix_US_county_sample,
            c("var1", "var2", "corr"),
            "5462ea21bef8d27d5a0ea4da35939549",
             c("corr"),
             c(10000),
             c("ed84bd7f5a9906187d765d06a0f1d1dc"))
}

# +
# Question 1.4

test_1.4 <- function() {
    check_plot(plot_corr_matrix_US_county_sample,
              "var1",
               "GeomTile",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "5462ea21bef8d27d5a0ea4da35939549",
               "none",
               FALSE,
               TRUE,
               TRUE)
}

# +
# Question 1.5

test_1.5 <- function() {
    check_DF(MLR_US_county_sample_results,
            c("term", "estimate",  "std.error", "statistic", "p.value"),  
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value"),  
             c(100, 100, 100,   1),
             c("ba0618d134ad32c7e0ec223a0f312a6e",
               "56a0d0cf7a4e74eadd007d97d5db870f",
               "a41b647a14a44bef71795cf43cbc559f",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}

# +
# Question 1.6


test_1.6 <- function() {
    
    check_numeric_element(VIF_MLR_US_county_sample[1],1e3,"d01a31e7cfb4ffb07e32981ad2eec709")
}

# +
# Question 1.7

test_1.7.0 <- function() {
    check_MC(answer1.7.0, getPermutations(LETTERS[1:7]),'257ffd0fec52cbf9ff2dba41f66d6b26')
}


test_1.7.1 <- function() {
    check_MC(answer1.7.1, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.8

test_1.8 <- function() {
    check_DF(red_MLR_US_county_sample_results,
            c("term", "estimate",  "std.error", "statistic", "p.value"),  
             "dd4ad37ee474732a009111e3456e7ed7",
             c("estimate",  "std.error", "statistic", "p.value"),  
             c(100, 100, 100,   1),
             c("9c2f525ed74b5b35e98a8d99382d8f0a",
               "2c22fec5caf20a879854853d0ae62845",
               "252416c512331ce8888806c587a41eb2",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}

# +
# Question 1.9


test_1.9 <- function() {
    
    check_numeric_element(VIF_red_MLR_US_county_sample[1],1e3,"863b97e3315dce945bbba5a5a80046ec")
}

# +
# Question 1.10

test_1.10 <- function() {
    check_MC(answer1.10, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 2.0

test_2.0 <- function() {
    check_DF(overall_UCBAdmissions.prop.summary,
            c("Gender", "Admit", "n", "proportion"),
             "234a2a5581872457b9fe1187d1616b13",
             c("proportion"),
             c(100),
             c("2567f3d5adc004a73dc268884026f3bd"))
}

# +
# Question 2.1

test_2.1 <- function() {
    check_plot(overall_UCBAdmissions.stacked.bars,
              "Gender",
               "GeomBar",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "234a2a5581872457b9fe1187d1616b13",
               "none",
               TRUE,
               TRUE,
               FALSE)
}

# +
# Question 2.3


test_2.3 <- function() {
    check_DF(overall_cont_table_Admit_vs_Gender,
            c("Admit", "Male", "Female"),
            "c01f179e4b57ab8bd9de309e6d576c48",
             c("Male" ,  "Female"),
             c(1, 1),
             c("dc8f8823950e3f4d794e77994001579e",
             "5c4676af1afff1f8ab63796858782759"))
}

# +
# Question 2.4

test_2.4 <- function() {
    check_DF(overall_chisq_test_Admit_vs_Gender,
            c("statistic", "p.value", "parameter", "method"),
            "4b5630ee914e848e8d07221556b0a2fb",
             c("statistic", "p.value", "parameter"),
             c(10000, 10000,1),
            c("43a098191f4405deb634250233525a85",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "4b5630ee914e848e8d07221556b0a2fb"))
}

# +
# Question 2.6

test_2.6 <- function() {
    check_DF(dept_UCBAdmissions.prop.summary.Dept,
            c("Dept", "Gender", "Admit", "n", "proportion"),
             "f92eebebcfea9ebd99a68de2cb409133",
             c("n",          "proportion"),
             c(1, 100),
             c("9797783aefe1c078aeaeb86beffcf723",
               "17c84e8091d697bfd7cf96ccc1413dbd"))
}

# +
# Question 2.7

test_2.7 <- function() {
    check_plot(dept_UCBAdmissions.stacked.bars,
              "Gender",
               "GeomBar",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "f92eebebcfea9ebd99a68de2cb409133",
               "none",
               TRUE,
               TRUE,
               FALSE)
}

# +
# Question 2.9

test_2.9 <- function() {
    check_DF(dept_chisq_test_Admit_vs_Gender,
            c("Dept","test_statistic", "p.value",        "adj_p.value"), 
             "25e6a154090e35101d7678d6f034353a",
             c("test_statistic", "p.value",        "adj_p.value"),   
             c(10, 10000, 10000),
             c("9e13926505f026ba03c475b3a83f7a04",
               "a6be6525f0e48dedff6b0363106c0138",
               "e26bea788695e5e1709e13c8c8e2f609"))
}
