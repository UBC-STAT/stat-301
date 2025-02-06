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

test_1.0.0 <- function() {
    check_numeric_element(SLR_poverty$coefficients[1], 1e4, "0a0353180ea5fef26849bc601d20a2f1")
    check_numeric_element(SLR_poverty$coefficients[2], 1e4, '5846f7dc814818c1166cdc301b8dfece')
}
test_1.0.1 <- function() {
    check_numeric_element(SLR_coverage$coefficients[1], 1e4, '67524902b593be58f32fec8682055286')
    check_numeric_element(SLR_coverage$coefficients[2], 1e4, 'f5d8de261024643b608ac72e31093996')
}
test_1.0.2 <- function() {
    check_numeric_element(MLR_poverty_coverage$coefficients[1], 1e4, 'bdbb8adc85440f1cf00cd95a035e3ecd')
    check_numeric_element(MLR_poverty_coverage$coefficients[2], 1e4, '39fed44eafefb888dd51e20fac6969a7')
}


# +
# Question 1.1

test_1.1.0 <- function() {
    check_numeric(SLR_coverage_coef, 100, '7bb0a3105068119405ee360c2bfddbfe') 
}


test_1.1.1 <- function() {
    check_numeric(MLR_coverage_coef, 100, '5da8c181bb8d1ad30f337090304a9f20') 
}
    


# +
# Question 1.2


test_1.2.0 <- function() {
    check_numeric(answer1.2.0, 100, '7bb0a3105068119405ee360c2bfddbfe') 
}


test_1.2.1 <- function() {
    check_numeric(answer1.2.1, 100, '5da8c181bb8d1ad30f337090304a9f20') 
}


# +
# Question 1.3

test_1.3 <- function() {
    check_DF(MLR_poverty_coverage_results,
            c("term", "estimate",  "std.error", "statistic", "p.value") ,
             "11946e7a3ed5e1776e81c0f0ecd383d0",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100, 100, 100,   1),
             c("76d0522951f4f8232fe17002ddbb5ac0",
               "bfd2cc5411b34f6f2e658d6561103878",
               "b59cbc76756ab1f757dff43ad07cb44e",
               "1473d70e5646a26de3c52aa1abd85b1f"))
}

# +
# Question 1.4

test_1.4 <- function() {
    check_MC(answer1.4, LETTERS[1:4], 'd110f00cfb1b248e835137025804a23b')
}

# +
# Question 2.0

test_2.0 <- function() {
  var_name <- deparse(substitute(TARGET_deathRate_boxplots))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(TARGET_deathRate_boxplots))
  })
  
  properties <- c(TARGET_deathRate_boxplots$layers[[1]]$mapping, TARGET_deathRate_boxplots$mapping)
  
  test_that(paste("Plot should have ", "state" ," on the x-axis"), {
    expect_true("state" == rlang::get_expr(properties$x))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true('GeomBoxplot' %in% class(TARGET_deathRate_boxplots$layers[[1]]$geom))

  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", TARGET_deathRate_boxplots$layers[[1]])[["stat_params"]][["binwidth"]])),
      "3e2e4a08c44d0224de5b7e668c75ace3")
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(TARGET_deathRate_boxplots$data)), "3850746911bb492c8445964365202c5b")
    expect_equal(digest(round(sum(TARGET_deathRate_boxplots$data['TARGET_deathRate']))), "57942f385bf40eb1fa0134061ee7c766")
    
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(TARGET_deathRate_boxplots$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(TARGET_deathRate_boxplots$labels$x == toString(rlang::get_expr(properties$x)))
  })
  

    test_that("Plot should have a title", {
      expect_true("title" %in% names(TARGET_deathRate_boxplots$labels))
    })
  
  
  print("Success!")
}



# +
# Question 2.1


test_2.1.0 <- function() {
    check_numeric_element(AC_data_LR$coefficients[1], 1000, 'f65f48afd885bc74af59213931bc4fb0') 
}


test_2.1.1 <- function() {
    check_numeric_element(AC_data_LR$coefficients[2], 1000, 'aa9bb98fba657c8c9e94a2fb9e216a17') 
}


# +
# Question 2.2

test_2.2 <- function() {
    check_DF(AC_data_LR_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100,   1, 100, 100),
             c("6fbc7c3acd85fb24861d70b253be4e77",
               "b8b8682ca14fc9e4f8b4feac4a073c93",
               "42fe196fb6a3daca72ccc0c2e88ef394",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "66e348316503214b6169424c7930ac51",
               "d214fe3858a54ef39f5867badefee3e1"))
}

# +
# Question 2.3

test_2.3 <- function() {
    check_MC(answer2.3, LETTERS[1:6], '7279bb4184f9c53d42729c6eb22db36a')
}

# +
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, LETTERS[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 2.5

test_2.5 <- function() {
    check_MC(answer2.5, LETTERS[1:3],'127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 3.0

test_3.0 <- function() {
    check_DF(MLR_state_poverty_add_results,
            c("term","estimate", "std.error", "statistic","p.value",   "conf.low",  "conf.high"),
             "11946e7a3ed5e1776e81c0f0ecd383d0",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100,   1, 100, 100),
             c("793ee7d7a9e013bfa62fb33241c6ff09",
               "34d1a827bfff2aded09acf48e0909a82",
               "1769b8fb785e87de6a8f68872e548ae6",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "6f9a075a51b48770f8a07ba667c0ec26",
               "4c3468b01b467f7466ac7514e170a46b"))
}

# +
# Question 3.1

test_3.1 <- function() {
    check_MC(answer3.1, getPermutations(LETTERS[1:4]), '8b63e49106226d2a45958a7e24c97c37')
}

# +
# Question 3.2

test_3.2 <- function() {
    check_plot(MLR_state_poverty_add_plot,
              "povertyPercent",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "3850746911bb492c8445964365202c5b",
               "bc73694a00134694936e16666bdca620",
               TRUE)
}

# +
# Question 3.3

test_3.3 <- function() {
    check_MC(answer3.3, LETTERS[1:3], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 4.0

test_4.0 <- function() {
    check_DF(MLR_state_poverty_int_results,
            c("term","estimate", "std.error", "statistic", "p.value",   "conf.low", "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("e11acbe519c2514e7d9d4861484ac84d",
               "8c3fee35029f171e0cb02e5aaec40dbe",
               "87189ff3588c21267ca731f35b1bce9a",
               "58f8415e0549358f3130fdf48d04df90",
               "1b4a332c5574bc910b9c9d1c8cb65811",
               "b9ee92f7f702896e4fddff0b5d8bf2cc"))
}

# +
# Question 4.1


test_4.1 <- function() {
    check_MC(answer4.1, getPermutations(LETTERS[1:6]), 'd110f00cfb1b248e835137025804a23b')
}

# +
# Question 4.2

test_4.2 <- function() {
    check_plot(MLR_state_poverty_int_plot,
              "povertyPercent",
               "GeomPoint",       
               FALSE,
              '3e2e4a08c44d0224de5b7e668c75ace3',
              '3850746911bb492c8445964365202c5b',
              'bc73694a00134694936e16666bdca620',
               TRUE)
}

# +
# Question 4.3

test_4.3 <- function() {
    check_MC(answer4.3, LETTERS[1:3], 'ddf100612805359cd81fdc5ce3b9fbba')
}
