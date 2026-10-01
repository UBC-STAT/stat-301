library(digest)
library(testthat)

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

# getPermutations <- function(vec) {
#     rsf <- c()
#     for (i in 1:length(vec)) {
#         for (j in i:length(vec)) {
#             temp <- vec[i:j] 
#             rsf <- c(rsf, paste(temp, collapse= ''))
#             if (i < j) {
#                 for (k in i:j) {
#                     rsf <- c(rsf, paste(temp[-k], collapse=''))
#                 }
#             }
#         }
#     }
#     return(unique(rsf))
# }

getPermutations <- function(vec) {
temp <- do.call(c, lapply(seq_along(vec), combn, x = vec, simplify = FALSE))
rsf <-c()
n <- (2^(length(vec)))-1    
for(i in 1:n){
rsf <- c(rsf,paste(unlist(temp[i]),collapse=''))
    }
return(unique(rsf))    
    }


#-----
test_1.1 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_student"', {
    expect_true(exists("default_logistic_student"))
  })
  
  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(default_logistic_student))
  })
  
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_student$residuals) * 10e4)), 
                 "1473d70e5646a26de3c52aa1abd85b1f")
  })
  
  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.2))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}


test_1.3 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_multiple"', {
    expect_true(exists("default_logistic_multiple"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(default_logistic_multiple))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_multiple$residuals) * 10e4)), "4cd0d70c6d524273008afa4703cd9df0")
  })

  print("Success!")
}

test_1.4 <- function() {
    check_MC(answer1.4, getPermutations(LETTERS[1:6]), 'bd9d5566bbeb4f989e69549f22047d8a')
}

#-----

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer1.5, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.5))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}


test_2.1 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_multiple_results"', {
    expect_true(exists("default_logistic_multiple_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(default_logistic_multiple_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')


  given_colnames <- colnames(default_logistic_multiple_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(default_logistic_multiple_results))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_multiple_results$conf.low) * 10e6)), "ba24456096c2af163742607b3e494ef7")
    expect_equal(digest(as.integer(sum(default_logistic_multiple_results$conf.high) * 10e6)), "f2c22cf099c18a563f722da22c975d80")
    expect_equal(digest(as.integer(sum(default_logistic_multiple_results$statistic) * 10e6)), "273c6d0ba411836667be62df7641a7b4")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_odds_results"', {
    expect_true(exists("default_logistic_odds_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(default_logistic_odds_results))
  })

  expected_colnames <- c('term','estimate','std.error','statistic','p.value','conf.low','conf.high')

  given_colnames <- colnames(default_logistic_odds_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(default_logistic_odds_results))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_odds_results$estimate) * 10e6)), "7a4bb9d19684e603bffc6f1961a7eda1")
    expect_equal(digest(as.integer(sum(default_logistic_odds_results$conf.low) * 10e6)), "55679ab470e07149e5a38ce2213a15df")
    expect_equal(digest(as.integer(sum(default_logistic_odds_results$conf.high) * 10e6)), "bdccf6329a8314620c8305cc31634021")
  })

  print("Success!")
}

#-----------
test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })

  answer_hash <- digest(tolower(answer2.3))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "8b63e49106226d2a45958a7e24c97c37")
  })

  print("Success!")
}


test_3.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1"', {
    expect_true(exists("answer3.1"))
  })

  answer_as_numeric <- as.numeric(answer3.1)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "7ea6059985f925469f09bb3af79c7dc1")
  })

  print("Success!")
}

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })

  answer_as_numeric <- as.numeric(answer3.2)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "26b7f981cf15e1b5f55fa6a141d8a3a0")
  })

  print("Success!")
}

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_fitted"', {
    expect_true(exists("default_logistic_fitted"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(default_logistic_fitted))
  })


  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(default_logistic_fitted))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_fitted$pred_prob)/100)), "11946e7a3ed5e1776e81c0f0ecd383d0")
    expect_equal(digest(as.integer(sum(default_logistic_fitted$pred_line)/10e3)), "a9a8883dac7a645a24f4ed180029e4a9")
    expect_equal(digest(as.integer(sum(default_logistic_fitted$pred_odds)/100)), "9d08099943f8627959cfb8ecee0d2f5d")
  })

  print("Success!")
}

test_3.4 <- function() {
    check_MC(answer3.4, getPermutations(LETTERS[1:6]), 'ec8a0a5a3680bfac4d17f8072e092200')
}

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "default_logistic_residuals"', {
    expect_true(exists("default_logistic_residuals"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(default_logistic_residuals))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(default_logistic_residuals))), "f1a30baa3072c1aad822c059f35c6841")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(default_logistic_residuals$resid_raw)* 10e6)), "50179256c4c016dcd020c8b03c02464c")
    expect_equal(digest(as.integer(sum(default_logistic_residuals$raw_byhand)* 10e6)), "50179256c4c016dcd020c8b03c02464c")
    expect_equal(digest(as.integer(sum(default_logistic_residuals$resid_pearson)* 10e6)), "634ade317778bf47f60e8983e0e90e26")
    expect_equal(digest(as.integer(sum(default_logistic_residuals$pearson_byhand)* 10e6)), "634ade317778bf47f60e8983e0e90e26")
  })

  print("Success!")
}

test_4.2 <- function() {
    check_MC(answer4.2, getPermutations(LETTERS[1:5]), '93e311a221d93a15962a246827baabdc')
}

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "residual_plot"', {
    expect_true(exists("residual_plot"))
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(round(sum(x_fit)/100)), "e5b57f323c7b3719bbaaf9f96b260d39")
    expect_equal(digest(round(sum(y_resid))), "b01ec9609569aecde321d7dd2674f6e1")
  })   

  print("Success!")
}