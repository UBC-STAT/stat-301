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
    check_TF(answer1.0, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 1.1

test_1.1 <- function() {
    check_TF(answer1.1, 'd2a90307aac5ae8d0ef58e2fe730d38b')
}

# +
# Question 1.2

test_1.2 <- function() {
    check_TF(answer1.2, 'd2a90307aac5ae8d0ef58e2fe730d38b')
}

# +
# Question 1.3

test_1.3 <- function() {
    check_TF(answer1.3, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 1.4

test_1.4 <- function() {
    check_TF(answer1.4, '05ca18b596514af73f6880309a21b5dd')
}

# +
# Question 2.0

test_2.0.0 <- function() {
    check_numeric_element(SLR_poverty$coefficients[1], 1e4, "0a0353180ea5fef26849bc601d20a2f1")
    check_numeric_element(SLR_poverty$coefficients[2], 1e4, '5846f7dc814818c1166cdc301b8dfece')
}
test_2.0.1 <- function() {
    check_numeric_element(SLR_coverage$coefficients[1], 1e4, '67524902b593be58f32fec8682055286')
    check_numeric_element(SLR_coverage$coefficients[2], 1e4, 'f5d8de261024643b608ac72e31093996')
}
test_2.0.2 <- function() {
    check_numeric_element(MLR_poverty_coverage$coefficients[1], 1e4, 'bdbb8adc85440f1cf00cd95a035e3ecd')
    check_numeric_element(MLR_poverty_coverage$coefficients[2], 1e4, '39fed44eafefb888dd51e20fac6969a7')
}





# +
# Question 2.1

test_2.1.0 <- function() {
    check_numeric(SLR_coverage_coef, 100, '7bb0a3105068119405ee360c2bfddbfe') 
}


test_2.1.1 <- function() {
    check_numeric(MLR_coverage_coef, 100, '5da8c181bb8d1ad30f337090304a9f20') 
}
    
    

# +
# Question 2.2


test_2.2.0 <- function() {
    check_numeric(answer2.2.0, 100, '7bb0a3105068119405ee360c2bfddbfe') 
}


test_2.2.1 <- function() {
    check_numeric(answer2.2.1, 100, '5da8c181bb8d1ad30f337090304a9f20') 
}
    

# +
# Question 2.3

test_2.3 <- function() {
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
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, LETTERS[1:4], 'd110f00cfb1b248e835137025804a23b')
}

# +
# Question 3.0

test_3.0 <- function() {
  var_name <- deparse(substitute(read_grades_boxplots))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(read_grades_boxplots))
  })
  
  properties <- c(read_grades_boxplots$layers[[1]]$mapping, read_grades_boxplots$mapping)
  
  test_that(paste("Plot should have ", x_axis_var," on the x-axis"), {
    expect_true(x_axis_var == rlang::get_expr(properties$x))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true('GeomBoxplot' %in% class(read_grades_boxplots$layers[[1]]$geom))

  })
  
  test_that("Plot does not have the correct bin width", {
    expect_equal(
      digest(as.integer(mget("stat_params", read_grades_boxplots$layers[[1]])[["stat_params"]][["binwidth"]])),
      "3e2e4a08c44d0224de5b7e668c75ace3")
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(read_grades_boxplots$data)), "d6af036ffbdd7ccfc34dc7862b0e50d3")
    expect_equal(digest(round(sum(read_grades_boxplots$data['english']))), '561551836159fd71390dedbdb24b2ba4')
    
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(read_grades_boxplots$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(read_grades_boxplots$labels$x == toString(rlang::get_expr(properties$x)))
  })
  

    test_that("Plot should have a title", {
      expect_true("title" %in% names(read_grades_boxplots$labels))
    })
  
  
  print("Success!")
}



# +
# Question 3.1


test_3.1.0 <- function() {
    check_numeric_element(read_grades_LR$coefficients[1], 1000, '90887ececd638605182079009ae19ac5') 
}


test_3.1.1 <- function() {
    check_numeric_element(read_grades_LR$coefficients[2], 1000, 'a4d02d58f24887f1dd5e74b211dab682') 
}
    

# +
# Question 3.2

test_3.2 <- function() {
    check_DF(read_grades_LR_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100,   1, 100, 100),
             c("efa201995672b5a767174c7f922664f3",
               "515762df41219fc81d41594e90cfaa2a",
               "47c5c72767b144d34e9318d360824d27",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "240d16b7381e7d8fb899a5466cf754ae",
               "961013eaecb76e3382ca63257d0ff194"))
}

# +
# Question 3.3

test_3.3 <- function() {
    check_MC(answer3.3, LETTERS[1:6], '7279bb4184f9c53d42729c6eb22db36a')
}

# +
# Question 3.4

test_3.4 <- function() {
    check_MC(answer3.4, LETTERS[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 3.5

#string
test_3.5.0 <- function() {
    var_name <- deparse(substitute(answer3.5.0))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  answer_as_lower <- tolower(answer3.5.0)
  test_that(paste(var_name, " should be a string"), {
    expect_true(is.character(answer_as_lower))
  })
  
  test_that(paste(var_name, " value is incorrect"), {
    expect_equal(digest(answer_as_lower), "5dca5f1942c9cd0a3f81cae37889b391")
  })
  
  print("Success!")
}

#MC

test_3.5.1 <- function() {
    check_MC(answer3.5.1, LETTERS[1:3],'127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 4.0

test_4.0 <- function() {
    check_DF(CASchools_MLR_Add_results,
            c("term","estimate", "std.error", "statistic","p.value",   "conf.low",  "conf.high"),
             "11946e7a3ed5e1776e81c0f0ecd383d0",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100,   1, 100, 100),
             c("7bdd7ff2ae2be4f8fea0f638704d4b0e",
               "2af1eb6f6547dd44db0c64a756f42079",
               "8253a52e94fcf08cc9d4aa86d2a0fa7c",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "65dd83fee5292402b2ec5217dd0baf25",
               "7cb851a3dda4da4efdaa1702c96f3d93"))
}

# +
# Question 4.1

test_4.1 <- function() {
    check_MC(answer4.1, getPermutations(LETTERS[1:4]), '8b63e49106226d2a45958a7e24c97c37')
}

# +
# Question 4.2

test_4.2 <- function() {
    check_plot(CASchools_MLR_Add_plot,
              "english",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "d6af036ffbdd7ccfc34dc7862b0e50d3",
               "561551836159fd71390dedbdb24b2ba4",
               TRUE)
}

# +
# Question 4.3

test_4.3 <- function() {
    check_MC(answer4.3, LETTERS[1:3], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 4.4

test_4.4 <- function() {
    check_DF(CASchools_MLR_Int_results,
            c("term","estimate", "std.error", "statistic", "p.value",   "conf.low", "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("a9770517f8980b0834b7b3a104724ce8",
               "2c76b47a6643b2417d172d761b1aff19",
               "911ebe5ca22a4c68ba1d579d3b8194cf",
               "25e6a154090e35101d7678d6f034353a",
               "46919dd400eadfc6eedfdae02ba9b096",
               "8cf87c6e8619efb99e4d5488bbba973f"))
}

# +
# Question 4.5


test_4.5 <- function() {
    check_MC(answer4.5, getPermutations(LETTERS[1:6]), '93e311a221d93a15962a246827baabdc')
}

# +
# Question 4.6

test_4.6 <- function() {
    check_plot(CASchools_MLR_Int_plot,
              "english",
               "GeomPoint",       
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "d6af036ffbdd7ccfc34dc7862b0e50d3",
               "561551836159fd71390dedbdb24b2ba4",
               TRUE)
}

# +
# Question 4.7

test_4.7 <- function() {
    check_MC(answer4.7, LETTERS[1:3], 'ddf100612805359cd81fdc5ce3b9fbba')
}
