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




get_plot_params <- function(answerX.X, isFactor) {
  
  properties <- c(answerX.X$layers[[1]]$mapping, answerX.X$mapping)
  x_axis_var <- as.character(rlang::get_expr(properties$x))
  print(x_axis_var)
  print(class(answerX.X$layers[[1]]$geom))
  if (length(answerX.X$layers) > 1) {
    print("GeomVline" %in% class(answerX.X$layers[[2]]$geom))
  } else {
    print(FALSE)
  }
  print(digest(as.integer(mget("stat_params", answerX.X$layers[[1]])[["stat_params"]][["binwidth"]])))
  print(digest(nrow(answerX.X$data)))
  if(!isFactor) {
    print(digest(round(sum(pull(answerX.X$data, x_axis_var)))))
  } else {
    print("none")
  }
  print("title" %in% names(answerX.X$labels))
  print(isFactor)
  print(answerX.X$labels$x != toString(rlang::get_expr(properties$x)))
}


get_DF_params <- function(df, precisionList) {
  cols <- colnames(df)
  print(cols)
  print(digest(as.integer(nrow(df))))
  print(cols)
  print(precisionList)
  for (i in 1: length(precisionList)) {
    print(digest(as.integer(sum(df[cols[i]],na.rm=T) * as.double(precisionList[[i]]))))
  }
  
}


get_DF_params_overflow <- function(df, precisionList) {
  cols <- colnames(df)
  print(cols)
  print(digest(as.integer(nrow(df))))
  print(cols)
  print(precisionList)
  for (i in 1: length(precisionList)) {
    print(digest(trunc(sum(df[cols[i]],na.rm=T) * as.double(precisionList[[i]]))))
  }
  
}


get_numeric_element_params <- function(answerX.X, precision) {
  print(precision)
  answer_as_numeric <- as.numeric(answerX.X)
  print(digest(as.integer(answer_as_numeric * precision)))
}
# -


# Question 1.0

test_1.0 <- function() {
  check_DF(dat_ss,
           c("gene","RSS","TSS", "myR2"),
           "a1af0464b4260e8c6044ddbf20d5607f",
           c("RSS","TSS", "myR2"),
           c(1000,1000,1000),
           c("4e8a0ea7b74d4bf3c9423eead25cfbd9",
             "9564c75e11688d755ea80e5758c6a0ef",
             "9b3991f9ae292cb9a49b18824cef818e"))
}

#--

test_1.1 <- function() {
  check_DF(dat_r2,
           c("gene","RSS","TSS", "myR2","r.squared"),
           "a1af0464b4260e8c6044ddbf20d5607f",
           c("RSS","TSS", "myR2","r.squared"),
           c(1000,1000,1000,1000),
           c("4e8a0ea7b74d4bf3c9423eead25cfbd9",
             "9564c75e11688d755ea80e5758c6a0ef",
             "9b3991f9ae292cb9a49b18824cef818e",
             "9b3991f9ae292cb9a49b18824cef818e"))
}

#--

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.2))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

#--

test_1.3 <- function() {
    check_DF(mlr_3genes_add_res,
             c("term","estimate",  "std.error", "statistic", "p.value"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100,100,100,100),
             c('3e98cd61c0f0313ba5b0b2fa1a37a0ec',
               'ade76ff25149e0df3a56010f12ea82fd',
               '61a5f7bea3f303f179bfedd59bdc6f52',
               'c6df9ff55bfad3fa7254de0d17b5a7f5'))
}

#--
#Question 1.4

test_1.4  <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D","E")', {
    expect_match(answer1.4, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.4))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}


#--
#Question 1.5

test_1.5 <- function() {
  check_numeric_element(Ftest_3genes_full_reduced$F[2], 
                        100, 
                        "cc18eca3d186eb8772b9768990dcd2ea")
}

#--

#Question 1.6

test_1.6  <- function() {
  test_that('Did not assign answer to an object called "answer1.6"', {
    expect_true(exists("answer1.6"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.6, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.6))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}


#--

#Question 1.7

test_1.7  <- function() {
  test_that('Did not assign answer to an object called "answer1.7"', {
    expect_true(exists("answer1.7"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D","E")', {
    expect_match(answer1.7, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.7))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("You are doing great!")
}


#--
#Question 1.8


test_1.8 <- function() {
  check_numeric_element(Ftest_3genes_interaction$statistic, 
                        100, 
                        "1a825441b9be399cbd0568b97e8902de")
}

#--

#Question 1.9

test_1.9 <- function() {
  check_numeric_element(Ftest_3genes_mrna$F[2], 
                        100, 
                        "e6c63b6571d445697f2c2e2089fe7fa4")
}

#--
test_2.2 <- function() {
    check_plot(Prestige_MLR_Int_plot,
               "income",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "b67babb4a76e89b72ff6f1ad5a4a36fe",
               "379a7c2dcf50387ee28b8ccb0cc5eed7",
               TRUE,
               FALSE,
               TRUE)
}

test_2.3 <- function() {
    check_numeric(TSS_Prestige_MLR_Int, 1e4, 'c07449fc079bbbae871264c13779421d')
    check_numeric(ESS_Prestige_MLR_Int, 1e3, '2df5f8554d588a1e295312f06d0cfa60')
    check_numeric(RSS_Prestige_MLR_Int, 1e4, '88dc22f30f0d084e4ffcf0f6bcc06b27')
    check_numeric(R_Squared_Prestige_MLR_Int, 1e3, 'c3ad708acb2b90a9e40e48f729083e69')
}

test_2.4 <- function() {
    check_DF(Prestige_MLR_Int_statistics,
            c("r.squared","adj.r.squared","sigma", "statistic" ,"p.value",  "df",   "logLik", "AIC", "BIC", "deviance",
              "df.residual", "nobs"),
             "4b5630ee914e848e8d07221556b0a2fb",
             c("r.squared","adj.r.squared","sigma", "statistic" ,"p.value",  "df",   "logLik", "AIC", "BIC", "deviance",
              "df.residual", "nobs"),
             c(1e3,1e3,1e3,1e3,1,1,1e3,1e3,1e3,1e3,1,1),
             c("c3ad708acb2b90a9e40e48f729083e69",
               "d1f3db5ed66a4ecd1c5bf3b456e5606b",
               "caa4da3fe2cad4ebe9b4929a9d486462",
               "0a4d2ccd7065e829f0d69e1711938ec2",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "dd4ad37ee474732a009111e3456e7ed7",
               "9be65057e511e815388785e2e36c06d3",
               "1bd6fc2deef0473947e4c80735dbfb61",
               "e04be09f306875f3914e75c9fc1e2975",
               "4893c25d9cff1549e2da7cb88ab2631b",
               "be38593212fa16b5a5be0a47ad23db30",
               "b67babb4a76e89b72ff6f1ad5a4a36fe"))
}

test_2.5 <- function() {
    check_MC(answer2.5, LETTERS[1:4], "6e7a8c1c098e8817e3df3fd1b21149d1")
}

test_2.6 <- function() {
    check_DF(Prestige_MLR_Add_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
            c(100,100,100,1,100,100),
             c("6a41e6633de0384c3ebb54a54428c5cb",
               "4abb356c7b8460ebf96ff801d6539873",
               "b01217681bd09300e97f1c89cb51af7f",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "d13e9cb371d04b8eaf3bdee67ba555fd",
               "1de3a03b1fd0dc9fba9d54288480165e"))
}

test_2.7 <- function() {
    check_MC(answer2.7, LETTERS[1:5], "d110f00cfb1b248e835137025804a23b")
}

test_2.8 <- function() {
    check_MC(answer2.8, LETTERS[1:4], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
test_2.9 <- function() {
    F_stat <-Prestige_F_test_full_vs_reduced$F[2] 
      test_that(paste('Did not assign answer to an object called ', "Prestige_F_test_full_vs_reduced"), {
    expect_true(exists("Prestige_F_test_full_vs_reduced"))
  })
    answer_as_numeric <- as.numeric(F_stat)
  test_that(paste("Prestige_F_test_full_vs_reduced", " should be a number"), {
    expect_false(is.na(answer_as_numeric))
  })
  
  test_that(paste("Prestige_F_test_full_vs_reduced", " value is incorrect"), {
    expect_equal(digest(as.integer(answer_as_numeric * 1e3)), '3f0f5a0bf2d264ea79d85365e1ebb978')
  })
  
    print("Success! You are doing great :)")
    
    
}
# -

test_2.10 <- function() {
    check_MC(answer2.10,LETTERS[1:4], '127a2ec00989b9f7faf671ed470be7f8')
}

test_2.11 <- function() {
    check_MC(answer2.11, LETTERS[1:4], "d110f00cfb1b248e835137025804a23b")
}

test_3.1 <- function() {
    check_DF(Prestige_MLR_Add_2_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "11946e7a3ed5e1776e81c0f0ecd383d0",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
                         c(100,100,100,1,100,100),
            c("9b7f54fee7f84498a350fb812924ce7b",
              "dbdee65ccaa02541cc02982948c9a7c7",
              "5945aba660bf0361f3dca7aa15d5eafa",
              "1473d70e5646a26de3c52aa1abd85b1f",
              "705584635ef67b6bbbbb88895812242d",
              "af9e929e08e325600c85e4dc006be5bb"
))
}

test_3.2 <- function() {
    check_numeric(add1_check,1e3,"9fbfbf7c89487218998ca16311532b6a")
    check_numeric(add2_check,1e3,"3288a805abf8da6f9537d1915da94463")
}

test_3.3 <- function() {
    check_MC(answer3.3, LETTERS[1:2], "127a2ec00989b9f7faf671ed470be7f8")
}

test_4.1 <- function() {
    check_DF(fwd_summary_df,
            c("RSQ","RSS","ADJ.R2","BIC"),
            "234a2a5581872457b9fe1187d1616b13",
            c("RSQ","RSS","ADJ.R2","BIC"),
            c(1e4,1e4,1e4,1e4),
            c("1405b7f019eecd30e7998e9e9d72bfca",
              "00dc77a87898e2c8740078eecce419dc",
              "3990f139bc76ac12066e883b2b56eb3c",
              "57a2eea9d4c90f8ec20ee06d41a0b9ca"))
    
    check_DF(back_summary_df,
            c("RSQ","RSS","ADJ.R2","BIC"),
            "234a2a5581872457b9fe1187d1616b13",
            c("RSQ","RSS","ADJ.R2","BIC"),
            c(1e4,1e4,1e4,1e4),
            c("1405b7f019eecd30e7998e9e9d72bfca",
              "00dc77a87898e2c8740078eecce419dc",
              "3990f139bc76ac12066e883b2b56eb3c",
              "57a2eea9d4c90f8ec20ee06d41a0b9ca"))
}

test_4.2 <- function() {
    check_MC(answer4.2, LETTERS[1:2], "127a2ec00989b9f7faf671ed470be7f8")
}
