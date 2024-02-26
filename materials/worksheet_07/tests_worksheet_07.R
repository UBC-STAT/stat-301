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
  test_that('Did not assign answer to an object called "SLR_ENSG00000085733_plot"', {
    expect_true(exists("SLR_ENSG00000085733_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(SLR_ENSG00000085733_plot))
  })

  properties <- c(SLR_ENSG00000085733_plot$layers[[1]]$mapping, SLR_ENSG00000085733_plot$mapping)

  test_that("Plot should have mrna on the x-axis", {
    expect_true("mrna" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(SLR_ENSG00000085733_plot$layers[[1]]$geom))

   })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", SLR_ENSG00000085733_plot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(SLR_ENSG00000085733_plot$data)), "fa5a4df7ac0f9782037da890557fd8b8")
    expect_equal(digest(round(sum(SLR_ENSG00000085733_plot$data$mrna))), "908d1fd10b357ed0ceaaec823abf81bc")

  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(SLR_ENSG00000085733_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(SLR_ENSG00000085733_plot$labels))
  })

  print("Success!")

}
# -
# Question 1.1

test_1.1.0 <- function() {
    check_numeric_element(SLR_ENSG00000085733$coefficients[1], 1e6, "25e6a154090e35101d7678d6f034353a")
    check_numeric_element(SLR_ENSG00000085733$coefficients[2], 1e5, "a03a4514784b3aff814a8aa7ab343d16")
}
test_1.1.1 <- function() {
  check_DF(SLR_ENSG00000085733_results,
           c("term","estimate",  "std.error", "statistic", "p.value" ),
           "c01f179e4b57ab8bd9de309e6d576c48",
           c("estimate",  "std.error", "statistic", "p.value" ),
           c(1e6,1e6,100 ,10),
           c("2f9f1c2afd9a90172d6e15a009de84b2",
             "79ba185f068a5011ce65ec801fd55276",
             "a7f7cec46d9e1398c6bb0b72351f3eab",
             "fa5a4df7ac0f9782037da890557fd8b8"
           ))
}

test_1.1.2 <- function() {
  check_DF(dat_ENSG00000085733,
           c(".rownames","prot","mrna",".fitted",".resid",".hat",".sigma",".cooksd",".std.resid"),
           "fa5a4df7ac0f9782037da890557fd8b8",
           c(".fitted",".resid"),
           c(1e6,1e6),
           c("a95ceee8390cb47bb05410a8d23c76cf",
             "1473d70e5646a26de3c52aa1abd85b1f"
           ))
}

test_1.2.0 <- function() {
  check_DF(SLR_ENSG00000085733_gof,
           c("r.squared","adj.r.squared","sigma","statistic","p.value","df","logLik","AIC","BIC","deviance","df.residual","nobs"),
           "4b5630ee914e848e8d07221556b0a2fb",
           c("r.squared"),
           c(1e3),
           c("6cb0c41728b80235d1532f88c59ce9e6"
           ))
}

test_1.2.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.2.1"', {
    expect_true(exists("answer1.2.1"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer1.2.1, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.2.1))
 
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success!")
}


test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer1.3, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.3))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer1.4, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.4))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "257ffd0fec52cbf9ff2dba41f66d6b26")
  })
  
  print("Success!")
}


test_1.5 <- function() {
  test_that('Did not assign answer to an object called "answer1.5"', {
    expect_true(exists("answer1.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.5, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.5))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "5569ed730134a270952e469c2be78612")
  })
  
  print("Success!")
}

#---------------------

# Question 2.0


test_2.0 <- function() {
  check_DF(summary_gof,
           c("gene","r.squared", "p.value"),
           "a1af0464b4260e8c6044ddbf20d5607f",
           c("r.squared", "p.value"),
           c(1000, 1e6),
           c("9b3991f9ae292cb9a49b18824cef818e",
            "4ff574bd8b88dcca0333d28e927ee31a"))
}

#--

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer2.1, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.1))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

#--

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "hist_slr_F"', {
    expect_true(exists("hist_slr_F"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(hist_slr_F))
  })

  properties <- c(hist_slr_F$layers[[1]]$mapping, hist_slr_F$mapping)

  test_that("Plot should have p.value on the x-axis", {
    expect_true("p.value" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(hist_slr_F$layers[[1]]$geom))

  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", hist_slr_F$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(hist_slr_F$data)), "a1af0464b4260e8c6044ddbf20d5607f")
    expect_equal(digest(round(sum(hist_slr_F$data$p.value))), "12923c0c32abd92b47a44187f7d6fb75")

  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(hist_slr_F$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(hist_slr_F$labels))
  })

  print("Success!")
}

#----

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "answer2.3"', {
    expect_true(exists("answer2.3"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.3, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.3))
 
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })
  
  print("Success!")
}

#-----
#Question 3.0


test_3.0 <- function() {
  test_that('"model.1" is incorrect', {
    expect_equal(digest(tolower(model.1)), "d110f00cfb1b248e835137025804a23b")
  })

  test_that('"model.2" is incorrect', {
    expect_equal(digest(tolower(model.2)), "93a9078c6326f37b481d3e99b60ad987")
  })

  test_that('"model.3" is incorrect', {
    expect_equal(digest(tolower(model.3)), "ddf100612805359cd81fdc5ce3b9fbba")
  })

  test_that('"model.4" is incorrect', {
    expect_equal(digest(tolower(model.4)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  test_that('"model.5" is incorrect', {
    expect_equal(digest(tolower(model.5)), "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

#---
test_3.1.0 <- function() {
    check_DF(mlr_3genes_int_results,
             c("term","estimate",  "std.error", "statistic", "p.value"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100,100,100,100),
             c('7c7124efff5c7039a1b1e7cba65c5379',
               'e9f8d19c99ed86518e645170bc3facd7',
               'b84519040eec75b640a1cce9c7f2be51',
               'ff9690743156cc5cd42197f6dba7e5a1'))
}

test_3.1.1 <- function() {
  check_DF(mlr_3genes_int_gof,
           c("r.squared","adj.r.squared","sigma","statistic","p.value","df","logLik","AIC","BIC","deviance","df.residual","nobs"),
           "4b5630ee914e848e8d07221556b0a2fb",
           c("r.squared"),
           c(1e3),
           c("40b423b6494baa72a66fe2fdd02b15c4"
           ))
}    

#--

test_3.2  <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D","E")', {
    expect_match(answer3.2, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.2))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })
  
  print("Success!")
}

#----
#Question 3.3

test_3.3 <- function() {
  check_numeric_element(Ftest_3genes_interaction[2,5], 
                        100, 
                        "1a825441b9be399cbd0568b97e8902de")
}


#-----
test_3.7 <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer3.2, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.2))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

test_3.3.0 <- function() {
    check_DF(mlr_3genes_int_results,
             c("term","estimate",  "std.error", "statistic", "p.value"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100,100,100,100),
             c('7c7124efff5c7039a1b1e7cba65c5379',
               'e9f8d19c99ed86518e645170bc3facd7',
               'b84519040eec75b640a1cce9c7f2be51',
               'ff9690743156cc5cd42197f6dba7e5a1'))
}

test_3.3.1 <- function() {
  check_DF(mlr_3genes_int_gof,
           c("r.squared","adj.r.squared","sigma","statistic","p.value","df","logLik","AIC","BIC","deviance","df.residual","nobs"),
           "4b5630ee914e848e8d07221556b0a2fb",
           c("r.squared"),
           c(1e3),
           c("40b423b6494baa72a66fe2fdd02b15c4"
           ))
}    

#---
#Question 3.4

test_3.4  <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C")', {
    expect_match(answer3.4, "a|b|c", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.4))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}


#--
#Question 3.5

test_3.5  <- function() {
  test_that('Did not assign answer to an object called "answer3.5"', {
    expect_true(exists("answer3.5"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D","E")', {
    expect_match(answer3.5, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.5))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}

#----

#Question 3.6

test_3.6  <- function() {
  test_that('Did not assign answer to an object called "answer3.6"', {
    expect_true(exists("answer3.6"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D","E")', {
    expect_match(answer3.6, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.6))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })
  
  print("Success!")
}


#--

#Question 3.7

test_3.7 <- function() {
  check_numeric_element(Ftest_3genes_mrna$F[2], 
                        100, 
                        "e6c63b6571d445697f2c2e2089fe7fa4")
}




#--




