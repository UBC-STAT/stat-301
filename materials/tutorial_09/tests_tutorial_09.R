library(digest)
library(testthat)

# +
# template code
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


is_alphabetical <- function(x, scan=function(...)x){
  all(diff(utf8ToInt(scan(,''))%%32)>0)
}


check_MC <- function(answerX.X, choiceList, expectedHash) {
  var_name <- deparse(substitute(answerX.X))
  test_that(paste('Did not assign answer to an object called ', var_name), {
    expect_true(exists(var_name))
  })
  
  
  test_that(paste('Solution should be a single alphabetical combination of ', toString(choiceList)), {
    
    expect_true(is_alphabetical(answerX.X) & (tolower(substr(answerX.X,nchar(answerX.X),nchar(answerX.X))) %in% tolower(choiceList)))
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
    print(deparse(substitute(df)),quote=FALSE)
  cols <- colnames(df)
  print(deparse(cols), quote = FALSE)
  print(digest(as.integer(nrow(df))))
  print(deparse(cols), quote = FALSE)
  print(deparse(precisionList), quote = FALSE)
  rsf <- c()
  for (i in 1: length(precisionList)) {
    rsf <- c(rsf,digest(as.integer(sum(df[cols[i]],na.rm=T) * as.double(precisionList[[i]]))))
  }
  print(deparse(rsf),quote=FALSE)
  
}


get_DF_params_overflow <- function(df, precisionList) {
  cols <- colnames(df)
  print(deparse(cols))
  print(digest(as.integer(nrow(df))))
  print(deparse(cols))
  print(deparse(precisionList))
  rsf <- c()
  for (i in 1: length(precisionList)) {
    rsf <- c(rsf,digest(trunc(sum(df[cols[i]],na.rm=T) * as.double(precisionList[[i]]))))
  }
  print(deparse(rsf))
  
}


get_numeric_element_params <- function(answerX.X, precision) {
  print(precision)
  answer_as_numeric <- as.numeric(answerX.X)
  print(digest(as.integer(answer_as_numeric * precision)))
}

# +
# Question 1.0

test_1.0 <- function() {
    check_numeric_element(SLR_fat$coefficients[1], 1e4,"a5e29b6a37ba57ba2adc38b285b81073")
    check_numeric_element(SLR_fat$coefficients[2], 1e4,"8b96cebcc19dfd12df6d77fa18cb307c")
}


# +
# Question 1.1

test_1.1 <- function() {
    check_DF(fat_cip,
             c("brozek", "weight", "fit", "lwr", "upr"),
 "05a58a4c10324b8b0ef8f6a3c0db4e57",
 c("brozek", "weight", "fit", "lwr", "upr"),
 c(10, 100, 1000, 1000, 1000),
 c("bd2cca783a5be547d527777174b508b8", "e066c1feab7b3ba792c198f3f98fdd0f", 
   "fabbc646cd3383d398fc1a037f9ab8bd", "2619e9ff6e3e61e67e994a082809106c", 
   "3a282fc8f11edc1fc56098d8b96e068a"))
}

# +
# Question 1.3

test_1.3 <- function() {
    check_DF(fat_pi,
             c("brozek", "weight", "fit", "lwr", "upr"),
 "05a58a4c10324b8b0ef8f6a3c0db4e57",
 c("brozek", "weight", "fit", "lwr", "upr"),
 c(10, 100, 1000, 1000, 1000),
 c("bd2cca783a5be547d527777174b508b8", "e066c1feab7b3ba792c198f3f98fdd0f", 
   "fabbc646cd3383d398fc1a037f9ab8bd", "c5f2154de62d1078b4c5796c3499854a", 
   "6312b86aad64ec1eecd7446c942f01ad"))
}

# +
# Question 2.0
                            
test_2.0 <- function() {
    check_DF(training_fat,
            c("brozek", "age", "weight", "height", "adipos",
              "neck", "chest", "abdom", "hip", "thigh", "knee", 
              "ankle", "biceps", "forearm", "wrist"),
 "10ed7b15ca812930d29ea06c2edde7ef",
 c("brozek", "age", "weight", "height"),
 c(10, 1, 100, 100),
 c("37ee509010689796f56d14d2db0400a3", "518b8d2d85113806a4693c85b804cdb1", 
   "455968a44152801cb167157de7d85536", "1eed1135ffda908402ec8f5527c07f09"))
    
    
        check_DF(testing_fat,
            c("brozek", "age", "weight", "height", "adipos",
              "neck", "chest", "abdom", "hip", "thigh", "knee", 
              "ankle", "biceps", "forearm", "wrist"),
 "d56c4f7c4ce276c7e47e1fa52e70d4b6",
 c("adipos", "neck", "chest", "abdom"),
 c(10, 10, 10, 10),
 c("0582f7ac003f2e854d427f0609cdd5eb", "0d39bb97b55d25ead6f7f807c838542e", 
   "4f4bc94700a77c2f211abd3dce17ef7e", "e4bd80d768c875a2eb5bbde6eced7f8b"))
}
                            
# +
# Question 2.1

test_2.1 <- function() {
  check_numeric_element(sum(fat_full_OLS$coefficients),1e4,"45fdb468aa4f799a79f6344a5ba2400d")
}

# -
# Question 2.2
test_2.2 <- function() {
    check_numeric_element(sum(fat_test_pred_full_OLS),1e4,"d241115d1509045f3d8d761bfcbabec0")
}

# +
# Question 2.3

test_2.3 <- function() {
    check_DF(fat_RMSE_models,
             c("Model","RMSE"),
             "4b5630ee914e848e8d07221556b0a2fb",
             "RMSE",
             1e+06,
             "8ad14d3f6a1bd62f6efb3f9cbcf1c6ed")
}

# +
# Question 3.0

test_3.0 <- function() {
  check_DF(fat_fwd_summary,
           c("n_input_variables","RSS", "BIC", "Cp"),
           "06cd248dd1409b804444bd9ad5533d1d",
           c("RSS", "BIC", "Cp"),
           c(1000, 1000, 1000),
           c("2344494dccc2f1d97ae1b687f9c1848e", 
             "a1fee7f9c4ee005b7800a88af6c1fe9e",
             "0ebaf4f33bfba43d88bf9616f0fb55a3"))
}


# +
# Question 3.1

test_3.1 <- function() {
    check_MC(answer3.1, LETTERS[1:14], "c733ca3274f47b25d5f66415bfd4261f")
}

# +
# Question 3.2

test_3.2 <- function() {
    check_numeric_element(sum(fat_red_OLS$coefficients),1e4,"e5fd00b24472cc93d5b1546344046260")
}

# +
# Question 3.3

test_3.3 <- function() {
    check_numeric_element(sum(fat_test_pred_red_OLS),1e4,"d13084c3518721747179f0bd6d8eb3da")
}

# +
# Question 3.4

test_3.4 <- function() {
    check_DF( fat_RMSE_models,
 c("Model","RMSE"),
 "c01f179e4b57ab8bd9de309e6d576c48",
 "RMSE",
 1e+06,
 "7ab71649bd34aa135bb2926db87c1d08")
}

# +
# Question 3.5

test_3.5 <- function() {
    check_MC(answer3.5,
            LETTERS[1:2],
            'ddf100612805359cd81fdc5ce3b9fbba')
}

# +

