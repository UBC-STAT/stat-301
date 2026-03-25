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
  test_that('Did not assign answer to an object called "model_matrix_X_train"', {
    expect_true(exists("model_matrix_X_train"))
  })

  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(model_matrix_X_train))
  })
  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(matrix_Y_train))
  })

  expected_colnames <- c('temperature',	'feel_temp','humidity',	'windspeed','holiday1',	'is_weekendweekend','bad_weathergood','peak_seasonpeak','rush_hourrush')
  given_colnames <- colnames(model_matrix_X_train)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(model_matrix_X_train))), "7c5989e554c5e06b4714ac3cac763963")
  })
  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(matrix_Y_train))), "7c5989e554c5e06b4714ac3cac763963")
  })

  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(model_matrix_X_train[,"temperature"]) * 10e2)), "8e15218b97a794f2c55477b349ee42ee")
  })
  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(matrix_Y_train))), "8cc7eb6023a54d6ec43fa2534d0d7038")
  })
  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "bike_cv_LASSO"', {
    expect_true(exists("bike_cv_LASSO"))
  })

  test_that("Solution should be a cv.glmnet object", {
    expect_true("cv.glmnet" %in% class(bike_cv_LASSO))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(bike_cv_LASSO$index[1,]), "9a6564e67167bff7e7cf99a541a641f1")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(bike_cv_LASSO$cvm[97]*10e6)), "c7f66da1cae4f223b9bae717f05900f7")
  })

  print("Success!")
}
                            
test_1.2 <- function() {
  test_that('Did not assign answer to an object called "lambda_1se_deviance_LASSO"', {
    expect_true(exists("lambda_1se_deviance_LASSO"))
  })

  answer_as_numeric <- as.numeric(lambda_1se_deviance_LASSO)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 10e6)), "4ca25779c599016eaf5c1d079454f7b6")
  })

  print("Success!")
}

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "coefs_bike_LASSO"', {
    expect_true(exists("coefs_bike_LASSO"))
  })

  test_that("Solution does not contain the correct data", {
    expect_equal(digest(as.integer(sum(coefs_bike_LASSO[,"s1"])*10e2)), "16d071bf6215df871c10120b4e79eebe")
  })

  print("Success!")
}                            

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.4, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.4))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "7ddf2c27b3f33a6ea4dfe386b9bcc2c8")
  })

  print("Success!")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "model_matrix_X_test"', {
    expect_true(exists("model_matrix_X_test"))
  })

  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(model_matrix_X_test))
  })
  test_that("Solution should be a matrix", {
    expect_true("matrix" %in% class(matrix_Y_test))
  })

  expected_colnames <- c('temperature',	'feel_temp','humidity',	'windspeed','holiday1',	'is_weekendweekend','bad_weathergood','peak_seasonpeak','rush_hourrush')
  given_colnames <- colnames(model_matrix_X_test)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(model_matrix_X_test))), "16e3624c950bd329b1aba823185e5ef1")
  })
  test_that("Matrix does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(matrix_Y_test))), "16e3624c950bd329b1aba823185e5ef1")
  })

  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(model_matrix_X_test[,"temperature"]) * 10e2)), "f80ee7d404c40343b09ef18e038684b8")
  })
  test_that("Matrix does not contain the correct data", {
    expect_equal(digest(as.integer(sum(matrix_Y_test))), "17085daf4118425a1cb4b23c4c81246e")
  })
  print("Success!")
} 

test_1.6 <- function() {
  test_that('Did not assign answer to an object called "bike_pred_test"', {
    expect_true(exists("bike_pred_test"))
  })

  expected_colnames <- c("lambda.1se")
  given_colnames <- colnames(bike_pred_test)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(bike_pred_test))), "16e3624c950bd329b1aba823185e5ef1")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(bike_pred_test[,1]))), "d5e9f84a40dff2a692335e1643417b88")
  })

  print("Success!")
}                            