# ---
# jupyter:
#   jupytext:
#     formats: r:light
#     text_representation:
#       extension: .r
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.11.4
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

library(testthat)
library(digest)

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

#-------
# +
# Question 1.0

test_1.0 <- function() {
  check_numeric_element(sum(fat_cv_lambda_ridge$lambda),
                        1e2,
                        "e424bd25547c048b867d14a95e386fd5")
}

# +
# Question 1.1

test_1.1 <- function() {
  check_numeric_element(sum(fat_cv_lambda_ridge$lambda) - sum(plot_data$lambda),
                        1e4,
                        "1473d70e5646a26de3c52aa1abd85b1f")
}

# +
# Question 1.2

test_1.2 <- function() {
  check_numeric(fat_lambda_min_MSE_ridge,
                1e4,
                "3e58fec15b97b4b65a18dd280f434516")
}

# +
# Question 1.3

test_1.3 <- function() {
  check_numeric_element(sum(fat_ridge_min_coef),
                        1e3,
                        "b6575e85d029f959d2206fbe87a76fda")
}

# +
# Question 1.4

test_1.4 <- function() {
  check_DF( fat_reg_coef,
            c("Full_OLS", "Ridge_min"),
            "e03a89536262b6a0e2beabd90a841c43",
            c("Full_OLS", "Ridge_min"),
            c(10000, 10000),
            c("6aa0bd857e2c6dc9ae873b09b7dd0ed9", "913985836b7bb0c323f8c8cd0ec748f2"))
}

# +

#----
# Question 1.5

test_1.5 <- function() {
  check_numeric_element(sum(fat_test_pred_full_OLS),1e4,"4021310ab0be33783b9d682568bf6b39")
}

# Question 1.6

test_1.6 <- function() {
  check_DF(fat_R_MSE_models,
           c("Model","R_MSE"),
           "4b5630ee914e848e8d07221556b0a2fb",
           "R_MSE",
           1e+06,
           "bdde9b46a0a478904f7b4737ba04ee62")
}

#----
# Question 1.7

test_1.7 <- function() {
  check_numeric_element(sum(fat_test_pred_ridge_min),
                        1e2,
                        "0531ee6bd3fbd015dc25d6561d5a5c58")
}

# +
# Question 1.8

test_1.8 <- function() {
  check_DF( fat_R_MSE_models,
            c("Model","R_MSE"),
            "c01f179e4b57ab8bd9de309e6d576c48",
            "R_MSE",
            1e+06,
            "33e63338a36c36a8eec8d81d999d97ab")
}


#-----------
test_2.0 <- function() {
  test_that('Did not assign answer to an object called "housing_inference"', {
    expect_true(exists("housing_inference"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(housing_inference))
  })

  expected_colnames <- c('LotFrontage','LotArea','MasVnrArea','TotalBsmtSF','GrLivArea','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','Fireplaces','GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ScreenPorch','PoolArea','ageSold','SalePrice')
  given_colnames <- colnames(housing_inference)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(housing_inference))), "795a6187cc2920bf85b645517b1c5133")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(housing_inference$Fireplaces))), "635d85ebc32e43cadb00fe448947d969")
  })
    
  test_that('Did not assign answer to an object called "housing_selection"', {
    expect_true(exists("housing_selection"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(housing_selection))
  })

  expected_colnames <- c('LotFrontage','LotArea','MasVnrArea','TotalBsmtSF','GrLivArea','BsmtFullBath','BsmtHalfBath','FullBath','HalfBath','BedroomAbvGr','KitchenAbvGr','Fireplaces','GarageArea','WoodDeckSF','OpenPorchSF','EnclosedPorch','ScreenPorch','PoolArea','ageSold','SalePrice')
  given_colnames <- colnames(housing_selection)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(housing_selection))), "79e45e1d5f262c0254744dd752dba4e4")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(housing_selection$Fireplaces))), "5015f2ef36add58ff850db8bc8192e14")
  })

  print("Success!")
}

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "lasso_model"', {
    expect_true(exists("lasso_model"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_model$lambda) * 100)), "a8844edd3aad401b4183c13431072822")
  })
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(length(lasso_model$lambda)), "70907d9a2fe63b90036adda611231960")
  })
    
  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_model$lambda.min) * 1000)), "9adb1eb6ce96f8913eb9a4835ef86323")
  })

  print("Success!")
}

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "beta_lasso"', {
    expect_true(exists("beta_lasso"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(beta_lasso) * 1000)), "e7efb1aae36d62baa58ca0239686894d")
  })

  print("Success!")
}

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "lasso_selected_covariates"', {
    expect_true(exists("lasso_selected_covariates"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(lasso_selected_covariates), "2d1f003aed7cd512d28539da59c67311")
  })

  print("Success!")
}

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "lasso_variables_vif"', {
    expect_true(exists("lasso_variables_vif"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(lasso_variables_vif) * 100000)), "bb3d2513d40b77a7504b5dc7fe8042fe")
  })

  print("Success!")
}

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer2.6, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.6))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })

  print("Success!")
}

test_2.7 <- function() {
  test_that('Did not assign answer to an object called "inference_model"', {
    expect_true(exists("inference_model"))
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(inference_model$coefficients)*10000)), "adbbf79beca4344f201dc0bb9eb2de56")
    expect_equal(digest(as.integer(sum(inference_model$df.residual))), "643b8e3656f86e132e4a164dbd00cac4")
  })

  print("Success!")
}
