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
  cols <- colnames(df)
  print(deparse(cols))
  print(digest(as.integer(nrow(df))))
  print(deparse(cols))
  print(deparse(precisionList))
    rsf <- c()
  for (i in 1: length(precisionList)) {
    rsf <- c(rsf,digest(as.integer(sum(df[cols[i]],na.rm=T) * as.double(precisionList[[i]]))))
  }
    print(deparse(rsf))
  
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
  check_DF(housing_full_OLS_results,
           c("term","estimate",  "std.error", "statistic", "p.value" ),
           "be3c152f6f6bcd5f85f9e4cba49b1e48",
           c("estimate",  "std.error", "statistic", "p.value" ),
           c(1,1,100 ,10),
           c("4d79092015412fcbfb892ef82e10f09f",
             "546f532db11d0273d869d24e4a3a6ee6",
             "a80f0f9169f2b3e95c1e6135f8b802e0",
             "3a289991381335a0fc94ad80f79fe88e"
           ))
    check_DF(testing_housing,
             c('LotFrontage', 'LotArea', 'MasVnrArea', 'TotalBsmtSF', 'GrLivArea', 
               'BsmtFullBath', 'BsmtHalfBath', 'FullBath', 'HalfBath', 'BedroomAbvGr',
               'KitchenAbvGr', 'Fireplaces', 'GarageArea', 'WoodDeckSF', 'OpenPorchSF',
               'EnclosedPorch', 'ScreenPorch', 'PoolArea', 'ageSold', 'SalePrice', 'pred_full_OLS'),
             "795a6187cc2920bf85b645517b1c5133",
             c("pred_full_OLS" ),
             c(1),
             c("2fd125bdca207ad5975cd86a231d7ab5"))
}
 
#--
# Question 1.1

test_1.1 <- function() {
    check_TF(answer1.1,
            "05ca18b596514af73f6880309a21b5dd")
}

#--
# Question 1.2
                            
test_1.2 <- function() {
  check_DF(housing_RMSE_models,
           c("Model","RMSE"),
           "4b5630ee914e848e8d07221556b0a2fb",
           c("RMSE"),
           c("100"),
           c("dc5d1880d0c6e4190a427d881622a666"))
}

# +
# Question 1.3

test_1.3 <- function() {
    check_DF(housing_RMSE_models_expanded,
              c("Model","RMSE"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("RMSE"),
             c(100),
             c("aa1b55c17f59f4383777b064f8f24c35")
    )
}

test_2.0.0  <- function() {
  check_DF(cars_null_results,
           c("term","estimate",  "std.error", "statistic", "p.value" ),
           "4b5630ee914e848e8d07221556b0a2fb",
           c("estimate",  "std.error", "statistic", "p.value" ),
           c(1,1,100 ,10),
           c("16b16a32c40f106dd362592fae92d810",
             "3d4b20da9c5087ee9b4c2729d9fbeee2",
             "55c9c56c03e7ceff53e9f995f23c97c3",
             "1473d70e5646a26de3c52aa1abd85b1f"
           ))
    }

test_2.0.1  <- function() {
  check_DF(cars_full_results,
           c("term","estimate",  "std.error", "statistic", "p.value" ),
           "2a099397e2d2dd0f2a2e5a5b4234867d",
           c("estimate",  "std.error", "statistic", "p.value" ),
           c(1,1,100 ,10),
           c("3c7ade74f22a2726626c2013a080d01b",
             "314ebc72c85b00cd8941d1a61dbc59a1",
             "d5dec40175f697f35b06f4f7a3528c8c",
             "be3c152f6f6bcd5f85f9e4cba49b1e48"
           ))
    }

test_2.1 <- function() {check_numeric_element(sum(cars_forward_summary$rsq),1, "71db8a6cad03244e6e50f0ad8bc95a65") 
                        }

test_2.2 <- function() {check_numeric_element(sum(cars_forward_summary$bic),0.001, "2530bbbbce4f6602a5b7a419f05d6e82") 
                        }
                            
test_2.4 <- function() {check_numeric_element(sum(modAIC_back$coeff),0.001, "865090b0c33fc39eef5e259758f8e991") 
                        }

test_2.6 <- function() {check_numeric_element(sum(modAIC_forward$coeff),0.001, "a93ab7a9dd4c354dd2b3ebb60535e893") 
                        }

test_2.7 <- function() {
    check_TF(answer2.7,
            "d2a90307aac5ae8d0ef58e2fe730d38b")
}                            

                           
                            

                    
                    