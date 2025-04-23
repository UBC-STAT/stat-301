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

#-------------

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer"', {
    expect_true(exists("breast_cancer"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer))
  })

  expected_colnames <- c('ID','mean_radius','mean_texture','mean_perimeter','mean_smoothness','mean_compactness','mean_concavity','mean_concave_points','mean_symmetry','mean_fractal_dimension','radius_error','texture_error','perimeter_error','smoothness_error','compactness_error','symmetry_error','fractal_dimension_error','target')
  given_colnames <- colnames(breast_cancer)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer))), "19074fefec837da04af22bb345a7a8cb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer$target))), "9487172f214b56a933ed69dff6a5e934")
  })

  print("Success!")
}

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_model"', {
    expect_true(exists("breast_cancer_model"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(breast_cancer_model))
  })

  test_that("glm does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_model$residuals) * 10e4)), "bc6253f1bc2b62959ff3f70ab6d7f128")
    expect_equal(digest(as.integer(sum(breast_cancer_model$coefficients) * 10e4)), "bd987161735734af9f997351f686c6da")
  })

  print("Success!")
}

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "breast_cancer_resid"', {
    expect_true(exists("breast_cancer_resid"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(breast_cancer_resid))
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(breast_cancer_resid))), "19074fefec837da04af22bb345a7a8cb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(breast_cancer_resid$resid_raw)* 10e4)), "1473d70e5646a26de3c52aa1abd85b1f")
    expect_equal(digest(as.integer(sum(breast_cancer_resid$resid_pearson)* 10e4)), "7e61c3051bb3049bd5a9c92b0b951df7")
  })

  print("Success!")
}

test_1.3.0 <- function() {
  check_numeric_element(summary_dev, 
                        100, 
                        "a3ed22f18356d16d157d2e5f2982bfa3")
}

test_1.3.1 <- function() {
  check_numeric_element(breast_cancer_resid_dev, 
                        100, 
                        "a3ed22f18356d16d157d2e5f2982bfa3")
}  

test_1.4.0 <- function() {
  check_numeric_element(summary_null, 
                        100, 
                        "c7a1aba815fc4ea06c596eac3f871e85")
}

test_1.4.1 <- function() {
  check_numeric_element(breast_cancer_glance$null.deviance, 
                        100, 
                        "c7a1aba815fc4ea06c596eac3f871e85")
}

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "model_null"', {
    expect_true(exists("model_null"))
  })

  test_that("Solution should be a glm object", {
    expect_true("glm" %in% class(model_null))
  })

  test_that("glm does not contain the correct data", {
    expect_equal(digest(as.integer(sum(model_null$residuals) * 10e4)), "1473d70e5646a26de3c52aa1abd85b1f")
    expect_equal(digest(as.integer(sum(model_null$coefficients) * 10e4)), "5f7a9b5708e4ffe9b1e4be627511d2b2")
  })

  print("You are doing great!")
}

test_1.6 <- function() {
  check_numeric_element(breast_cancer_gof$Deviance[2], 
                        1, 
                        "b60cd9e7adea266f22143c1f855506f1")
}                      

test_1.7 <- function() {
    check_TF(answer1.7, '05ca18b596514af73f6880309a21b5dd')
}

                                                   
#--

#Question 1.8

test_1.8  <- function() {
  test_that('Did not assign answer to an object called "answer1.8"', {
    expect_true(exists("answer1.8"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.8, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.8 ))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("You are doing great!")
}
                            
#----
test_1.9 <- function() {
  check_numeric_element(breast_cancer_aov$Deviance[2], 
                        1, 
                        "bfc11d0fe85d8e3038dc8efe8787392c")
}                          

test_1.10  <- function() {
  test_that('Did not assign answer to an object called "answer1.10"', {
    expect_true(exists("answer1.10"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.10, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.10))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("You are doing great!")
}

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
    
    
        check_DF(selection_set_fat,
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
  check_DF(fat_bwd_summary_df,
           c("n_input_variables","RSQ", "RSS", "ADJ.R2"),
           "06cd248dd1409b804444bd9ad5533d1d",
           c("RSQ", "RSS", "ADJ.R2"),
           c(1000, 1000, 1000),
           c("dd578c83d1356e2363859f0ca649f95d","8807c4a844c7de0775a0e3120ca60499", "75ff97ee9d21884c2e0f1206b3b3c399"))
}

# +
# Question 2.2

test_2.2 <- function() {
  check_MC(answer2.2, LETTERS[1:14], "524dee37861cc9bab42227df389509af")
}

# Question 2.3

test_2.3 <- function() {
  check_numeric_element(sum(fat_bwd_generative$coefficients),1e4,"f7134397f6afa181af608fffc309e902")
}


# Question 2.4

test_2.4 <- function() {
  check_numeric_element(answer2.4,1e4,"caa4da3fe2cad4ebe9b4929a9d486462")
}
