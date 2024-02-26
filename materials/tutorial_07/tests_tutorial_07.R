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
#--
#Question 1.0

test_1.0 <- function() {
  check_numeric_element(Ftest_3genes_add_full$F[2], 
                        100, 
                        "cc18eca3d186eb8772b9768990dcd2ea")
}

#--

#Question 1.1

test_1.1  <- function() {
  test_that('Did not assign answer to an object called "answer1.1"', {
    expect_true(exists("answer1.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.1, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.1 ))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("You are doing great!")
}
                            
#----
# Question 1.2
                            
test_1.2.0 <- function() {
  check_DF(add_mrna_results,
           c("term","estimate",  "std.error", "statistic", "p.value", "conf.low", "conf.high"),
           "234a2a5581872457b9fe1187d1616b13",
           c("estimate",  "std.error", "statistic", "p.value" , "conf.low", "conf.high"),
           c(1e6,1e6,100 ,10, 1e6,1e6),
           c("fe063eefeb8dbe2c9ee184361b79093e",
             "0426791560fff8db010b19ea3af7b77f",
             "61a5f7bea3f303f179bfedd59bdc6f52",
             "8eaca7c9b35d05ab15c9125bc92372fa",
             "523bd10bd34507591ddd319eef7eae74", 
             "a152274bbc908e5f9ad240243435e40a"
           ))
}
                          
                            
test_1.2.1 <- function() {
  check_numeric_element(Ftest_3genes_add_mrna$F[2], 
                        100, 
                        "9487172f214b56a933ed69dff6a5e934")
}
                            

# Question 1.4

test_1.4 <- function() {
    check_MC(answer1.4, LETTERS[1:4], "ddf100612805359cd81fdc5ce3b9fbba")
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
    
    
        check_DF(second_set_fat,
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
