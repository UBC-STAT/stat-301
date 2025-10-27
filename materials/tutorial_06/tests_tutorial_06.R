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
#Question 1.1

test_1.1 <- function() {
  check_numeric_element(Ftest_3genes_add_full$F[2], 
                        100, 
                        "cc18eca3d186eb8772b9768990dcd2ea")
}

#--

#Question 1.2

test_1.2  <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer1.2 ))
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("You are doing great!")
}
                            
#----
# Question 1.3
                            
test_1.3.0 <- function() {
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
                          
                            
test_1.3.1 <- function() {
  check_numeric_element(Ftest_3genes_add_mrna$F[2], 
                        100, 
                        "9487172f214b56a933ed69dff6a5e934")
}
                            

# Question 1.5

test_1.5 <- function() {
    check_MC(answer1.5, LETTERS[1:4], "ddf100612805359cd81fdc5ce3b9fbba")
}

