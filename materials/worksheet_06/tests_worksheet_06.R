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
    check_DF(properties_cip_90,
            c("assess_val", "BLDG_METRE", "fit", "lwr", "upr"),
            "5d6e7fe43b3b73e5fd2961d5162486fa",
            c("assess_val", "BLDG_METRE", "fit", "lwr", "upr"),
            c(1, 1, 100, 100, 100),
             c("2bd910d5d0a2784e920bb70864394e2c", "da016125afe448211b90b4a6920fb474", 
               "757c105401ef48fc18d9825963aa517f", "c22644555dc9b2cafcb486f6f121aa22",
               "280f782da52a910144605e6ffbca844a"))
}

# +
# Question 1.1

test_1.1 <- function() {
    check_MC(answer1.1,
             LETTERS[1:4],
            '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.2

test_1.2 <- function() {
    check_TF(answer1.2,
            "05ca18b596514af73f6880309a21b5dd")
}

# +
# Question 1.3

test_1.3 <- function() {
    check_TF(answer1.3,
            'd2a90307aac5ae8d0ef58e2fe730d38b')
}

# +
# Question 1.4

test_1.4 <- function() {
    check_numeric(n_cpi_wider, 1, '1473d70e5646a26de3c52aa1abd85b1f')
}
#------
                            
                            
# Question 2.0
                            
test_2.0 <- function() { expect_equal(digest(as.integer(sum(MLR_rome$coefficients))),"9564c75e11688d755ea80e5758c6a0ef")
print("Success!")
}

# Question 2.1                            
test_2.1 <- function() {
    check_DF(rome_cip,
             c("price", "distance", "rating", "bedrooms", "fit", "lwr", "upr"),
            "a56a1f140e9adf6b548545996e36c4cd",
            c("price", "distance", "rating", "bedrooms", "fit", "lwr", "upr"),
            c(1, 1, 1, 1, 1, 1, 1),
             c("c79402ae8828aaaf2b221420a75e5e1c", "a4339e3e183f231478260eab21db2329", 
               "30050a2d9cc5cea6df845c35f8b356b4", "20979583a129c962044e1f0fdce9b728", 
               "c79402ae8828aaaf2b221420a75e5e1c", "271f6a81ec36980e9d8a9a85fdb27808",
               "12f3a881bc011c1841bdbea3100e9ba5"))
}
                            
# Question 2.2

test_2.2 <- function() {
    check_MC(answer2.2, LETTERS[1:4], 'ddf100612805359cd81fdc5ce3b9fbba')
}                            

# Question 2.3                            
test_2.3 <- function() {
    check_DF(rome_pi,
             c("price", "distance", "rating", "bedrooms", "fit", "lwr", "upr"),
            "a56a1f140e9adf6b548545996e36c4cd",
            c("price", "distance", "rating", "bedrooms", "fit", "lwr", "upr"),
            c(1, 1, 1, 1, 1, 1, 1),
             c("c79402ae8828aaaf2b221420a75e5e1c", "a4339e3e183f231478260eab21db2329", 
               "30050a2d9cc5cea6df845c35f8b356b4", "20979583a129c962044e1f0fdce9b728", 
               "c79402ae8828aaaf2b221420a75e5e1c", "cf631aec591b0360edd24fafb9f8167d",
               "a9a7dcf8b0d76f084a52a9b92c378271"))
}  
                            
# Question 2.4                            
test_2.4 <- function() {
    check_TF(answer2.4,
            "05ca18b596514af73f6880309a21b5dd")
}

# Question 2.5

test_2.5 <- function() {
    check_MC(answer2.5, LETTERS[1:4], '6e7a8c1c098e8817e3df3fd1b21149d1')
}  
