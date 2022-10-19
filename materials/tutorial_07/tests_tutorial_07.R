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


# +
# Question 1.0

test_1.0 <- function() {
    check_DF(facebook_sample,
            c("replicate","total_engagement_percentage",
              "page_engagement_percentage",  "share_percentage",
              "comment_percentage",          "post_category"),              
             "5d6e7fe43b3b73e5fd2961d5162486fa",
             c("replicate","total_engagement_percentage",
              "page_engagement_percentage",  "share_percentage"), 
             c(1,1e4,1e4,1e4),
             c("5d6e7fe43b3b73e5fd2961d5162486fa",
               "599ff6781567232c286fb73dbb42613b",
               "606754c80299bebb9a748f3310ab567b",
               "6d0e1e0088de80006b6d92b0221ff99a"))
}

# +
# Question 1.1

test_1.1 <- function() {
    
    check_numeric_element(sum(facebook_MLR_add$coefficients), 1e4, "860c25faa23d2d3dd487aeaf92ba90b1")
    check_numeric_element(sum(facebook_MLR_add_2$coefficients), 1e4, "e19723c7a5f008d56e4beeaa365bcb98")
    check_numeric_element(sum(facebook_MLR_int$coefficients), 1e4, "4f5a2dfd287198a5f236e2d3c0619aea")
}

# +
# Question 1.2

test_1.2 <- function() {
    
    check_plot(facebook_MLR_Add_plot,
              "page_engagement_percentage",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "5d6e7fe43b3b73e5fd2961d5162486fa",
               "9557989c7e717647716e732efc7b4577",
               TRUE,
               FALSE,
               TRUE)
}

# +
# Question 1.3

test_1.3 <- function() {
    
    check_plot(facebook_MLR_Int_plot,
              "page_engagement_percentage",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "5d6e7fe43b3b73e5fd2961d5162486fa",
               "9557989c7e717647716e732efc7b4577",
               TRUE,
               FALSE,
               TRUE)
}

# +
# Question 1.4

test_1.4 <- function() {
    check_DF(facebook_MLR_add_results,
            c("term","estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             rep(100,6),
             c("7e7978afcc5fa9457d6bad4d397e506c",
               "da7a9b97e93f145c23f4aa044fa4548f",
               "875d5ffb31a0bd487e8c6c0ba8ff5f55",
               "cd5b378492e3bce4b69e74ab1b495779",
               "c8c7262d31454c7c8201d8381bb635e7",
               "1f8f3efd14b2de706bfced1e8e67d925"))
    
        check_DF(facebook_MLR_add_2_results,
            c("term","estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             rep(100,6),
             c("300e5d5a0a22e1d96cc388ba2aaa0cc1",
               "a517ec220e6f27b64c99ff9471312436",
               "501de1612b1a456132756ad3c0bfd3b2",
               "fdf6fa3c76dbe4f1284ce4f08e00924c",
               "0f2b900a2dd9efadcd458b2329c18655",
               "5b50667deb4199465dc22a775126090f"))
    
        check_DF(facebook_MLR_int_results,
            c("term","estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value" ,  "conf.low",  "conf.high"),
             rep(100,6),
             c("42abbb70835a094617c494eb427e2b3b",
               "a3b304cf7239e444879c2d58946962a0",
               "a67e3f58defd4d06c7f170fe936c0257",
               "5f80a2fefde46c5f55850375f0330dae",
               "1dd10013ee5a4f9ba9926ad8ff9b9c74",
               "ba68b1da7b94fb157498993e55809507"))
    
}

# +
# Question 1.5

test_1.5 <- function() {
    check_MC(answer1.5, LETTERS[1:4], "ddf100612805359cd81fdc5ce3b9fbba")
}

# +
# Question 1.6

test_1.6 <- function() {
    check_MC(answer1.6, LETTERS[1:5], "c402dd576693c733f9e77ab1a00fd506")
}

# +
# Question 1.7

test_1.7 <- function() {
    check_MC(answer1.7, LETTERS[1:4], "95767987b2037a2f09c4e5c0997ec206")
}

# +
# Question 1.8

test_1.8 <- function() {
    check_numeric_element(sum(facebook_MLR_add_statistics), 1e3, 'cfce330f855026ac1b0aa6b84b0e7cc7')
    check_numeric_element(sum(facebook_MLR_add_2_statistics), 1e3, '66b650ca28245766693308415bccc071')
    check_numeric_element(sum(facebook_MLR_int_statistics), 1e3, '357b860704cc561789c0cc937eef1b64')
}

# +
# Question 1.11

test_1.11 <- function() {
    check_numeric_element(facebook_F_test_add2_vs_add$F[2], 1e3, '8ae1ac7bdf62dca7c19b427a9153445c')
    check_numeric_element(facebook_F_test_int_vs_add$F[2], 1e3, '107f934699b0da7dd018a3650d1212b6')
}

