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




check_plot <- function(answerX.X, x_axis_var, geom_type, hasVline, bin_width_hash, nrow_hash, x_axis_var_hash, hasTitle) {
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
    expect_equal(digest(round(sum(answerX.X$data[x_axis_var]))), x_axis_var_hash)
    
    # If X_AXIS_VAR is not known:
    # expect_equal(digest(round(sum(pull(answerX.X$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(answerX.X$labels$x == toString(rlang::get_expr(properties$x)))
  })
  
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





# +
# Question 1.0

test_1.0 <- function() {
    check_MC(answer1.0, LETTERS[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 1.1

test_1.1 <- function() {
    check_MC(answer1.1, LETTERS[1:4], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 1.2

test_1.2 <- function() {
    check_MC(answer1.2, LETTERS[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

# +
# Question 1.3

test_1.3 <- function() {
    check_MC(answer1.3, LETTERS[1:6], 'd110f00cfb1b248e835137025804a23b')
}

# +
# Question 1.4

test_1.4.0 <- function() {
    check_numeric_element(facebook_MLR_add$coefficients[1],1e4,"d5b88dcc246f32cfed1b43deb9812631")
}

test_1.4.1 <- function() {
    check_numeric_element(facebook_MLR_add$coefficients[2],1e4,"145f986f8144db1b38d9628565448655")
}

test_1.4.2 <- function() {
    check_numeric_element(facebook_MLR_add$coefficients[3],1e4,"8fa0daec38928998e5275cee03bf0cd7")
}

test_1.4.3 <- function() {
    check_numeric_element(facebook_MLR_add$coefficients[4],1e4,"107ed321a2cbee329ab57593781fd5cf")
}

# +
# Question 1.5

test_1.5 <- function() {
    check_plot(facebook_MLR_add_plot,
               "page_engagement_percentage",
               "GeomPoint" ,
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "a026d6d420e9adc250cb3d47fed00e84",
               "d4847fc7ad8586ea2772cffffb6758d5",
               TRUE)
}

# +
# Question 1.6

test_1.6 <- function() {
    check_DF(facebook_MLR_add_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "234a2a5581872457b9fe1187d1616b13",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("03fc87c40baf0078cf7f7e09d9c206ba",
               "6ab3d402d9b22801ab4fb94ef4e63b1d",
               "5a5a897f1afa810878b46c0f1c4feb78",
               "f120166a24c1bd7b43a32da9dd0bf6ec",
               "dccd84fdf0a11bffecbc6fb46088082c",
               "09b7c5f2db0f3079ce4979b5a75e4a45"))
}

# +
# Question 1.7

# manual

# +
# Question 1.8

test_1.8 <- function() {
    check_MC(answer1.8, getPermutations(LETTERS[1:4]), "257ffd0fec52cbf9ff2dba41f66d6b26")
}

# +
# Question 2.0

test_2.0 <- function() {
    check_MC(answer2.0, LETTERS[1:6], '7279bb4184f9c53d42729c6eb22db36a')
}

# +
# Question 2.1

test_2.1.0 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[1], 1e4, "83ce1ea7bcbc4a20419acd3c541b753b")
}
test_2.1.1 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[2], 1e4, "5b9d785757b2dd6c1c632bc016c7e5d1")
}
test_2.1.2 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[3], 1e4, "5dd11cc8a48e222ddd65314cc0eb8ca2")
}
test_2.1.3 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[4], 1e4, "0101688f7ce28bf8721de13b5d23b2dd")
}
test_2.1.4 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[5], 1e4, "2963ea64fefb03ffeffedf77b45b5d78")
}
test_2.1.5 <- function() {
    check_numeric_element(facebook_MLR_int$coefficients[6], 1e4, "0a23ca33e027d05cac4743a9c1e3e4a0")
}

# +
# Question 2.2

test_2.2 <- function() {
    check_plot(facebook_MLR_int_plot,
               "page_engagement_percentage",
               "GeomPoint",
               FALSE,
               "3e2e4a08c44d0224de5b7e668c75ace3",
               "a026d6d420e9adc250cb3d47fed00e84",
               "d4847fc7ad8586ea2772cffffb6758d5",
               TRUE)
}

# +
# Question 2.3

test_2.3 <- function() {
    check_DF(facebook_MLR_int_results,
            c("term","estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
             c(100, 100, 100, 100, 100, 100),
             c("ca21e364698e573f63da3ecac022c6e3",
               "c653fc074ad3928b0ec8d45b45e751f2",
               "0e752ac149d3e40494f3b3dfbce3cdb0",
               "8dcc9ab9194a962167360a4ff6bb6827",
               "d864bbc4823e449540584d436fea6bea",
               "988d4e086adb7e0f3e2db2de485d8128"))
}

# +
# Question 2.4

test_2.4 <- function() {
    check_MC(answer2.4, getPermutations(LETTERS[1:5]), '93e311a221d93a15962a246827baabdc')
}

# +
# Question 2.5

# manual

# +
# Quesiton 2.6


test_2.6.0 <- function() {
        check_DF(facebook_SLR_action_results,
            c("term","estimate",  "std.error", "statistic", "p.value"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100, 100, 100, 100),
             c("f46cbe1c8264919d762582360b48c456",
               "80b0ae73fe0e882b0a24973e4e2c8203",
               "734d7d339d820b5c18ae1dc497546477",
               "4b5630ee914e848e8d07221556b0a2fb"))
}





test_2.6.1 <- function() {
        check_DF(facebook_SLR_product_results,
            c("term","estimate",  "std.error", "statistic", "p.value"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("estimate",  "std.error", "statistic", "p.value"),
             c(100, 100, 100, 100),
             c("8745f3cfb2e7716c02117a0e1c394082",
               "262e00171252ad97820501c35c732e3d",
               "18b577cdb9f397b4bcd98a1c63d33b32",
               "dd4ad37ee474732a009111e3456e7ed7"))
}


test_2.6.2 <- function() {
        check_DF(facebook_MLR_int_results,
            c("term","estimate",  "std.error", "statistic", "p.value", "conf.low",  "conf.high"),
             "25e6a154090e35101d7678d6f034353a",
             c("estimate",  "std.error", "statistic", "p.value", "conf.low",  "conf.high"),
             c(100, 100, 100, 100,100,100),
             c("ca21e364698e573f63da3ecac022c6e3",
               "c653fc074ad3928b0ec8d45b45e751f2",
               "0e752ac149d3e40494f3b3dfbce3cdb0",
               "8dcc9ab9194a962167360a4ff6bb6827",
              "d864bbc4823e449540584d436fea6bea",
              "988d4e086adb7e0f3e2db2de485d8128"))
}

# +
# Question 2.7

# manual
