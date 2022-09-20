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

# Question 2.0

test_2.0 <- function() {
  check_MC(answer2.0.0, c("TARGET_deathRate", "povertyPercent", "PCTPrivateCoverage"), "4188171f603a73990c4b0fc767fbb43f")
  check_MC(answer2.0.1, c("TARGET_deathRate", "povertyPercent", "PCTPrivateCoverage"), "875df41e6dd76148be710418bd7b5b0b")
}
#distinct


# +
# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "cancer_poverty_scatterplot"', {
    expect_true(exists("cancer_poverty_scatterplot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(cancer_poverty_scatterplot))
  })

  properties <- c(cancer_poverty_scatterplot$layers[[1]]$mapping, cancer_poverty_scatterplot$mapping)

  test_that("Plot should have povertyPercent on the x-axis", {
    expect_true("povertyPercent" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(cancer_poverty_scatterplot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(cancer_poverty_scatterplot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", cancer_poverty_scatterplot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(cancer_poverty_scatterplot$data)), "906679c1f1b0a7c8e05a254c9e04f3d2")
    expect_equal(digest(round(sum(cancer_poverty_scatterplot$data$povertyPercent))), "4a4d5dd218f223678e55d0fbb2fe0dd0")

    # If povertyPercent is not known:
    # expect_equal(digest(round(sum(pull(cancer_poverty_scatterplot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(cancer_poverty_scatterplot$labels$x == toString(rlang::get_expr(properties$x)))
  })


  print("Success!")
}

# -

# Question 2.2
test_2.2 <- function() {
    check_MC(answer2.2, letters[1:2], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# Question 3.0
test_3.0 <- function() {
  check_MC(answer3.0, letters[1:2], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 4.0

test_4.0 <- function() {
    check_DF(US_cancer_sample250,
            c("replicate", "TARGET_deathRate"  ,"povertyPercent"    ,"PctPrivateCoverage"),
            'acbe123c5ee8d5d2d45c5655f2d526f0',
            c("TARGET_deathRate"  ,"povertyPercent"    ,"PctPrivateCoverage"),
            c(10,10,10),
            c('db996f2c5454af9d5639090601a65092',
             "f034fd1a116feed872e028cd6303ff75",
             "f9a2aa79118e51cae242dc7c8f1aae4d"))
}

# +
# Question 4.1

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "SLR_cancer_sample250"', {
    expect_true(exists("SLR_cancer_sample250"))
  })

  answer_as_numeric <- as.numeric(SLR_cancer_sample250$coefficients[1])
  test_that("sample250$coefficients[1] should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1e3)), "4d8985be39f5b0df49c9135bd8e3effc")
  })

    
    
  answer_as_numeric <- as.numeric(SLR_cancer_sample250$coefficients[2])
  test_that("sample250$coefficients[2] should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1e3)), "6294977d8af230b2f352458eb168c277")
  })
  print("Success!")
}



# Question 4.2
test_4.2 <- function() {
    check_MC(answer4.2, letters[1:3], "6e7a8c1c098e8817e3df3fd1b21149d1")
}

# +
# Question 4.3

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "SLR_cancer_sample250_plot"', {
    expect_true(exists("SLR_cancer_sample250_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(SLR_cancer_sample250_plot))
  })

  properties <- c(SLR_cancer_sample250_plot$layers[[1]]$mapping, SLR_cancer_sample250_plot$mapping)

  test_that("Plot should have povertyPercent on the x-axis", {
    expect_true("povertyPercent" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomPoint" %in% class(SLR_cancer_sample250_plot$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(SLR_cancer_sample250_plot$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", SLR_cancer_sample250_plot$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(SLR_cancer_sample250_plot$data)), "acbe123c5ee8d5d2d45c5655f2d526f0")
    expect_equal(digest(round(sum(SLR_cancer_sample250_plot$data$povertyPercent))), "8414775c40e822ecc3e53a820a0e0cc1")

    # If povertyPercent is not known:
    # expect_equal(digest(round(sum(pull(SLR_cancer_sample250_plot$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(SLR_cancer_sample250_plot$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(SLR_cancer_sample250_plot$labels))
  })

  print("Success!")

}
# -

# Question 4.4
test_4.4 <- function() {
    check_MC(answer4.4.0, letters[1:2], 'ddf100612805359cd81fdc5ce3b9fbba')
    check_MC(answer4.4.1, letters[1:2], '127a2ec00989b9f7faf671ed470be7f8')
}

#---------
# +
# Question 5.1.0

test_5.1.0 <- function() {
  check_DF(SLR_cancer_sample250_results,
           c("term","estimate",  "std.error", "statistic", "p.value" ),
           "c01f179e4b57ab8bd9de309e6d576c48",
           c("estimate",  "std.error", "statistic", "p.value" ),
           c(100,100,100 ,10),
           c("50c8e6660f6829fbc5293340ef6aa952",
             "3a50453f0c060ef50060fd0c212b8d0c",
             "f2780df0bee5ea701b335735c4c22e74",
             "1473d70e5646a26de3c52aa1abd85b1f"
           ))
}
# -

# Question 5.2.0
test_5.2.0 <- function() {
    check_MC(answer5.2.0, letters[1:4], "6e7a8c1c098e8817e3df3fd1b21149d1")
}

# Question 5.2.1
test_5.2.1 <- function() {
    check_MC(answer5.2.1, letters[1:4], 'd110f00cfb1b248e835137025804a23b')
}

# Question 5.2.2
test_5.2.2 <- function() {
    check_MC(answer5.2.2, letters[1:4], '127a2ec00989b9f7faf671ed470be7f8')
}

# Question 5.2.3
test_5.2.3 <- function() {
    check_MC(answer5.2.3, letters[1:3], 'ddf100612805359cd81fdc5ce3b9fbba')
}

# +
# Question 5.3.0

test_5.3.0 <- function() {
    check_DF(SLR_cancer_sample250_CIs, 
            c("term", "estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
            "c01f179e4b57ab8bd9de309e6d576c48",
            c("estimate",  "std.error", "statistic", "p.value",   "conf.low",  "conf.high"),
            c(100,100,100,1,100,100),
             c("50c8e6660f6829fbc5293340ef6aa952",
               "3a50453f0c060ef50060fd0c212b8d0c",
               "f2780df0bee5ea701b335735c4c22e74",
               "1473d70e5646a26de3c52aa1abd85b1f",
               "a941bd883f14010db095651779debab5",
               "976663ad7c1a1401288bdbe047cbbab5")
            )
}

# +
# Question 6.0

#df
test_6.0 <- function() {
    check_DF(lm_boot250,
            c("boot_intercept", "boot_slope"  ),
            "b6a6227038bf9be67533a45a6511cc7e",
             c("boot_intercept", "boot_slope"),    
             c(10000, 10000),
             c("a4b5678bfbb4cba7e97a864e59a86b02",
               "7b44c4fdbb2a98a84f489942a6692e61"))
}

# +
# Question 6.1

test_6.1 <- function() {
  test_that('Did not assign answer to an object called "slope_sampling_dist_250"', {
    expect_true(exists("slope_sampling_dist_250"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(slope_sampling_dist_250))
  })

  properties <- c(slope_sampling_dist_250$layers[[1]]$mapping, slope_sampling_dist_250$mapping)

  test_that("Plot should have boot_slope on the x-axis", {
    expect_true("boot_slope" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(slope_sampling_dist_250$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(slope_sampling_dist_250$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", slope_sampling_dist_250$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(slope_sampling_dist_250$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(slope_sampling_dist_250$data$boot_slope))), "a4d7d529f3cfbf5a268517168bbc38da")

    # If boot_slope is not known:
    # expect_equal(digest(round(sum(pull(slope_sampling_dist_250$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(slope_sampling_dist_250$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(slope_sampling_dist_250$labels))
  })

  print("Success!")
}


# +
# Question 6.2

test_6.2 <- function() {
  test_that('Did not assign answer to an object called "slope_sampling_dist_500"', {
    expect_true(exists("slope_sampling_dist_500"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(slope_sampling_dist_500))
  })

  properties <- c(slope_sampling_dist_500$layers[[1]]$mapping, slope_sampling_dist_500$mapping)

  test_that("Plot should have boot_slope on the x-axis", {
    expect_true("boot_slope" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(slope_sampling_dist_500$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(slope_sampling_dist_500$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", slope_sampling_dist_500$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(slope_sampling_dist_500$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(slope_sampling_dist_500$data$boot_slope))), "dbc54d0980f93fffcb8e536628667f3f")

    # If boot_slope is not known:
    # expect_equal(digest(round(sum(pull(slope_sampling_dist_500$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(slope_sampling_dist_500$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(slope_sampling_dist_500$labels))
  })

  print("Success!")
}


# +
# Question 6.3

test_6.3 <- function() {
  test_that('Did not assign answer to an object called "slope_sampling_dist_3047"', {
    expect_true(exists("slope_sampling_dist_3047"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(slope_sampling_dist_3047))
  })

  properties <- c(slope_sampling_dist_3047$layers[[1]]$mapping, slope_sampling_dist_3047$mapping)

  test_that("Plot should have boot_slope on the x-axis", {
    expect_true("boot_slope" == rlang::get_expr(properties$x))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomBar" %in% class(slope_sampling_dist_3047$layers[[1]]$geom))

    # Remove if not needed:
    # expect_true("GeomVline" %in% class(slope_sampling_dist_3047$layers[[2]]$geom))
  })

  test_that("Plot does not have the correct bin width", {
  expect_equal(
    digest(as.integer(mget("stat_params", slope_sampling_dist_3047$layers[[1]])[["stat_params"]][["binwidth"]])),
    "3e2e4a08c44d0224de5b7e668c75ace3"
    )
  })

  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(slope_sampling_dist_3047$data)), "b6a6227038bf9be67533a45a6511cc7e")
    expect_equal(digest(round(sum(slope_sampling_dist_3047$data$boot_slope))), "4ca352797b1af741275676247c73a873")

    # If boot_slope is not known:
    # expect_equal(digest(round(sum(pull(slope_sampling_dist_3047$data, rlang::get_expr(properties$x))))), "HASH_HERE")
  })

  test_that("x-axis label should be descriptive and human readable", {
    expect_false(slope_sampling_dist_3047$labels$x == toString(rlang::get_expr(properties$x)))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(slope_sampling_dist_3047$labels))
  })

  print("Success!")
}

# -

# Question 6.4
test_6.4 <- function() {
    check_MC(answer6.4, letters[1:3], '6e7a8c1c098e8817e3df3fd1b21149d1')
}

# +
# Question 6.5

test_6.5 <- function() {
    check_DF(boot_SLR_CIs,
            c("B_avg","B_conf.low","B_conf.high"),
             "c01f179e4b57ab8bd9de309e6d576c48",
             c("B_avg","B_conf.low", "B_conf.high"),
             c(100,100,100),
             c("ac32ba774e465fb04b64942abd97e171",
               "9f7ec4bc7059a07baf016c77ffbc75fe",
               "f3972d8aef7f79ed9592fa70a927acac"))
}
