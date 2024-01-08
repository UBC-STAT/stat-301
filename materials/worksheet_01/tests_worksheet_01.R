library(digest)
library(testthat)

# Question 1.0


test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0.0"', {
    expect_true(exists("answer1.0.0"))
  })

  test_that('Did not assign answer to an object called "answer1.0.1', {
    expect_true(exists("answer1.0.1"))
  })

  test_that('Did not assign answer to an object called "answer1.0.2"', {
    expect_true(exists("answer1.0.2"))
  })

  test_that('Did not assign answer to an object called "answer1.0.3"', {
    expect_true(exists("answer1.0.3"))
  })

  test_that('Did not assign answer to an object called "answer1.0.4"', {
    expect_true(exists("answer1.0.4"))
  })

  test_that('Did not assign answer to an object called "answer1.0.5"', {
    expect_true(exists("answer1.0.5"))
  })

  test_that('Did not assign answer to an object called "answer1.0.6"', {
    expect_true(exists("answer1.0.6"))
  })

  test_that('Did not assign answer to an object called "answer1.0.7"', {
    expect_true(exists("answer1.0.7"))
  })

  test_that('Did not assign answer to an object called "answer1.0.8"', {
    expect_true(exists("answer1.0.8"))
  })

  test_that('Did not assign answer to an object called "answer1.0.9"', {
    expect_true(exists("answer1.0.9"))
  })
    

  test_that('"answer1.0.0" is incorrect', {
    expect_equal(digest(tolower(answer1.0.0)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  test_that('"answer1.0.1" is incorrect', {
    expect_equal(digest(tolower(answer1.0.1)), "ddf100612805359cd81fdc5ce3b9fbba")
  })

  test_that('"answer1.0.2" is incorrect', {
    expect_equal(digest(tolower(answer1.0.2)), "93a9078c6326f37b481d3e99b60ad987")
  })

  test_that('"answer1.0.3" is incorrect', {
    expect_equal(digest(tolower(answer1.0.3)), "7279bb4184f9c53d42729c6eb22db36a")
  })

  test_that('"answer1.0.4" is incorrect', {
    expect_equal(digest(tolower(answer1.0.4)), "93a9078c6326f37b481d3e99b60ad987")
  })

  test_that('"answer1.0.5" is incorrect', {
    expect_equal(digest(tolower(answer1.0.5)), "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  test_that('"answer1.0.6" is incorrect', {
    expect_equal(digest(tolower(answer1.0.6)), "93a9078c6326f37b481d3e99b60ad987")
  })

  test_that('"answer1.0.7" is incorrect', {
    expect_equal(digest(tolower(answer1.0.7)), "d110f00cfb1b248e835137025804a23b")
  })

  test_that('"answer1.0.8" is incorrect', {
    expect_equal(digest(tolower(answer1.0.8)), "127a2ec00989b9f7faf671ed470be7f8")
  })

  test_that('"answer1.0.9" is incorrect', {
    expect_equal(digest(tolower(answer1.0.9)), "93a9078c6326f37b481d3e99b60ad987")
  })
  


  print("Success!")
}

# Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "answer1.1.0"', {
    expect_true(exists("answer1.1.0"))
  })

  test_that('Did not assign answer to an object called "answer1.1.1', {
    expect_true(exists("answer1.1.1"))
  })

  test_that('Did not assign answer to an object called "answer1.1.2"', {
    expect_true(exists("answer1.1.2"))
  })

  test_that('Did not assign answer to an object called "answer1.1.3"', {
    expect_true(exists("answer1.1.3"))
  })

  test_that('Did not assign answer to an object called "answer1.1.4"', {
    expect_true(exists("answer1.1.4"))
  })

  test_that('Did not assign answer to an object called "answer1.1.5"', {
    expect_true(exists("answer1.1.5"))
  })

  test_that('Did not assign answer to an object called "answer1.1.6"', {
    expect_true(exists("answer1.1.6"))
  })

  test_that('Did not assign answer to an object called "answer1.1.7"', {
    expect_true(exists("answer1.1.7"))
  })
    
  test_that('"answer1.1.0" is incorrect', {
    expect_equal(digest(tolower(answer1.1.0)), "9ea4abe21634cce386f79e1129af2744")
  })

  test_that('"answer1.1.1" is incorrect', {
    expect_equal(digest(tolower(answer1.1.1)), "317be6c8c60403220d0f4f9fa663a873")
  })

  test_that('"answer1.1.2" is incorrect', {
    expect_equal(digest(tolower(answer1.1.2)), "f001447a6eb0832abe634a9d73b9ec9d")
  })

  test_that('"answer1.1.3" is incorrect', {
    expect_equal(digest(tolower(answer1.1.3)), "960eec3042ab34d09f2b9225fb5bc4da")
  })

  test_that('"answer1.1.4" is incorrect', {
    expect_equal(digest(tolower(answer1.1.4)), "db294f40a69f16cc3faae4b5530f864d")
  })

  test_that('"answer1.1.5" is incorrect', {
    expect_equal(digest(tolower(answer1.1.5)), "617ffaf4c3c94a5982db589f522475bf")
  })

  test_that('"answer1.1.6" is incorrect', {
    expect_equal(digest(tolower(answer1.1.6)), "a333e1a273609919bed20474a75b24f8")
  })

  test_that('"answer1.1.7" is incorrect', {
    expect_equal(digest(tolower(answer1.1.7)), "bc2b8ee7fd55735cc5fc11acdb0eeacb")
  })

  print("Success!")
}

# Question 2.0

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "answer2.0"', {
    expect_true(exists("answer2.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.0, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.0))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })

  print("Success!")
}

# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "answer2.1"', {
    expect_true(exists("answer2.1"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer2.1, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}

# Question 2.2

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "answer2.2"', {
    expect_true(exists("answer2.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", or "E")', {
    expect_match(answer2.2, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer2.2))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d110f00cfb1b248e835137025804a23b")
  })

  print("Success!")
}

# Question 2.3

test_2.3 <- function() {
    test_that('Did not assign answer to an object called "tiktok_true_params"', {
        expect_true(exists("tiktok_true_params"))
    })
    
    test_that('tiktok_true_params should be a data frame', {
        expect_true("data.frame" %in% class(tiktok_true_params))
    })
    
    expected_colnames <- c("mean_current_ad", "sd_current_ad", "mean_new_ad", "sd_new_ad")
    given_colnames <- colnames(tiktok_true_params)
    
    
    test_that('tiktok_true_params does not have the correct columns', {
        expect_equal(length(setdiff(
            union(expected_colnames, given_colnames),
            intersect(expected_colnames, given_colnames)
        )), 0)
    })
    
    
    test_that("tiktok_true_params does not contain the correct number of rows", {
        expect_equal(digest(as.integer(nrow(tiktok_true_params))), "4b5630ee914e848e8d07221556b0a2fb")
    })
    
    
    test_that("tiktok_true_params does not contain the correct data", {
        expect_equal(digest(as.integer(sum(tiktok_true_params[1,]) * 100)), "c96a15d59cd6d4e1a27a604eaee5c3a2")
    })

  print("Success!")
}

# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "tiktok_sample"', {
    expect_true(exists("tiktok_sample"))
  })

  test_that("tiktok_sample should be a data frame", {
    expect_true("data.frame" %in% class(tiktok_sample))
  })


  test_that("tiktok_sample frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(tiktok_sample))), "2567f3d5adc004a73dc268884026f3bd")
  })

  test_that("tiktok_sample$ad_watched does not contain the correct data", {
    expect_equal(digest(as.integer(sum(tiktok_sample$ad_watched == "current"))), "5d6e7fe43b3b73e5fd2961d5162486fa")
  })
    
  test_that("tiktok_sample$dwell_time does not contain the correct data", {
    expect_equal(digest(as.integer(sum(tiktok_sample$dwell_time) *10^3)), "a41df85d0b8f8cd0061047829f378be1")
  })

  print("Success!")
}

# Question 2.5

test_2.5 <- function() {
  test_that('Did not assign answer to an object called "tiktok_sample"', {
    expect_true(exists("tiktok_sample"))
  })
  
  test_that("tiktok_sample should be a data frame", {
    expect_true("data.frame" %in% class(tiktok_sample))
  })
  
  expected_colnames <- c("user", "ad_watched", "dwell_time")
  given_colnames <- colnames(tiktok_sample)
  test_that("tiktok_sample does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })
  
  test_that("samples_TikTok does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(tiktok_sample))), "2567f3d5adc004a73dc268884026f3bd")
  })
  
  test_that("samples_TikTok does not contain the correct data", {
    expect_equal(digest(sum(as.integer(tiktok_sample$dwell_time))), "d211f7f7fb5f0e63f095771c887fa98a")
  })
  
  test_that("Did not assign answer to an object called dwell_time_boxplots", {
    expect_true(exists("dwell_time_boxplots"))
  })
  
  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(dwell_time_boxplots))
  })
  
  properties <- c(dwell_time_boxplots$layers[[1]]$mapping, dwell_time_boxplots$mapping)
  
  test_that("Plot should have ad_watched on the x-axis", {
    expect_true("ad_watched" == tolower(rlang::get_expr(properties$x)))
  })
  
  test_that("Plot should have dwell_time on the y-axis", {
    expect_true("dwell_time" == tolower(rlang::get_expr(properties$y)))
  })
  
  test_that("Plot should have ad_watched on fill property", {
    expect_true("ad_watched" == tolower(rlang::get_expr(properties$fill)))
  })
  
  test_that("Plot does not have the correct layers", {
    expect_true("GeomBoxplot" %in% class(dwell_time_boxplots$layers[[1]]$geom))
  })
  
  test_that("Plot does not use the correct data", {
    expect_equal(digest(nrow(dwell_time_boxplots$data)), "2567f3d5adc004a73dc268884026f3bd")
    expect_equal(digest(length(unique(dwell_time_boxplots$data$ad_watched))), "c01f179e4b57ab8bd9de309e6d576c48")
  })
  
  test_that("x-axis label should be descriptive and human readable", {
    expect_false(dwell_time_boxplots$labels$x == toString(rlang::get_expr(properties$x)))
  })
  
  test_that("y-axis label should be descriptive and human readable", {
    expect_false(dwell_time_boxplots$labels$y == toString(rlang::get_expr(properties$y)))
  })
  
  test_that("Plot should have a title", {
    expect_true("title" %in% names(dwell_time_boxplots$labels))
  })
  
  print("Success!")
}

# Question 2.6

test_2.6 <- function() {
  test_that('Did not assign answer to an object called "answer2.6"', {
    expect_true(exists("answer2.6"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D", "E")', {
    expect_match(answer2.6, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.6))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  
  print("Success!")
}

# Question 2.7

test_2.7 <- function() {
 test_that('"answer2.7$statistic" is incorrect', {
    expect_equal(digest(as.integer(answer2.7$statistic * 1e4)), "1474c261ae5d423288be9004fb51acc3")
  })
    
  test_that('"answer2.7$parameter" is incorrect', {
    expect_equal(digest(as.integer(answer2.7$parameter * 1e5)), "904656c118fbb670cedb1b0e50233c53")
  })
  
    
  test_that('"answer2.7$p.value" is incorrect', {
    expect_equal(digest(as.integer(answer2.7$p.value * 1e9)), "35d372a6286bfeae5bf63a5dd4d57d5a")
  })
  
  print("Success!")
}

# Question 2.8

test_2.8 <- function() {
  test_that('Did not assign answer to an object called "answer2.8"', {
    expect_true(exists("answer2.8"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer2.8, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer2.8))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "6e7a8c1c098e8817e3df3fd1b21149d1")
  })
  
  print("Success!")
}

# Question 2.9

test_2.9 <- function() {
  test_that('Did not assign answer to an object called "tiktok_permute_results"', {
    expect_true(exists("tiktok_permute_results"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(tiktok_permute_results))
  })

  expected_colnames <- c("obs_test_stat", "pvalue")
  given_colnames <- colnames(tiktok_permute_results)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(tiktok_permute_results))), "4b5630ee914e848e8d07221556b0a2fb")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(tiktok_permute_results$obs_test_stat) * 1e5)), "230d8ed07fee633f974205193056006b")
    expect_equal(digest(as.integer(sum(tiktok_permute_results$pvalue) * 1e5)), "1473d70e5646a26de3c52aa1abd85b1f")
    
  })

  print("Success!")
}

# Question 3.0

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "answer3.0"', {
    expect_true(exists("answer3.0"))
  })

  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.0, "true|false", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.0))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "d2a90307aac5ae8d0ef58e2fe730d38b")
  })

  print("Success!")
}


# Question 3.1

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "answer3.1"', {
    expect_true(exists("answer3.1"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D")', {
    expect_match(answer3.1, "a|b|c|d", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.1))
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })
  
  print("Success!")
}


# Question 3.2

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "answer3.2"', {
    expect_true(exists("answer3.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E")', {
    expect_match(answer3.2, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer3.2))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

# Question 3.3

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "answer3.3"', {
    expect_true(exists("answer3.3"))
  })
  
  test_that('Solution should be a single character ("A", "B", "C", "D", "E")', {
    expect_match(answer3.3, "a|b|c|d|e", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.3))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "") {
  #  print("")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })
  
  print("Success!")
}

# Question 3.4

test_3.4 <- function() {
  test_that('Did not assign answer to an object called "answer3.4"', {
    expect_true(exists("answer3.4"))
  })
  
  test_that('Solution should be "true" or "false"', {
    expect_match(answer3.4, "true|false", ignore.case = TRUE)
  })
  
  answer_hash <- digest(tolower(answer3.4))
  #if (answer_hash == "") {
  #  print("HINT_HERE")
  #}
  
  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "05ca18b596514af73f6880309a21b5dd")
  })
  
  print("Success! You are done with the first worksheet of STAT 301!!")
}
