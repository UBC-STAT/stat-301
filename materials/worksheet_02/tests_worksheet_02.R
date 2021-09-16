library(digest)
library(testthat)

# Question 1.0
test_1.0 <- function() {
  test_that('Did not assign answer to an object called "answer1.0"', {
    expect_true(exists("answer1.0"))
  })

  answer_as_numeric <- as.numeric(answer1.0)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution should be an integer", {
    expect_true(answer_as_numeric %% 1 == 0)
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "234a2a5581872457b9fe1187d1616b13")
  })

  print("Success!")
}


# +
# Question 1.1

test_1.1 <- function() {
  test_that('Did not assign answer to an object called "clickthrough_rate_pop"', {
    expect_true(exists("clickthrough_rate_pop"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(clickthrough_rate_pop))
  })

  expected_colnames <- c("image", "clickthrough_rate")
  given_colnames <- colnames(clickthrough_rate_pop)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(clickthrough_rate_pop))), "234a2a5581872457b9fe1187d1616b13")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(clickthrough_rate_pop$clickthrough_rate) * 10000)), "2e2b6044e89fea7cefe35d977f8d25ee")
  })

  print("Success!")
}


# +
# Question 1.2

test_1.2 <- function() {
  test_that('Did not assign answer to an object called "answer1.2"', {
    expect_true(exists("answer1.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer1.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.2))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}


# +
# Question 1.3

test_1.3 <- function() {
  test_that('Did not assign answer to an object called "answer1.3"', {
    expect_true(exists("answer1.3"))
  })

  answer_as_numeric <- as.numeric(answer1.3)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric * 1e2)), "1049715f3606ac15b4ec3909b10b3a58")
  })

  print("Success!")
}


# +
# Question 1.4

test_1.4 <- function() {
  test_that('Did not assign answer to an object called "answer1.4"', {
    expect_true(exists("answer1.4"))
  })

  test_that('Solution should be a single character ("A", "B")', {
    expect_match(answer1.4, "a|b", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer1.4))

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "127a2ec00989b9f7faf671ed470be7f8")
  })

  print("Success!")
}


# +
# Question 1.5

test_1.5 <- function() {
  test_that('Did not assign answer to an object called "bandit"', {
    expect_true(exists("bandit"))
  })
  
  test_that('The wrong class was used.', {
    expect_equal(bandit$class_name, 'BasicBernoulliBandit')
  })
  
  test_that("Solution is incorrect: wrong number of arms", {
    expect_equal(digest(bandit$k), "234a2a5581872457b9fe1187d1616b13")
  })
    
  test_that("Solution is incorrect: wrong probabilities", {
    expect_equal(digest(as.integer(bandit$weights * 100)), "7ad35d40225a34e8a6f4f3766a0bd5ad")
  })  

  print("Success!")
}

# +
# Question 2.0

test_2.0 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_policy"', {
    expect_true(exists("epsilon_first_policy"))
  })
  
  test_that('The wrong class was used.', {
    expect_equal(epsilon_first_policy$class_name, 'EpsilonFirstPolicy')
  })
  
  test_that("Solution is incorrect: wrong number of arms", {
    expect_equal(digest(as.integer(epsilon_first_policy$first)), "16071ab8270571c6c83d682892e00ea5")
  })

  print("Success!")
}

# +
# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_agent"', {
    expect_true(exists("epsilon_first_agent"))
  })
  test_that('You did not use the right bandit.', {
    expect_equal(epsilon_first_agent$bandit, bandit)
  })
  test_that('You did not use the right policy', {
    expect_equal(epsilon_first_agent$policy, epsilon_first_policy)
  })
    
  test_that('The wrong class was used.', {
    expect_equal(epsilon_first_agent$name, 'EpsilonFirst')
  })

  print("Success!")
}

# +
# Question 2.2

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_simulator"', {
    expect_true(exists("epsilon_first_simulator"))
  })
  test_that('You did not use the right agent', {
    expect_equal(epsilon_first_simulator$agents[[1]], epsilon_first_agent)
  })
  test_that('You did not use the right number of simulations', {
    expect_equal(digest(epsilon_first_simulator$simulations), "f14d903563e7c44a2c122bc61a7c28e0")
  })
  test_that('You did not use the right horizon', {
    expect_equal(digest(epsilon_first_simulator$horizon), "2567f3d5adc004a73dc268884026f3bd")
  })
  print("Success!")
}

# +
# Question 2.3

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_log"', {
    expect_true(exists("epsilon_first_log"))
  })
  test_that('Wrong results', {
    expect_equal(digest(as.integer(sum(epsilon_first_log$data$choice))), "17dd934041b0ff043c1e59d05e89c2a2")
  })
    
  print("Success!")
}


# +
# Question 2.4

test_2.4 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_summary"', {
    expect_true(exists("epsilon_first_summary"))
  })

  test_that("Solution should be a data frame", {
    expect_true("data.frame" %in% class(epsilon_first_summary))
  })

  expected_colnames <- c("t", "avg_reward", "avg_cum_reward_rate")
  given_colnames <- colnames(epsilon_first_summary)
  test_that("Data frame does not have the correct columns", {
    expect_equal(length(setdiff(
      union(expected_colnames, given_colnames),
      intersect(expected_colnames, given_colnames)
    )), 0)
  })

  test_that("Data frame does not contain the correct number of rows", {
    expect_equal(digest(as.integer(nrow(epsilon_first_summary))), "2567f3d5adc004a73dc268884026f3bd")
  })

  test_that("Data frame does not contain the correct data", {
    expect_equal(digest(as.integer(sum(epsilon_first_summary$avg_reward) * 10000)), "7b1596d00699d6302d9f5a72cac68313")
    expect_equal(digest(as.integer(sum(epsilon_first_summary$avg_cum_reward_rate) * 10000)), "d0020715550290f66feea2a26b92f2f8")
  })

  print("Success!")
}
# -

test_2.5 <- function() {


  test_that('Did not assign answer to an object called "epsilon_first_plot"', {
    expect_true(exists("epsilon_first_plot"))
  })

  test_that("Solution should be a ggplot object", {
    expect_true(is.ggplot(epsilon_first_plot))
  })

  properties <- c(epsilon_first_plot$layers[[1]]$mapping, epsilon_first_plot$mapping)

  test_that("Plot should have t on the x-axis", {
    expect_true("t" == rlang::get_expr(properties$x))
  })

  test_that("Plot should have avg_reward on the y-axis", {
    expect_true("avg_reward" == rlang::get_expr(properties$y))
  })

  test_that("Plot does not have the correct layers", {
    expect_true("GeomLine" %in% class(epsilon_first_plot$layers[[1]]$geom))
  })

  test_that("Plot should have a title", {
    expect_true("title" %in% names(epsilon_first_plot$labels))
  })

  print("Success!")
}


# +
# Question 3.0

test_3.0 <- function() {
  test_that('Did not assign answer to an object called "epsilon_greedy_policy"', {
    expect_true(exists("epsilon_greedy_policy"))
  })
  
  test_that('The wrong class was used.', {
    expect_equal(epsilon_greedy_policy$class_name, 'EpsilonGreedyPolicy')
  })
  
  test_that("Solution is incorrect: wrong number of arms", {
    expect_equal(digest(as.integer(epsilon_greedy_policy$epsilon*100)), "be3c152f6f6bcd5f85f9e4cba49b1e48")
  })

  print("Success!")
}

# +
# Question 3.1

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "agents"', {
    expect_true(exists("agents"))
  })

  test_that('You did not use the right policy', {
    expect_equal(agents[[1]]$policy, epsilon_first_policy)
  })
    
  test_that('The wrong class was used.', {
    expect_equal(agents[[2]]$name, 'EpsilonGreedy')
  })
  
  test_that('You did not use the right policy', {
    expect_equal(agents[[2]]$policy, epsilon_greedy_policy)
  })  

  print("Success!")
}

# +
# Question 3.2

test_3.2 <- function() {
  test_that('Did not assign answer to an object called "my_simulator"', {
    expect_true(exists("my_simulator"))
  })
  test_that('You did not use the right agent', {
    expect_equal(my_simulator$agents[[1]], epsilon_first_agent)
  })
  test_that('You did not use the right agent', {
    expect_equal(my_simulator$agents[[2]], agents[[2]])
  })
  test_that('You did not use the right number of simulations', {
    expect_equal(digest(my_simulator$simulations), "f14d903563e7c44a2c122bc61a7c28e0")
  })
  test_that('You did not use the right horizon', {
    expect_equal(digest(my_simulator$horizon), "2567f3d5adc004a73dc268884026f3bd")
  })
  print("Success!")
}

# +
# Question 3.3

test_3.3 <- function() {
  test_that('Did not assign answer to an object called "history"', {
    expect_true(exists("history"))
  })
  test_that('Wrong results', {
    expect_equal(digest(as.integer(sum(history$data$choice))), "ac917bc7e9ab2b31b330f1605fa66e97")
  })
    
  print("Success!")
}


# +
# Question 4.0

test_4.0 <- function() {
  test_that('Did not assign answer to an object called "epsilon_greedy_vs_first"', {
    expect_true(exists("epsilon_greedy_vs_first_avg"))
  })

    print("Success!")
}


# +
# Question 4.1

test_4.1 <- function() {
  test_that('Did not assign answer to an object called "epsilon_greedy_vs_first"', {
    expect_true(exists("epsilon_greedy_vs_first_cumulative"))
  })

    print("Success!")
}


# +
# Question 4.2

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "answer4.2"', {
    expect_true(exists("answer4.2"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.2, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.2))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}


# +
# Question 4.3

test_4.3 <- function() {
  test_that('Did not assign answer to an object called "answer4.3"', {
    expect_true(exists("answer4.3"))
  })

  test_that('Solution should be a single character ("A", "B", "C", or "D")', {
    expect_match(answer4.3, "a|b|c|d", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer4.3))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "ddf100612805359cd81fdc5ce3b9fbba")
  })

  print("Success!")
}

