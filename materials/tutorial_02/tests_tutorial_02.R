library(digest)
library(testthat)

# +
# Question 1.0

test_1.0 <- function() {
  test_that('Did not assign answer to an object called "bandit"', {
    expect_true(exists("bandit"))
  })
  
  test_that('The wrong class was used.', {
    expect_equal(bandit$class_name, 'BasicGaussianBandit')
  })
  
  test_that("Solution is incorrect: wrong number of arms", {
    expect_equal(digest(bandit$k), "fa5a4df7ac0f9782037da890557fd8b8")
  })
    
  test_that("Solution is incorrect: wrong probabilities", {
    expect_equal(digest(as.integer(sum(bandit$mu_per_arm) * 1000)), "2a5e4c025d6e73e1370f64c7e07cc425")
    expect_equal(digest(as.integer(sum(bandit$sigma_per_arm) * 1000)), "be2e04ae0841fd48b1ab57d83ec9ead8")
  })  

  print("Success!")
}
# -

# Question 2.0
test_2.0 <- function() {
  test_that('Did not assign answer to an object called "simulations"', {
    expect_true(exists("simulations"))
  })

  answer_as_numeric <- as.numeric(simulations)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "6e96c307060fba1b1d3a36d2410fd595")
  })
    
  test_that('Did not assign answer to an object called "horizon"', {
    expect_true(exists("horizon"))
  })

  answer_as_numeric <- as.numeric(horizon)
  test_that("Solution should be a number", {
    expect_false(is.na(answer_as_numeric))
  })

  test_that("Solution is incorrect", {
    expect_equal(digest(as.integer(answer_as_numeric)), "2567f3d5adc004a73dc268884026f3bd")
  })

  print("Success!")
}


# +
# Question 2.1

test_2.1 <- function() {
  test_that('Did not assign answer to an object called "epsilon_first_05"', {
    expect_true(exists("epsilon_first_05"))
  })
  test_that('Did not assign answer to an object called "epsilon_first_20"', {
    expect_true(exists("epsilon_first_20"))
  })  
  
  test_that('The wrong class was used.', {
    expect_equal(epsilon_first_05$class_name, 'EpsilonFirstPolicy')
  })
  test_that('The wrong class was used.', {
    expect_equal(epsilon_first_20$class_name, 'EpsilonFirstPolicy')
  })
  
  print("Success!")
}

# +
# Question 2.2

test_2.2 <- function() {
  test_that('Did not assign answer to an object called "epsilon_greedy_05"', {
    expect_true(exists("epsilon_greedy_05"))
  })
  test_that('Did not assign answer to an object called "epsilon_greedy_20"', {
    expect_true(exists("epsilon_greedy_20"))
  })
    
  test_that('The wrong class was used.', {
    expect_equal(epsilon_greedy_05$class_name, 'EpsilonGreedyPolicy')
    expect_equal(epsilon_greedy_20$class_name, 'EpsilonGreedyPolicy')
  })
  
  test_that("Solution is incorrect: wrong value of epsilon", {
    expect_equal(digest(as.integer(epsilon_greedy_05$epsilon*100)), "dd4ad37ee474732a009111e3456e7ed7")
    expect_equal(digest(as.integer(epsilon_greedy_20$epsilon*100)), "be3c152f6f6bcd5f85f9e4cba49b1e48")
  })

  print("Success!")
}


# +
# Question 2.3

test_2.3 <- function() {
  test_that('Did not assign answer to an object called "ucb_policy"', {
    expect_true(exists("ucb_policy"))
  })
  
  test_that('The wrong class was used.', {
    expect_equal(ucb_policy$class_name, 'UCB1Policy')
  })
  
  print("Success!")
}
# -

test_3.1 <- function() {
  test_that('Did not assign answer to an object called "agents"', {
    expect_true(exists("agents"))
  })

  test_that('You did not use the right policy', {
    expect_equal(agents[[1]]$policy, epsilon_first_05)
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[1]]$name, "epsilon_first_05")
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[2]]$policy, epsilon_first_20)
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[2]]$name, "epsilon_first_20")
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[3]]$policy, epsilon_greedy_05)
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[3]]$name, "epsilon_greedy_05")
  })
  
  test_that('You did not use the right policy', {
    expect_equal(agents[[4]]$policy, epsilon_greedy_20)
  })  
  test_that('You did not use the right policy', {
    expect_equal(agents[[4]]$name, "epsilon_greedy_20")
  })
  test_that('You did not use the right policy', {
    expect_equal(agents[[5]]$policy, ucb_policy)
  })  
  test_that('You did not use the right policy', {
    expect_equal(agents[[5]]$name, "ucb_policy")
  })

  print("Success!")
}

# +
test_4.1 <- function() {
  test_that('Did not assign answer to an object called "simulator"', {
    expect_true(exists("simulator"))
  })
  test_that('You did not use the right agent', {
    expect_equal(simulator$agents, agents)
  })
  test_that('You did not use the right number of simulations', {
    expect_equal(digest(simulator$simulations), "6e96c307060fba1b1d3a36d2410fd595")
  })
  test_that('You did not use the right horizon', {
    expect_equal(digest(simulator$horizon), "2567f3d5adc004a73dc268884026f3bd")
  })
  print("Success!")
}


# -

test_4.2 <- function() {
  test_that('Did not assign answer to an object called "history"', {
    expect_true(exists("history"))
  })
  test_that('Wrong results', {
    expect_equal(digest(as.integer(sum(history$data$choice))), "32dc45c1b995198ea788a768b9d74e05")
  })
    
  print("Success!")
}


# Question 5.0
test_5.0 <- function() {
  test_that('Did not assign answer to an object called "answer5.0"', {
    expect_true(exists("answer5.0"))
  })

  test_that('Solution should be a single character ("A", "B", "C", "D", "E")', {
    expect_match(answer5.0, "a|b|c|d|e", ignore.case = TRUE)
  })

  answer_hash <- digest(tolower(answer5.0))
  #if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #} else if (answer_hash == "HASH_HERE") {
  #  print("HINT_HERE")
  #}

  test_that("Solution is incorrect", {
    expect_equal(answer_hash, "93a9078c6326f37b481d3e99b60ad987")
  })

  print("Success!")
}

