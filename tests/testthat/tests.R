############################## calc_utility ####################################

test_that("calc_utility works", {
  expect_equal(calc_utility(0.2, c(1, 0), c(0.5, 0.3, 0.2)), 0.14)
  expect_equal(calc_utility(0.2, 1, c(0.5, 0.5)), 0.2)
  expect_equal(calc_utility(c(0.2, 0.3), c(1, 0), c(0.5, 0.3, 0.2)), 0.042)
  expect_error(calc_utility(c(), c(), c()), 
    "At least one of 'fundamental_vars' or 'additional_vars' must be provided.")
  expect_error(calc_utility(c(0.2, 0.3), c(1, 0), c(0.5)), 
    regexp = "Incorrect number of betas")
  expect_error(calc_utility(c(0.2, 0.3), c(1, 0), c(0.5, 0.5, 0.2)), 
    "Betas must sum to 1.")
  expect_error(calc_utility(c(0.2, 1.2), c(1, 0), c(0.5, 0.3, 0.2)), 
    "All variables must be scaled between 0 and 1.")
  expect_error(calc_utility(c(0.2, 1.2), c(12, 0), c(0.5, 0.3, 0.2)), 
    "All variables must be scaled between 0 and 1.")
  expect_error(calc_utility(c(0.2, 1), c(3, 0), c(0.5, 0.3, 0.2)), 
    "All variables must be scaled between 0 and 1.")
})

test_that("calc_utility works for dataframes", {
  # set up sample dataframe
  F1 <- c(0.1, 0.2, 0.5, 0.9)
  F2 <- c(0.4, 0.4, 0.6, 0.8)
  A1 <- c(0.1, 0.5, 0.8, 0.3)
  A2 <- c(0, 1, 0, 1)
  dat <- data.frame(cbind(F1, F2, A1, A2))
  betas <- c(0.4, 0.4, 0.2)

  # set up output
  dat$utility <- c(0.0096, 0.0640, 0.1560, 0.5184)

  expect_equal(sapply(
    1:nrow(dat), 
    function(i) {
      calc_utility(
        c(dat$F1[i], dat$F2[i]), 
        c(dat$A1[i], dat$A2[i]),
        betas
      )
    }
  ), dat$utility)
})


############################# scale_with_sigmoid ###############################


test_that("scale_with_sigmoid works", {
  expect_equal(
    round(scale_with_sigmoid(
      log10(c(1, 10, 50, 100)), 
      10^(-(log10(log10(max(100)))-1)), 
      log10(max(100))/2
    ),7),
    c(0.0066929, 0.5000000, 0.9705409, 0.9933071)
  )
})


############################# scale_sample_size ################################


test_that("scale_sample_size works", {
  expect_equal(
    round(scale_sample_size(
      c(1, 10, 50, 100)
    ),7),
    c(0.0066929, 0.5000000, 0.9705409, 0.9933071)
  )
  expect_equal(
    round(scale_sample_size(
      c(1, 10, 50, 100), 
      method="log-linear"
    ),7),
    c(0.000000, 0.500000, 0.849485, 1.000000)
  )
  df <- data.frame(SampleSize=c(10, 15, 100, 30, 50, 90, 140))
  expect_equal(
    round(scale_sample_size(
      df$SampleSize)
    ,7),
    c(0.4157012, 0.6177623, 0.9868631, 0.8679240, 0.9486533, 0.9837918, 0.9933071)
  )
  expect_error(
    round(scale_sample_size(
      c(1, 10, 50, 100), 
      method="linear"
    ),7),
    "Method must be either log-linear or sigmoid."
  )
})
