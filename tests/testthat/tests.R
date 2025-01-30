### calc_utility
test_that("calc_utility works", {
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