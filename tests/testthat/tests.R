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


############################# calc_grade_weight ################################


test_that("calc_grade_weight works", {
  expect_equal(
    calc_grade_weight(grade=5, max_grade=0, min_grade=5, min_weight=0.4), 0.4
  )
  expect_equal(
    calc_grade_weight(grade=5, max_grade=5, min_grade=0, min_weight=0.4), 1
  )
  expect_equal(
    calc_grade_weight(grade=3, max_grade=0, min_grade=6, min_weight=0.4), 0.7
  )
  expect_equal(
    calc_grade_weight(grade=0, max_grade=0, min_grade=6, min_weight=0.6), 1
  )
  expect_equal(
    calc_grade_weight(grade=2, max_grade=3, min_grade=2, min_weight=0.5), 0.5
  )
  expect_equal(
    calc_grade_weight(grade=2, max_grade=3, min_grade=1, min_weight=0.6), 0.8
  )
  expect_equal(
    calc_grade_weight(grade=2.5, max_grade=3, min_grade=1, min_weight=0.6), 0.9
  )
  expect_equal(
    calc_grade_weight(grade=2, max_grade=3, min_grade=0, min_weight=0.7), 0.9
  )
  expect_equal(
    calc_grade_weight(grade=0, max_grade=3, min_grade=0, min_weight=0.7), 0.7
  )
  expect_equal(
    calc_grade_weight(grade=3, max_grade=0, min_grade=3, min_weight=0.7), 0.7
  )
  expect_equal(
    calc_grade_weight(grade=2, max_grade=0, min_grade=5, min_weight=0.5), 0.8
  )
  df <- data.frame(Grade=c(1, 0, 2, 1, 1, 2, 2, 2, 0))
  expect_equal(
    sapply(
      1:nrow(df), 
      function(i) {
        calc_grade_weight(
          grade=df$Grade[i], 
          max_grade=2,
          min_grade=0,
          min_weight=0.6
        )
      }
    ),
    c(0.8, 0.6, 1.0, 0.8, 0.8, 1.0, 1.0, 1.0, 0.6)
  )
  expect_error(
    calc_grade_weight(grade=0, max_grade=3, min_grade=1, min_weight=0.7),
    "Grade provided is not within minimum/maximum bounds."
  )
  expect_error(
    calc_grade_weight(grade=0, max_grade=3, min_grade=6, min_weight=0.7),
    "Grade provided is not within minimum/maximum bounds."
  )
})


############################# calc_grade_weight ################################


test_that("calc_uncertainty_weight works", {
  expect_equal(
    calc_uncertainty_weight(scaled_sample_size = 0.7, grade_weight = 0.8), 0.56
  )
  expect_equal(
    calc_uncertainty_weight(
      scaled_sample_size = c(0.7, 0.5, 0.3, 0.8), 
      grade_weight = c(0.8, 0.2, 0.4, 0.4)
    ), 
    c(0.56, 0.10, 0.12, 0.32)
  )
  df <- data.frame(
    GradeWeights = c(0.8, 0.6, 1.0, 0.8, 0.8, 1.0, 1.0, 1.0, 0.6),
    ScaledSampSize = c(0.5, 0.3, 0.8, 0.6, 0.9, 0.7, 0.8, 0.7, 0.2)
  )
  expect_equal(
    calc_uncertainty_weight(
      scaled_sample_size = df$ScaledSampSize, 
      grade_weight = df$GradeWeights
    ), 
    c(0.40, 0.18, 0.80, 0.48, 0.72, 0.70, 0.80, 0.70, 0.12)
  )
})


############################### calc_std_error #################################


test_that("calc_std_error works", {
  expect_equal(
    round(
      calc_std_error(utility = 0.7, sample_size = 100, grade_weight = 0.8), 
      7
    ),
    0.0509175
  )
  expect_equal(
    round(
      calc_std_error(
        utility = c(0.7, 0.5, 0.3, 0.8), 
        sample_size = c(100, 80, 50, 115), 
        grade_weight = c(0.8, 0.5, 0.4, 0.4)
      ), 
    7
    ),
    c(0.0509175, 0.0780869, 0.1000000, 0.0583460)
  )
  df <- data.frame(
    Utility = c(0.9, 0.2, 1.0, 0.8, 0.7, 0, 1.0, 1.0, 0.4),
    SampSize = c(0.5, 0.3, 0.8, 0.6, 0.9, 0.7, 0.8, 0.7, 0.2),
    GradeWeights = c(0.8, 0.6, 1.0, 0.8, 0.8, 1.0, 1.0, 1.0, 0.6)
  )
  expect_equal(
    round(
      calc_std_error(
        utility = df$Utility,
        sample_size = df$SampSize, 
        grade_weight = df$GradeWeights
      ),
    5
    ),
    c(0.25355, 0.36823, 0.00236, 0.32880, 0.34942, 0.00243, 0.00236, 0.00243, 0.46291)
  )
})