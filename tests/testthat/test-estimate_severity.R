# Tests for .estimate_severity()
# Note that this is an internal function underlying cfr_static()
# when corrected_for_delays is TRUE

# load Ebola 1976 outbreak data
data("ebola1976")

# Ebola onset to death distribution comes from Barry et al. 2018
# a gamma distribution with shape = 2.40, scale = 3.33

poisson_threshold <- 100

# get the corrected dataframe
df_corrected <- estimate_outcomes(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

# run estimate_severity
severity_estimate <- .estimate_severity(
  total_cases = sum(df_corrected$cases),
  total_deaths = sum(df_corrected$deaths),
  total_outcomes = sum(df_corrected$estimated_outcomes),
  poisson_threshold = 100
)

test_that("`estimate_severity`: Basic expectations", {
  expect_vector(severity_estimate, numeric())
  expect_named(
    severity_estimate,
    sprintf("severity_%s", c("estimate", "low", "high"))
  )
  # expect within values
  expect_true(
    all(severity_estimate >= 0.0 & severity_estimate <= 1.0)
  )
  # expect that lo, me, and hi are in roughly ascending order
  expect_true(
    all(
      severity_estimate["severity_low"] <
        severity_estimate["severity_estimate"] &&
        severity_estimate["severity_estimate"] <
          severity_estimate["severity_high"]
    )
  )
  # also check for a snapshot
  expect_snapshot(
    severity_estimate
  )

  # check estimate_severity with higher poisson threshold
  # forcing use of an alternative calculation
  severity_estimate_lt <- .estimate_severity(
    total_cases = sum(df_corrected$cases),
    total_deaths = sum(df_corrected$deaths),
    total_outcomes = sum(df_corrected$estimated_outcomes),
    poisson_threshold = 1000
  )
  # snapshot of severity estimate using alternative method
  expect_snapshot(
    severity_estimate_lt
  )

  # expect that severity is lower when there are fewer cases
  ebola1976$deaths <- ebola1976$deaths - 2
  ebola1976$deaths[ebola1976$deaths < 0] <- 0

  # get the corrected dataframe
  df_corrected <- estimate_outcomes(
    data = ebola1976,
    delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
  )

  # run estimate_severity
  severity_low_deaths <- .estimate_severity(
    total_cases = sum(df_corrected$cases),
    total_deaths = sum(df_corrected$deaths),
    total_outcomes = sum(df_corrected$estimated_outcomes),
    poisson_threshold = poisson_threshold
  )

  expect_true(
    all(severity_estimate > severity_low_deaths)
  )
})

test_that("Special cases of `.estimate_severity()`", {
  # all values are 0
  total_cases <- 0
  total_deaths <- 0
  total_outcomes <- 0
  expect_identical(
    .estimate_severity(total_cases, total_deaths, total_outcomes),
    c(
      severity_estimate = NA_real_,
      severity_low = NA_real_,
      severity_high = NA_real_
    )
  )

  # any 2 values are 0
  total_cases <- 10
  expect_identical(
    .estimate_severity(total_cases, total_deaths, total_outcomes),
    c(
      severity_estimate = NA_real_,
      severity_low = NA_real_,
      severity_high = NA_real_
    )
  )

  total_cases <- 0
  total_outcomes <- 10
  expect_identical(
    .estimate_severity(total_cases, total_deaths, total_outcomes),
    c(
      severity_estimate = NA_real_,
      severity_low = NA_real_,
      severity_high = NA_real_
    )
  )

  # total deaths == total cases, or total deaths == total outcomes
  # when total cases < poisson threshold
  total_cases <- 99
  total_deaths <- 99
  total_outcomes <- 0
  expect_identical(
    .estimate_severity(
      total_cases, total_deaths, total_outcomes,
      poisson_threshold = 100
    ),
    c(
      severity_estimate = NA, # set NA because not valid calculation
      severity_low = NA,
      severity_high = NA
    )
  )

  # expect that total cases >= poisson threshold does not return NAs
  total_cases <- 100
  total_deaths <- 100
  total_outcomes <- 0
  expect_error(
    expect_identical(
      .estimate_severity(total_cases, total_deaths, total_outcomes),
      c(
        severity_estimate = 0.99,
        severity_low = NA_real_,
        severity_high = NA_real_
      )
    )
  )
})

# NOTE: tests on errors removed as input checking moved to upper-level functions

# Expect NAs when multiple values are zero
test_that("estimate_severity returns NAs when inputs are zeros", {
  test_df <- c(
    severity_estimate = NA_real_,
    severity_low = NA_real_,
    severity_high = NA_real_
  )

  expect_identical(
    .estimate_severity(0, 0, 0),
    test_df
  )

  expect_identical(
    .estimate_severity(0, 0, 1),
    test_df
  )

  expect_identical(
    .estimate_severity(1, 0, 0),
    test_df
  )
  # not testing the case where total_outcomes is 0
})
