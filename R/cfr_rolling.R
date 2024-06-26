#' Estimate static severity for an expanding time series
#'
#' @description Calculates the CFR at each time point in the case and death time
#' series supplied, using an expanding window of time. The static CFR is
#' calculated for each time point, using the time series from the start to each
#' time point, and increasing the number of time points included by one in each
#' iteration.
#'
#' @details When delay correction is applied by passing a delay distribution
#' density function to `delay_density`, the internal function
#' [.estimate_severity()] is used to calculate the rolling severity.
#'
#' Note that in the naive method the severity estimate and confidence intervals
#' cannot be calculated for days on which the cumulative number of cases since
#' the start of the time-series, and for days on which the cumulative number of
#' deaths reported exceeds the cumulative reported cases, and is returned as
#' `NA`.
#'
#' @inheritParams cfr_static
#'
#' @return A `<data.frame>` with the date, maximum likelihood estimate and 95%
#' confidence interval of the daily severity estimates, named
#' "severity_estimate", "severity_low", and "severity_high", with one row for
#' each day in the original data.frame.
#'
#' @details
#' `cfr_rolling()` applies the internal function `.estimate_severity()` to an
#' expanding time-series of total cases, total estimated outcomes, and total
#' deaths. The method used to generate a profile likelihood for each day depends
#' on the outbreak size and initial severity estimate for that day. This is
#' essentially the same as running [cfr_static()] on each new day. The method
#' used for each day is not communicated to the user, in order to prevent
#' cluttering the terminal with messages.
#'
#' @export
#'
#' @examples
#' # load package data
#' data("ebola1976")
#'
#' # estimate severity without correcting for delays
#' cfr_static(ebola1976)
#'
#' # estimate severity for each day while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Barry et al. 2018
#' # The Lancet. <https://doi.org/10.1016/S0140-6736(18)31387-4>
#' # view only the first values
#' estimate <- cfr_rolling(
#'   ebola1976,
#'   delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
#' )
#'
#' head(estimate)
#'
cfr_rolling <- function(data,
                        delay_density = NULL,
                        poisson_threshold = 100) {
  # Add message to indicate this is a utility function
  message(
    "`cfr_rolling()` is a convenience function to help understand how",
    " additional data influences the overall (static) severity.",
    " Use `cfr_time_varying()` instead to estimate severity changes over",
    " the course of the outbreak."
  )
  # input checking
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 3
  )
  # check that input `<data.frame>` has columns date, cases, and deaths
  checkmate::assert_names(
    colnames(data),
    must.include = c("date", "cases", "deaths")
  )
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("date", "cases", "deaths")],
    types = c("Date", "integerish"),
    any.missing = FALSE
  )
  # check that data$date is a date column
  checkmate::assert_date(data$date, any.missing = FALSE, all.missing = FALSE)
  # check for excessive missing date and throw an error
  # also check delay_density
  stopifnot(
    "Input data must have sequential dates with none missing or duplicated" =
      identical(unique(diff(data$date)), 1) # use numeric 1, not integer
    # this solution works when df$date is `Date`
    # this may need more thought for dates that are integers, POSIXct,
    # or other units; consider the units package
  )
  checkmate::assert_count(poisson_threshold, positive = TRUE)

  # NOTE: delay_density is checked in estimate_outcomes() if passed and not NULL

  # prepare cumulative sums
  cumulative_cases <- cumsum(data$cases)
  cumulative_deaths <- cumsum(data$deaths)

  # Check cumulative sums for count type
  checkmate::assert_integerish(cumulative_cases, lower = 0)
  # use assert_number to set upper limit at total_cases
  checkmate::assert_integerish(
    cumulative_deaths,
    upper = max(cumulative_cases), lower = 0
  )

  if (!is.null(delay_density)) {
    # calculating the total number of cases and deaths after correcting for
    # the number of cases with estimated outcomes and using this estimate as the
    # of deaths
    data <- estimate_outcomes(
      data = data,
      delay_density = delay_density
    )

    cumulative_outcomes <- cumsum(data$estimated_outcomes)

    # Get direct estimates of daily p, throw message if some p are < 1e-4
    # NOTE: choosing message rather than warning, as warnings are nearly
    # guaranteed in the early stages of an outbreak due to poor data
    p_mid_values <- cumulative_deaths / round(cumulative_outcomes)

    if (any(is.infinite(p_mid_values) | p_mid_values < 1e-4)) {
      message(
        "Some daily ratios of total deaths to total cases with known outcome",
        " are below 0.01%: some CFR estimates may be unreliable.",
        call. = FALSE
      )
    }

    # generate series of CFR estimates with expanding time window
    # Suppress method choice messages to prevent spamming user.
    severity_estimates <- suppressMessages(
      Map(
        cumulative_cases, cumulative_deaths, cumulative_outcomes, p_mid_values,
        f = function(cases, deaths, outcomes, p_mid) {
          .estimate_severity(cases, deaths, outcomes, poisson_threshold, p_mid)
        }
      )
    )

    # bind list elements together; list to matrix to data.frame
    severity_estimates <- as.data.frame(do.call(rbind, severity_estimates))
  } else {
    # check for good indices
    indices <- which(
      cumulative_deaths <= cumulative_cases &
        cumulative_cases > 0
    )

    # subset the good cumulative data
    cumulative_cases <- cumulative_cases[indices]
    cumulative_deaths <- cumulative_deaths[indices]

    # prepare holding matrix
    severity_estimates <- matrix(NA_real_, nrow = nrow(data), ncol = 3)
    colnames(severity_estimates) <- c(
      "severity_estimate", "severity_low", "severity_high"
    )

    # calculating the uncorrected CFR rolling over all days
    severity_estimates[indices, "severity_estimate"] <- cumulative_deaths /
      cumulative_cases

    cfr_lims <- Map(
      cumulative_deaths, cumulative_cases,
      f = function(x, n) stats::binom.test(x, n, p = 1)$conf.int
    )
    cfr_lims <- do.call(rbind, cfr_lims)

    # assign to matrix
    severity_estimates[indices, c("severity_low", "severity_high")] <- cfr_lims

    # process into a data.frame and return
    # bind single row data.frames and return, convert to data.frame when
    # matrix is returned from no delay correction
    severity_estimates <- as.data.frame(severity_estimates)
  }

  # add date
  severity_estimates$date <- data$date

  # return severity estimate with names in correct order
  severity_estimates[, c(
    "date", "severity_estimate", "severity_low", "severity_high"
  )]
}
