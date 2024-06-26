#' Calculate expected outcomes
#'
#' @description
#' An internal function to calculate the number of outcomes expected given
#' a time-series of cases and probability density or mass functions evaluated
#' at relevant time points.
#'
#' @param cases A numeric vector of the time-series of cases.
#' @param pmf_vals A numeric vector of the probability mass function or
#' probability density function of a reporting delay distribution evaluated at
#' each time point represented in `cases`.
#' @keywords internal
#' @noRd
#' @return A numeric vector of the number of outcomes expected.
#'
.convolve_cases_pmfs <- function(cases, pmf_vals) {
  # no input checks as this is an internal function
  vapply(
    X = seq_along(cases),
    FUN = function(i) {
      # estimate expected number of outcomes
      sum(cases[seq_len(i)] * rev(pmf_vals[seq_len(i)]))
    },
    FUN.VALUE = numeric(1)
  )
}

#' @title Estimate known outcomes of cases using a delay distribution
#'
#' @description Estimates the expected number of individuals with known outcomes
#' from a case and outcome time series of outbreak data, and an epidemiological
#' delay distribution of symptom onset to outcome.
#' When calculating a case fatality risk, the outcomes must be deaths, the delay
#' distribution must be an onset-to-death distribution, and the function returns
#' estimates of the known death outcomes.
#'
#' @inheritParams cfr_static
#'
#' @return A `<data.frame>` with the columns in `data`, and with two additional
#' columns:
#'
#'  - `"estimated_outcomes"` for the number of cases with an outcome of interest
#' (usually, death) estimated to be known on the dates specified in `data`, and
#'
#'  - `u_t` for the ratio of cumulative number of estimated known outcomes
#' and the cumulative number of cases reported until each date specified in
#' `data`.
#'
#' @details
#'
#' The ratio `u_t` represents, for the outbreak, the overall proportion of
#' cases whose outcomes are expected to be known by each day $i$. For an ongoing
#' outbreak with relatively long delays between symptom onset and case outcome,
#' a `u_t` value of 1.0 may indicate that the outbreak has ended, as the
#' outcomes of all cases are expected to be known.
#'
#' @export
#'
#' @examples
#' # Load Ebola 1976 outbreak data
#' data("ebola1976")
#'
#' # estimate severity for each day while correcting for delays
#' # obtain onset-to-death delay distribution parameters from Barry et al. 2018
#' # examine the first few rows of the output
#' estimated_outcomes <- estimate_outcomes(
#'   data = ebola1976,
#'   delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
#' )
#'
#' head(estimated_outcomes)
estimate_outcomes <- function(data,
                              delay_density) {
  # some input checking; this function is mainly called internally
  # but currently exported
  # input checking is a candidate for removal
  checkmate::assert_data_frame(
    data,
    min.rows = 1, min.cols = 2
  )
  checkmate::assert_names(
    names(data),
    must.include = c("cases", "date")
  )
  # check for any NAs among data
  checkmate::assert_data_frame(
    data[, c("cases", "deaths")],
    any.missing = FALSE
  )
  stopifnot(
    "`delay_density` must be a function with a single required argument,
    and evaluating distribution density at a vector of values and returning a
    numeric vector of the same length.
    E.g. function(x) stats::dgamma(x = x, shape = 5, scale = 1)" =
      (test_fn_req_args(delay_density) &&
        test_fn_num_out(delay_density)) || is.null(delay_density)
  )

  pmf_vals <- delay_density(seq(from = 0, to = nrow(data) - 1L))

  # calculate expected outcomes
  # NOTE: assumes daily data, which is checked in higher level functions
  data$estimated_outcomes <- .convolve_cases_pmfs(data$cases, pmf_vals)

  # calculate u_t as ratio of estimated outcomes to reported cases
  # on each day
  data$u_t <- cumsum(data$estimated_outcomes) / cumsum(data$cases)

  # replace u_t that is NaN with NA (due to zero division)
  data$u_t[is.nan(data$u_t)] <- NA_real_

  # return dataframe with added columns
  data
}
