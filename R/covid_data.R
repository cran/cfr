#' Daily Covid-19 case and death data for countries with 100,000 or more deaths
#'
#' Data adapted from the \{covidregionaldata\} package of daily cases and
#' deaths from the 19 countries with 100,000 or more deaths over the period
#' 2020-01-01 to 2022-12-31. See the **References** for the publication which
#' links to data sources made available through \{covidregionaldata\}.
#' Included as \{covidregionaldata\} is no longer on CRAN.
#' Data are provided as a `<data.frame>`.
#'
#' @format ## `covid_data`
#' A `<data.frame>` with 20,786 rows and 4 columns:
#' \describe{
#'   \item{date}{Calendar date in the format %Y-%m-%d}
#'   \item{country}{The country name in simple format, e.g. "United States"
#' rather than "United States of America"}
#'   \item{cases}{Number of cases reported on each date}
#'   \item{deaths}{Number of deaths reported on each date}
#' }
#' @source \doi{10.21105/joss.03290}.
#' @references
#' Joseph Palmer, Katharine Sherratt, Richard Martin-Nielsen, Jonnie Bevan,
#' Hamish Gibbs, Sebastian Funk and Sam Abbott (2021). covidregionaldata:
#' Subnational data for COVID-19 epidemiology. \doi{10.21105/joss.03290}
"covid_data"
