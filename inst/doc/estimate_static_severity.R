## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  fig.width = 5, fig.height = 3
)

## ----message = FALSE, warning=FALSE, eval = TRUE------------------------------
library(cfr)

# packages to wrangle and plot data
library(dplyr)

## ----message = FALSE, warning = FALSE, eval = TRUE----------------------------
data("ebola1976")

# view the first few rows
head(ebola1976)

df_ebola_subset <- filter(ebola1976, date <= "1976-09-30")

## -----------------------------------------------------------------------------
# calculate known death outcomes
df_estimated_outcomes_ebola <- estimate_outcomes(
  data = df_ebola_subset,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

# print head of data frame
head(df_estimated_outcomes_ebola)

# print tail of data frame
tail(df_estimated_outcomes_ebola)

## ----message = FALSE, warning = FALSE, eval = TRUE----------------------------
# calculating the naive CFR
cfr_static(
  data = df_ebola_subset
)

# calculating the corrected CFR
cfr_static(
  df_ebola_subset,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

## -----------------------------------------------------------------------------
# get Covid data loaded with the package
data("covid_data")

# filter for the U.K
df_covid_uk <- filter(
  covid_data,
  country == "United Kingdom", date <= "2020-12-31"
)

# View the first few rows and recall necessary columns: date, cases, deaths
head(df_covid_uk)

## ----message = FALSE, warning = FALSE, eval = TRUE----------------------------
# calculating the naive CFR
cfr_static(
  df_covid_uk
)

# calculating the corrected CFR
cfr_static(
  df_covid_uk,
  delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440)
)

