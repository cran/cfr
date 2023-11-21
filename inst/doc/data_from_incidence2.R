## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(cfr)

# load {incidence2}
library(incidence2)

## -----------------------------------------------------------------------------
# get data bundled with the {incidence2} package
covid_uk <- covidregionaldataUK

# view the data
head(covid_uk)

## -----------------------------------------------------------------------------
# convert to incidence2 object
covid_uk_incidence <- incidence(
  covid_uk,
  date_index = "date",
  groups = "region",
  counts = c("cases_new", "deaths_new"),
  count_names_to = "count_variable"
)

# View head of prepared data with NAs retained
# Note that this will cause issues with CFR functions such as cfr_static()
head(
  prepare_data(
    covid_uk_incidence,
    cases_variable = "cases_new",
    deaths_variable = "deaths_new"
  )
)

