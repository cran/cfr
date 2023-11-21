## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# load cfr
library(cfr)

## -----------------------------------------------------------------------------
data("ebola1976")

# view ebola dataset
head(ebola1976)

## -----------------------------------------------------------------------------
cfr_static(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

## -----------------------------------------------------------------------------
# estimate reporting with a baseline severity of 70%
estimate_ascertainment(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33),
  severity_baseline = 0.7
)

