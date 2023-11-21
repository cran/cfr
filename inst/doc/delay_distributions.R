## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# load necessary packages
library(cfr)

# some distribution packages
library(distributional)
library(distcrete)

## -----------------------------------------------------------------------------
# the probability density function at `x` for a Gamma distribution
dgamma(x = seq(10), shape = 5, rate = 1)

## -----------------------------------------------------------------------------
# wrap stats::dgamma() in a function
# the Gamma distribution parameters are contained within dens_gamma()
dens_gamma <- function(x) {
  stats::dgamma(x = x, shape = 5, scale = 1)
}

# check over a vector of `x`
dens_gamma(x = seq(10))

## -----------------------------------------------------------------------------
# load package data
data("ebola1976")

# pass function wrapping dgamma to cfr_static()
cfr_static(
  data = ebola1976,
  delay_density = function(x) dgamma(x, shape = 2.40, scale = 3.33)
)

## -----------------------------------------------------------------------------
# using {distributional} and parameters from Barry et al. 2018
dist_onset_to_death_ebola <- dist_gamma(shape = 2.40, rate = 1.0 / 3.33)

# wrap function and pass it to cfr_static()
# unlist() required as density(<distribution>, x) is a list
cfr_static(
  data = ebola1976,
  delay_density = function(x) unlist(density(dist_onset_to_death_ebola, x))
)

## -----------------------------------------------------------------------------
# using {distcrete} and parameters from Barry et al. 2018
dist_onset_to_death_ebola <- distcrete(
  name = "gamma", shape = 2.40, scale = 3.33, interval = 1
)

# pass density function to cfr_static()
cfr_static(
  data = ebola1976,
  delay_density = dist_onset_to_death_ebola$d
)

