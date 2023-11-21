## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  fig.width = 5, fig.height = 3
)

## ----message = FALSE, warning=FALSE, eval = TRUE------------------------------
# load {cfr} and data packages
library(cfr)

# packages to wrangle and plot data
library(dplyr)
library(tidyr)
library(purrr)
library(scales)
library(forcats)
library(ggplot2)

## -----------------------------------------------------------------------------
# get Covid data provided with the package
data("covid_data")

# filter for the U.K
df_covid_uk <- filter(
  covid_data,
  country == "United Kingdom", date <= "2020-06-30"
)

# view the data format
tail(df_covid_uk)

## -----------------------------------------------------------------------------
# static ascertainment on data
estimate_ascertainment(
  data = df_covid_uk,
  delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440),
  severity_baseline = 0.014
)

## ----warning=FALSE------------------------------------------------------------
# countries with weekly reporting
weekly_reporting <- c("France", "Germany", "Spain", "Ukraine")

# subset for early covid outbreaks
covid_data_early <- filter(
  covid_data, date < "2020-06-01",
  !country %in% weekly_reporting
)

# nest the data
df_reporting <- nest(covid_data_early, .by = country)

# define density function
delay_density <- function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440)

# calculate the reporting rate in each country using
# map on nested dataframes
df_reporting <- mutate(
  df_reporting,
  reporting = map(
    .x = data, .f = estimate_ascertainment,
    # arguments to function
    severity_baseline = 0.014,
    delay_density = delay_density
  )
)

# unnest the data
df_reporting <- unnest(df_reporting, cols = "reporting")

# visualise the data
head(df_reporting)

## ----fig.cap = "Example plot of the ascertainment ratio by country during the early stages of the Covid-19 pandemic.", class.source = 'fold-hide'----
df_reporting %>%
  ggplot() +
  geom_pointrange(
    aes(
      x = fct_reorder(country, ascertainment_mean),
      y = ascertainment_mean,
      ymin = ascertainment_low,
      ymax = ascertainment_high
    )
  ) +
  coord_flip() +
  labs(x = NULL, y = "Ascertainment ratio") +
  theme(legend.position = "none") +
  scale_y_continuous(
    labels = percent, limits = c(0, 1)
  ) +
  theme_classic() +
  theme(legend.position = "top")

