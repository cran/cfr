## ----include = FALSE----------------------------------------------------------
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
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)

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

## -----------------------------------------------------------------------------
# calculating the naive time-varying CFR
df_covid_cfr_uk_naive <- cfr_time_varying(
  df_covid_uk,
  burn_in = 7,
  smoothing_window = 7
)

# calculating the corrected time-varying CFR
df_covid_cfr_uk_corrected <- cfr_time_varying(
  df_covid_uk,
  delay_density = function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440),
  burn_in = 7,
  smoothing_window = 7
)

# assign method tag and plot
df_covid_cfr_uk_naive$method <- "naive"
df_covid_cfr_uk_corrected$method <- "corrected"

df_covid_cfr_uk <- bind_rows(df_covid_cfr_uk_naive, df_covid_cfr_uk_corrected)

## ----fig.cap = "Example plot of the naive time-varying CFR. We calculate the time-varying CFR for the Covid-19 pandemic in the U.K., uncorrected for delays. The red line shows the CFR estimate while the shaded grey region shows the lower and upper limits of the estimate as 95% confidence intervals.", class.source = 'fold-hide'----
ggplot(df_covid_cfr_uk) +
  geom_ribbon(
    aes(
      x = date, ymin = severity_low, ymax = severity_high,
      fill = method
    ),
    alpha = 0.5
  ) +
  geom_line(
    aes(
      x = date, y = severity_mean, colour = method
    )
  ) +
  scale_x_date(
    date_labels = "%b-%Y"
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  scale_fill_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Naive CFR", "Corrected CFR")
  ) +
  scale_colour_brewer(
    palette = "Dark2",
    name = NULL,
    labels = c("Naive CFR", "Corrected CFR")
  ) +
  labs(
    x = "Date", y = "CFR (%)"
  ) +
  coord_cartesian(
    expand = FALSE
  ) +
  theme_classic() +
  theme(legend.position = "top")

## -----------------------------------------------------------------------------
# countries with weekly reporting
weekly_reporting <- c("France", "Germany", "Spain", "Ukraine")
covid_data <- filter(covid_data, !country %in% weekly_reporting)

# for each country, get the time-varying severity estimate,
# correcting for delays and smoothing the case and death data

# first nest the data; nest() from {tidyr}
df_covid_cfr <- nest(
  covid_data,
  .by = country
)

## -----------------------------------------------------------------------------
# define delay density function
delay_density <- function(x) dlnorm(x, meanlog = 2.577, sdlog = 0.440)

# to each nested data frame, apply the function `cfr_time_varying`
# overwrite the `data` column, as all data will be preserved
df_covid_cfr <- mutate(
  df_covid_cfr,
  # using map() from {purrr}
  data = map(
    .x = data, .f = cfr_time_varying,
    # arguments to the function
    delay_density = delay_density,
    smoothing_window = 7, burn_in = 7
  )
)

# unnest the cfr data; unnest() from {tidyr}
df_covid_cfr <- unnest(df_covid_cfr, cols = data)

## ----fig.cap = "Example plot of the corrected time-varying CFR. We calculate the time-varying CFR for the Covid-19 pandemic in Brazil, India, and the United States, corrected for delays.", class.source = 'fold-hide', fig.width=8----
filter(df_covid_cfr, country %in% c("Brazil", "India", "United States")) %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = date, ymin = severity_low, ymax = severity_high,
      group = country
    ),
    fill = "grey"
  ) +
  geom_line(
    aes(
      x = date, y = severity_mean, colour = country
    )
  ) +
  scale_x_date(
    date_labels = "%b-%Y"
  ) +
  scale_y_continuous(
    labels = percent
  ) +
  scale_colour_brewer(
    palette = "Dark2"
  ) +
  labs(
    x = "Date", y = "CFR (%)"
  ) +
  coord_cartesian(
    ylim = c(0, 0.15),
    expand = FALSE
  ) +
  theme_classic() +
  theme(legend.position = "top")

