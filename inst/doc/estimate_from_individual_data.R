## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300,
  out.width = "100%"
)

## ----message = FALSE, warning=FALSE, eval = TRUE------------------------------
# load {cfr}
library(cfr)

# packages to wrangle and plot data
library(dplyr)
library(magrittr)
library(ggplot2)
library(tidyr)
library(incidence2)

## -----------------------------------------------------------------------------
# Simulate data
nn <- 1e3 # Number of cases to simulate
pp <- 0.1 # Assumed CFR
set.seed(10) # Set seed for reproducibility

# Generate random case onset timings in Jan & Feb 2024
case_onsets <- as.Date("2024-01-01") + sample.int(60, nn, replace = TRUE)

# Define current date of data availability (i.e. max follow up)
max_obs <- as.Date("2024-01-20")

# Sample delays from onset to outcome

# 1. Deaths: assume mean delay = 5 days, sd = 5 days
log_param <- c(meanlog = 1.262864, sdlog = 0.8325546)
delay_death <- function(x) {
  dlnorm(x,
    meanlog = log_param[["meanlog"]],
    sdlog = log_param[["sdlog"]]
  )
}
outcome_death <- round(rlnorm(round(nn * pp),
  meanlog = log_param[["meanlog"]],
  sdlog = log_param[["sdlog"]]
))

# 2. Recoveries: assume mean delay = 15 days, sd = 5 days
log_param <- c(meanlog = 2.65537, sdlog = 0.3245928)
outcome_recovery <- round(rlnorm(round(nn * (1 - pp)),
  meanlog = log_param[["meanlog"]],
  sdlog = log_param[["sdlog"]]
))

# Create vector of outcome dates
all_outcomes <- case_onsets + c(outcome_death, outcome_recovery)

# Create vector of outcome types
type_outcome <- c(rep("D", length(outcome_death)),
                  rep("R", length(outcome_recovery)))

# Create vector of known outcomes as of max_obs
known_outcomes <- type_outcome
known_outcomes[(all_outcomes > max_obs)] <- ""

## ----fig.cap = "Individual level timings of onsets and outcomes, for individuals with a known outcome as of 20th Jan 2024.", class.source = 'fold-hide'----
# Create a data frame with onset and outcome dates, and outcome type
data <- data.frame(
  id = 1:nn,
  case_onsets = case_onsets,
  outcome_dates = all_outcomes,
  outcome_type = type_outcome,
  known_outcome = known_outcomes
)

# Filter out unknown outcomes (after the max_obs date)
data <- data %>% filter(nzchar(known_outcome))

# Arrange data by onset date
data <- data %>%
  arrange(case_onsets) %>%
  mutate(id_ordered = row_number()) # Assign a new 'id_ordered'

# Prepare data for plotting (onset and outcome events in same column)
plot_data <- data %>%
  tidyr::pivot_longer(
    cols = c(case_onsets, outcome_dates),
    names_to = "event_type",
    values_to = "date"
  ) %>%
  mutate(
    event_label = ifelse(event_type == "case_onsets", "Onset", "Outcome")
  )

# Create plot with lines linking onset and outcome
ggplot() +
  geom_segment(
    data = data, aes(x = case_onsets,
                     xend = outcome_dates,
                     y = id_ordered,
                     yend = id_ordered),
    color = ifelse(data$outcome_type == "D", "red", "green"),
    size = 0.5
  ) +
  geom_point(data = plot_data, aes(x = date,
                                   y = id_ordered,
                                   color = outcome_type,
                                   shape = event_label),
             size = 2) +
  geom_vline(xintercept = as.numeric(max_obs),
             linetype = "dashed",
             color = "black",
             size = 0.5) +
  scale_color_manual(values = c(D = "darkred", R = "darkgreen")) +
  scale_shape_manual(values = c(Onset = 16, Outcome = 17)) +
  labs(
    x = "Date",
    y = "Individual (Ordered by onset date)",
    color = "Outcome type",
    shape = "Event type"
  ) +
  theme_minimal()

## -----------------------------------------------------------------------------
# Filter on known outcomes
total_deaths <- sum(known_outcomes == "D")
total_outcomes <- (total_deaths + sum(known_outcomes == "R"))

# Calculate CFR with 95% CI
cfr_filter <- binom.test(total_deaths, total_outcomes)
cfr_estimate <- (signif(as.numeric(c(cfr_filter$estimate,
                                     cfr_filter$conf.int)), 3))
cfr_estimate

## -----------------------------------------------------------------------------
# Get times of death for fatal outcomes
death_times <- (case_onsets)[type_outcome == "D"] + outcome_death

# Create a single data.frame with event types
events <- data.frame(
  dates = c(case_onsets, death_times),
  event = c(rep("cases", length(case_onsets)),
            rep("deaths", length(death_times)))
)

# Use incidence2 to calculate counts of cases and deaths by day
counts <- incidence2::incidence(events,
                                date_index = "dates",
                                groups = "event",
                                complete_dates = TRUE)

# Pivot incidence object to get data.frame with counts for cases and deaths
df <- counts %>%
  tidyr::pivot_wider(names_from = event,
                     values_from = count,
                     values_fill = 0) %>%
  dplyr::rename(date = date_index)

cfr_static(df, delay_death)

## -----------------------------------------------------------------------------
# Calculate total deaths and total cases
total_deaths <- sum(df$deaths)
total_cases <- sum(df$cases)

# Create data.frame with cases over time only
df_case <- df
df_case$deaths <- 0

# Calculate the expected number of known fatal outcomes over time
e_outcomes <- estimate_outcomes(df_case, delay_death)

# Calculate the CFR
total_deaths / (total_cases * tail(e_outcomes$u_t, 1))

## -----------------------------------------------------------------------------
# Create data.frame with cases over time only
df_cases <- df
df_cases$deaths <- 0

# Add total deaths to the final row in the 'deaths' column
df_cases$deaths[nrow(df_cases)] <- sum(df$deaths)

# Calculate CFR
cfr_static(df_cases, delay_death)

