library(tidymodels)
library(modeldata)
library(forecast)  
library(timetk)    
library(zoo)
library(sweep)

# From https://www.tidymodels.org/learn/models/time-series/


glimpse(drinks)

roll_rs <- rolling_origin(
  drinks, 
  initial = 12 * 20, 
  assess = 12,
  cumulative = FALSE
)

get_date <- function(x) {
  min(assessment(x)$date)
}

start_date <- map(roll_rs$splits, get_date)
roll_rs$start_date <- do.call("c", start_date)

fit_model <- function(x, ...) {
  # suggested by Matt Dancho:
  x %>%
    analysis() %>%
    # Since the first day changes over resamples, adjust it
    # based on the first date value in the data frame 
    tk_ts(start = .$date[[1]] %>% as.yearmon(), 
          frequency = 12, 
          silent = TRUE) %>%
    auto.arima(...)
}



roll_rs$arima <- map(roll_rs$splits, fit_model)


roll_rs$interpolation <- map_dbl(
  roll_rs$arima,
  function(x) 
    sw_glance(x)[["MAPE"]]
)


roll_rs$interpolation


get_extrap <- function(split, mod) {
  n <- nrow(assessment(split))
  # Get assessment data
  pred_dat <- assessment(split) %>%
    mutate(
      pred = as.vector(forecast(mod, h = n)$mean),
      pct_error = ( S4248SM144NCEN - pred ) / S4248SM144NCEN * 100
    )
  mean(abs(pred_dat$pct_error))
}


roll_rs$extrapolation <- 
  map2_dbl(roll_rs$splits, roll_rs$arima, get_extrap)


roll_rs %>%
  select(interpolation, extrapolation, start_date) %>%
  pivot_longer(cols = matches("ation"), names_to = "error", values_to = "MAPE") %>%
  ggplot(aes(x = start_date, y = MAPE, col = error)) + 
  geom_point() + 
  geom_line()

