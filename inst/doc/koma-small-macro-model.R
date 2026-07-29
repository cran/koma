## -----------------------------------------------------------------------------
equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"

exogenous_variables <- c("world_gdp", "interest_rate_germany", "exchange_rate", "oil_price")

## ----collapse=TRUE------------------------------------------------------------
library(koma)

sys_eq <- system_of_equations(
  equations = equations,
  exogenous_variables = exogenous_variables
)

print(sys_eq)

## -----------------------------------------------------------------------------
dates <- list(
  estimation = list(start = c(1996, 1), end = c(2019, 4)),
  forecast = list(start = c(2023, 1), end = c(2023, 4))
)

## -----------------------------------------------------------------------------
?small_open_economy

## -----------------------------------------------------------------------------
series <- names(small_open_economy)
series <- series[!series %in% c("interest_rate", "interest_rate_germany")]

ts_data <- lapply(series, function(x) {
  as_ets(small_open_economy[[x]],
    series_type = "level", method = "diff_log"
  )
})
names(ts_data) <- series

ts_data$interest_rate <- as_ets(small_open_economy$interest_rate,
  series_type = "rate", method = "none"
)
ts_data$interest_rate_germany <- as_ets(small_open_economy$interest_rate_germany,
  series_type = "rate", method = "none"
)

## -----------------------------------------------------------------------------
ts_data[sys_eq$endogenous_variables] <-
  lapply(sys_eq$endogenous_variables, function(x) {
    stats::window(ts_data[[x]], end = c(2019, 4))
  })

## ----collapse=TRUE------------------------------------------------------------
estimates <- estimate(
  sys_eq,
  ts_data = ts_data,
  dates = dates
)
print(estimates)

## ----collapse=TRUE------------------------------------------------------------
summary(estimates)

## ----collapse=TRUE------------------------------------------------------------
summary(estimates, variables = "investment")

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   trace_plot(estimates, variables = c("consumption", "investment"))
# }

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   running_mean_plot(estimates, variables = c("consumption", "investment"))
# }

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#   acf_plot(estimates, variables = c("consumption", "investment"))
# }

## ----collapse=TRUE------------------------------------------------------------
forecasts <- forecast(
  estimates,
  dates = dates
)
print(forecasts)

## ----collapse=TRUE------------------------------------------------------------
rate(forecasts$mean$gdp)
level(forecasts$mean$gdp)

## ----eval=FALSE---------------------------------------------------------------
# fig <- plot(forecasts, variables = "prices")
# fig
# 
# restrictions <- list(prices = list(value = 0.5, horizon = 1))
# 
# forecasts <- forecast(estimates,
#   dates = dates,
#   restrictions = restrictions
# )
# plot(forecasts, fig = fig, variables = "prices")
# forecasts$mean$prices

