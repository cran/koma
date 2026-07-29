## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(koma)

## -----------------------------------------------------------------------------
equations <- "consumption ~ gdp + consumption.L(1) + interest_rate,
investment ~ gdp + investment.L(1) + interest_rate,
exports ~ world_gdp + exchange_rate + exports.L(1),
imports ~ gdp + exchange_rate + imports.L(1),
gdp == 0.55*consumption + 0.20*investment + 0.30*exports - 0.05*imports"

exogenous_variables <- c("interest_rate", "world_gdp", "exchange_rate")

## -----------------------------------------------------------------------------
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
data("small_open_economy")
series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
ts_data <- small_open_economy[series]

## ----eval=FALSE---------------------------------------------------------------
# estimates <- estimate(ts_data, sys_eq, dates)
# #> Some of the time series in `ts_data` are not `ets`.
# #> They will be automatically converted with `as_ets` using the defaults:
# #> Default settings
# #> series_type = level
# #> method = percentage
# #> Are these correct? (y/n): n
# #> Enter series_type: level
# #> Enter method: diff_log
# #> Specify exception to default settings? (y/n): y
# #> Enter series name: interest_rate
# #> Enter series_type for interest_rate (default level): rate
# #> Enter method for interest_rate (default diff_log): none
# #> Specify exception to default settings? (y/n): n

## -----------------------------------------------------------------------------
ts_data <- lapply(ts_data, function(x) {
    as_ets(x, series_type = "level", method = "diff_log")
})
ts_data$interest_rate <- as_ets(
    ts_data$interest_rate,
    series_type = "rate",
    method = "none"
)

## -----------------------------------------------------------------------------
estimates <- estimate(
    ts_data,
    sys_eq,
    dates
)

print(estimates)
summary(estimates)

## -----------------------------------------------------------------------------
estimates$ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(estimates$ts_data[[x]], end = c(2022, 4))
    })

## -----------------------------------------------------------------------------
forecasts <- forecast(estimates, dates)
print(forecasts)

rate(forecasts$mean$gdp)
level(forecasts$mean$gdp)

## -----------------------------------------------------------------------------
summary(forecasts)
summary(forecasts, variables = "gdp", horizon = 2)

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("plotly", quietly = TRUE)) {
#     plot(forecasts, variables = c("gdp", "consumption"))
# }

