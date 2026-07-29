## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## -----------------------------------------------------------------------------
library(koma)

## -----------------------------------------------------------------------------
equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1) + exports_level.L(1) + world_gdp_level.L(1) + exchange_rate_level.L(1),
imports ~ exports + consumption + investment + imports.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment,
exports_level == 1*exports + 1*exports_level.L(1),
world_gdp_level == 1*world_gdp + 1*world_gdp_level.L(1),
exchange_rate_level == 1*exchange_rate + 1*exchange_rate_level.L(1)"

exogenous_variables <- c("world_gdp", "exchange_rate")

## ----collapse=TRUE------------------------------------------------------------
sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

print(sys_eq)

## -----------------------------------------------------------------------------
?small_open_economy

## -----------------------------------------------------------------------------
ts_data <- small_open_economy[c(
    "consumption", "investment", "exports", "imports",
    "gdp", "domestic_demand", "world_gdp", "exchange_rate"
)]

series <- names(ts_data)
ts_data <- lapply(series, function(x) {
    as_ets(ts_data[[x]],
        series_type = "level", method = "diff_log"
    )
})
names(ts_data) <- series

ts_data$exports_level <- as_ets(log(ts_data$exports) * 100,
    series_type = "level", method = "none"
)
ts_data$world_gdp_level <- as_ets(log(ts_data$world_gdp) * 100,
    series_type = "level", method = "none"
)
ts_data$exchange_rate_level <- as_ets(log(ts_data$exchange_rate) * 100,
    series_type = "level", method = "none"
)

## -----------------------------------------------------------------------------
dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2024, 4))
)

## ----collapse=TRUE------------------------------------------------------------
estimates <- estimate(
    sys_eq,
    ts_data = ts_data,
    dates = dates
)
print(estimates)

## ----collapse=TRUE------------------------------------------------------------
summary(estimates, variables = "exports")

## -----------------------------------------------------------------------------
ecm_stats <- summary(estimates, variables = "exports")
ecm_coef <- ecm_stats[["exports"]]@coef
adjustment_speed <- ecm_coef["exports_level.L(1)"]
long_run_world_gdp <- -ecm_coef["world_gdp_level.L(1)"] / adjustment_speed
long_run_exchange_rate <- -ecm_coef["exchange_rate_level.L(1)"] / adjustment_speed

sprintf(
    "Adjustment speed: %.3f, Long-run world GDP: %.3f, Exchange rate: %.3f",
    adjustment_speed,
    long_run_world_gdp,
    long_run_exchange_rate
)

## ----collapse=TRUE------------------------------------------------------------
estimates$ts_data[sys_eq$endogenous_variables] <-
    lapply(sys_eq$endogenous_variables, function(x) {
        stats::window(estimates$ts_data[[x]], end = c(2022, 4))
    })

forecasts <- forecast(
    estimates,
    dates = dates
)
print(forecasts)

## -----------------------------------------------------------------------------
rate(forecasts$mean$exports)
level(forecasts$mean$exports)

