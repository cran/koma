## -----------------------------------------------------------------------------
equations <- "consumption ~ profits + profits.L(1) + total_wages,
investment ~ profits + profits.L(1) + capital_stock.L(1),
wages ~ gdp + gdp.L(1) + time_trend,
gdp == (n_consumption/n_gdp)*consumption + (n_investment/n_gdp)*investment + (n_government/n_gdp)*government,
profits == (n_gdp/n_profits)*gdp - (n_taxes/n_profits)*taxes - (n_wages/n_profits)*wages,
capital_stock == 1.001*capital_stock.L(1) + 0.0002*investment,
total_wages == (n_wages/n_total_wages)*wages + (n_government_wages/n_total_wages)*government_wages"

exogenous_variables <- c("government", "government_wages", "taxes", "time_trend")

## ----collapse=TRUE------------------------------------------------------------
library(koma)

sys_eq <- system_of_equations(
  equations = equations,
  exogenous_variables = exogenous_variables
)

print(sys_eq)

## -----------------------------------------------------------------------------
dates <- list(
  estimation = list(start = c(1990, 1), end = c(2020, 4)),
  forecast = list(start = c(2021, 1), end = c(2023, 4)),
  dynamic_weights = list(start = c(1990, 1), end = c(2020, 4))
)

## -----------------------------------------------------------------------------
?klein

## -----------------------------------------------------------------------------
# Some series in the klein dataset are nominal. We need to deflate them using the GDP deflator.
klein$profits <- klein$n_profits / (klein$d_gdp / 100)
klein$capital_stock <- klein$n_capital_stock / (klein$d_gdp / 100)
klein$wages <- klein$n_wages / (klein$d_gdp / 100)
klein$government_wages <- klein$n_government_wages / (klein$d_gdp / 100)
klein$taxes <- klein$n_taxes / (klein$d_gdp / 100)

real_series <- c(
  "gdp", "consumption", "investment", "profits", "capital_stock",
  "government", "wages", "government_wages", "taxes"
)
real_ts_data <- lapply(real_series, function(x) {
  as_ets(klein[[x]],
    series_type = "level", method = "diff_log", value_type = "real"
  )
})
names(real_ts_data) <- real_series

real_ts_data$total_wages <- real_ts_data$government_wages + real_ts_data$wages

# net exports can be negative, we thus want to use percentage change instead of log difference
real_ts_data$net_exports <- as_ets(klein$net_exports,
  series_type = "level", method = "percentage", value_type = "real"
)

# To compute the weights in the model, we include the nominal series in the dataset as well.
nominal_series <- c(
  "n_gdp", "n_investment", "n_government", "n_government_wages",
  "n_taxes", "n_profits", "n_wages", "n_consumption"
)
nominal_ts_data <- lapply(nominal_series, function(x) {
  as_ets(klein[[x]],
    series_type = "level", method = "diff_log", value_type = "nominal"
  )
})
names(nominal_ts_data) <- nominal_series

nominal_ts_data$n_total_wages <- nominal_ts_data$n_government_wages + nominal_ts_data$n_wages

# Combining the real and nominal series
ts_data <- c(real_ts_data, nominal_ts_data)

# Adding the time trend
ts_data$time_trend <- ets(seq_len(length(klein$taxes)),
  start = stats::start(ts_data$taxes), frequency = 4,
  series_type = "level", method = "none", value_type = NA
)

## -----------------------------------------------------------------------------
ts_data[sys_eq$endogenous_variables] <-
  lapply(sys_eq$endogenous_variables, function(x) {
    stats::window(ts_data[[x]], end = c(2020, 4))
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
forecasts <- forecast(
  estimates,
  dates = dates
)
print(forecasts)

## ----eval=FALSE---------------------------------------------------------------
# plot(forecasts, variables = "gdp")

