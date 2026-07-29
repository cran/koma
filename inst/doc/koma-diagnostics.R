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
gdp == (consumption/gdp)*consumption + (investment/gdp)*investment"

exogenous_variables <- c("interest_rate")

sys_eq <- system_of_equations(
    equations = equations,
    exogenous_variables = exogenous_variables
)

dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4))
)

## ----eval=FALSE---------------------------------------------------------------
# data("small_open_economy")
# 
# series <- unique(c(sys_eq$endogenous_variables, sys_eq$exogenous_variables))
# ts_data <- small_open_economy[series]
# 
# ts_data <- lapply(ts_data, function(x) {
#     as_ets(x, series_type = "level", method = "diff_log")
# })
# ts_data$interest_rate <- as_ets(
#     ts_data$interest_rate,
#     series_type = "rate",
#     method = "none"
# )
# 
# set.seed(123)
# estimates <- estimate(
#   ts_data = ts_data,
#   sys_eq = sys_eq,
#   dates = dates,
#   options = list(gibbs = list(ndraws = 200))
# )

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#     trace_plot(
#         estimates,
#         variables = c("consumption", "investment"),
#         params = c("beta", "gamma"),
#         thin = 2,
#         max_draws = 100
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#     running_mean_plot(
#         estimates,
#         variables = c("consumption", "investment"),
#         params = c("beta", "gamma"),
#         grace_draws = 25
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE)) {
#     acf_plot(
#         estimates,
#         variables = c("consumption", "investment"),
#         params = c("beta", "gamma"),
#         max_lag = 20
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# if (requireNamespace("ggplot2", quietly = TRUE) &&
#     requireNamespace("plotly", quietly = TRUE)) {
#     acf_plot(
#         estimates,
#         variables = "consumption",
#         params = "beta",
#         interactive = TRUE
#     )
# }

