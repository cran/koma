## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(koma)

## ----install-future-parallelly, eval=FALSE------------------------------------
# install.packages("future")
# install.packages("parallelly")

## ----model-setup--------------------------------------------------------------
library(koma)

equations <- "consumption ~ gdp + consumption.L(1),
investment ~ investment.L(1),
exports ~ world_gdp + exports.L(1),
imports ~ domestic_demand + imports.L(1),
prices ~ exchange_rate + oil_price + prices.L(1),
interest_rate ~ prices + interest_rate_germany + prices.L(1),
gdp == 0.6*consumption + 0.6*domestic_demand + 0.5*exports - 0.4*imports,
domestic_demand == 0.6*consumption + 0.4*investment"

exogenous_variables <- c("world_gdp", "interest_rate_germany", "exchange_rate", "oil_price")

dates <- list(
    estimation = list(start = c(1996, 1), end = c(2019, 4)),
    forecast = list(start = c(2023, 1), end = c(2023, 4))
)

sys_eq <- system_of_equations(equations, exogenous_variables)

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

## ----sequential-exec, eval=FALSE----------------------------------------------
# estimates <- estimate(ts_data, sys_eq, dates)

## ----workers, eval=FALSE------------------------------------------------------
# # Get the available workers
# workers <- parallelly::availableCores(omit = 1)

## ----multisession, eval=FALSE-------------------------------------------------
# future::plan("future::multisession", workers = workers)
# 
# estimates <- estimate(ts_data, sys_eq, dates)

## ----multicore, eval=FALSE----------------------------------------------------
# future::plan("future::multicore", workers = workers)
# 
# estimates <- estimate(ts_data, sys_eq, dates)

## ----multisession-macos, eval=FALSE-------------------------------------------
# future::plan("future::multisession", workers = workers)
# 
# estimates <- estimate(ts_data, sys_eq, dates)

