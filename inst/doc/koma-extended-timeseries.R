## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(koma)

## ----create-ets---------------------------------------------------------------
x <- ets(
  data = 1:10,
  start = c(2019, 1),
  frequency = 4,
  series_type = "level",
  value_type = "real",
  method = "diff_log"
)
x

ts_obj <- stats::ts(1:10, start = c(2019, 1), frequency = 4)
y <- as_ets(
  ts_obj,
  series_type = "level",
  value_type = "real",
  method = "diff_log"
)
attr(y, "ets_attributes")

## ----window-extend------------------------------------------------------------
stats::window(x, start = c(2019, 4))

stats::window(x, start = 2018, extend = TRUE)

stats::na.omit(stats::window(x, start = 2018, extend = TRUE))

## ----rates-levels-------------------------------------------------------------
x_rate <- rate(x)
x_rate
attr(x_rate, "anker")

rate_window <- stats::window(x_rate, start = c(2019, 4))
level(rate_window)

## ----rate-lag-----------------------------------------------------------------
x_rate_lag <- lag(x_rate, k = -1)
x_rate_lag
attr(x_rate_lag, "anker")

## ----rebase-------------------------------------------------------------------
rebase(x, start = c(2020, 1), end = c(2020, 1))
rebase(x, start = c(2020, 1), end = c(2020, 4))

## ----temporal-aggregation-----------------------------------------------------
if (requireNamespace("tempdisagg", quietly = TRUE)) {
  tempdisagg::ta(x, conversion = "sum", to = "annual")
}

## ----arithmetic-indexing------------------------------------------------------
log(x)
diff(x)
x * 10
x[1:2]
x / x

## ----plain-ts-conversion------------------------------------------------------
level_series <- stats::ts(c(100, 101, 99, 103, 105, 104, 108, 110),
  start = c(2020, 1), frequency = 4
)
as_ets(level_series, series_type = "level", method = "diff_log")

