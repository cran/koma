## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(koma)

## -----------------------------------------------------------------------------
set.seed(123)

draws <- c(
    rnorm(2000, mean = -2, sd = 0.3),
    rnorm(2000, mean = 2, sd = 0.3)
)
plot(stats::density(draws))

hdr_res <- hdr(
    draws,
    probs = c(0.5, 0.99),
    integration = "grid"
)

hdi_res <- hdi(
    draws,
    probs = c(0.5, 0.99)
)

hdr_res$intervals$level_99
hdi_res$intervals$level_99

## -----------------------------------------------------------------------------
hdr_tuned <- hdr(
    draws,
    probs = 0.9,
    bw = "nrd0",
    adjust = 1.2,
    kernel = "gaussian",
    integration = "monte_carlo",
    mc_use_observed = TRUE,
    mc_quantile_type = 7
)

## ----eval = FALSE-------------------------------------------------------------
# # Define a small system
# 
# equations <- "y ~ x + y.L(1)"
# exogenous_variables <- "x"
# 
# sys_eq <- system_of_equations(
#     equations = equations,
#     exogenous_variables = exogenous_variables
# )
# 
# # Simulate data (small sample for speed)
# 
# gamma_matrix <- matrix(1, nrow = 1)
# beta_matrix <- matrix(c(0.2, 0.5, 0.3), nrow = 3)
# sigma_matrix <- matrix(0.01, nrow = 1)
# 
# sample <- generate_sample_data(
#     sample_size = 80,
#     sample_start = c(2000, 1),
#     burnin = 20,
#     gamma_matrix = gamma_matrix,
#     beta_matrix = beta_matrix,
#     sigma_matrix = sigma_matrix,
#     endogenous_variables = "y",
#     exogenous_variables = "x",
#     predetermined_variables = "y.L(1)"
# )
# 
# ts_data <- lapply(sample$ts_data, function(x) {
#     as_ets(x, series_type = "rate", method = "diff_log")
# })
# 
# # Estimate with fewer draws for a quick example
# 
# dates <- list(
#     estimation = list(start = c(2000, 3), end = c(2019, 4))
# )
# 
# estimates <- estimate(
#     ts_data,
#     sys_eq,
#     dates,
#     options = list(gibbs = list(ndraws = 400, burnin_ratio = 0.5, nstore = 1))
# )
# 
# # HDRs from coefficient draws
# 
# hdr_est <- hdr(estimates, probs = c(0.5, 0.99))
# summary(hdr_est)
# 
# # HDIs from coefficient draws
# 
# hdi_est <- hdi(estimates, probs = c(0.5, 0.99))
# summary(hdi_est)
# 
# # Example: inspect the 99% beta interval for x in equation y
# hdi_est$intervals$y$beta$x$level_99

## ----eval = FALSE-------------------------------------------------------------
# plot(hdr_est)

