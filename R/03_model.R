# 03_model.R
# Fit a Bayesian regression model (brms, Stan backend) for log(bacteria),
# predicting from river flow and rainfall.
#
# Formula:
#   log(bacteria) ~ rain_decay + log_flow + s(doy, k = 6) + (1 | year)
#
#   rain_decay : exponentially-decayed trailing rainfall (short CSO pulses)
#   log_flow   : same-day Vantaanjoki river flow (watershed wetness/dilution)
#   s(doy)     : smooth within-season trend (warmer water -> more growth)
#   (1 | year) : year-to-year variation not explained by flow/rain
#
# Student-t family for robustness to occasional extreme spikes (e.g. >800/>2400
# censored readings). Cached fits: <file>.rds (delete, or change the formula,
# to force a refit).

library(brms)
library(dplyr)

MODEL_FORMULA <- log_value ~ rain_decay + log_flow + s(doy, k = 6) + (1 | year)

# ── Fit one model ───────────────────────────────────────────────────────────
# grid:   list returned by build_grid(), containing $season_grid
# target: "log_entero" or "log_coli"
# file:   path prefix for caching (NULL = no cache); saved as <file>.rds
fit_bacteria_model <- function(grid, target = "log_entero",
                                chains = 4, iter = 2000, seed = 42,
                                file = NULL) {

  train <- grid$season_grid |>
    rename(log_value = all_of(target)) |>
    filter(!is.na(log_value))

  if (nrow(train) == 0) stop("No observed rows for target: ", target)

  priors <- c(
    set_prior("normal(0, 3)", class = "Intercept"),
    set_prior("normal(0, 2)", class = "b"),
    set_prior("student_t(3, 0, 2)", class = "sds"),
    set_prior("student_t(3, 0, 2)", class = "sd")
  )

  fit <- brm(
    MODEL_FORMULA,
    data    = train,
    family  = student(),
    prior   = priors,
    chains  = chains,
    iter    = iter,
    seed    = seed,
    control = list(adapt_delta = 0.95),
    file    = file,
    refresh = 200
  )

  list(fit = fit, target = target)
}
