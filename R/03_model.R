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
#
# Divergent transitions signal that the sampler can't be trusted (biased
# posterior). If any show up, we automatically refit with a higher
# adapt_delta (smaller step size) and a new seed, up to MAX_REFITS times.

library(brms)
library(dplyr)

MODEL_FORMULA <- log_value ~ rain_decay + log_flow + s(doy, k = 6) + (1 | year)

# ── Helper: count divergent transitions across all chains ──────────────────
count_divergences <- function(fit) {
  np <- nuts_params(fit, pars = "divergent__")
  sum(np$Value)
}

# ── Fit one model ───────────────────────────────────────────────────────────
# grid:   list returned by build_grid(), containing $season_grid
# target: "log_entero" or "log_coli"
# file:   path prefix for caching (NULL = no cache); saved as <file>.rds
fit_bacteria_model <- function(grid, target = "log_entero",
                                chains = 4, iter = 2000, seed = 42,
                                file = NULL,
                                adapt_delta = 0.95,
                                max_adapt_delta = 0.999,
                                max_refits = 5) {

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
    control = list(adapt_delta = adapt_delta),
    file    = file,
    refresh = 200
  )

  n_divergent <- count_divergences(fit)
  refit_count <- 0

  while (n_divergent > 0 && refit_count < max_refits && adapt_delta < max_adapt_delta) {
    refit_count <- refit_count + 1
    adapt_delta <- min(adapt_delta + 0.02, max_adapt_delta)

    message(sprintf(
      "  %d divergent transition(s) detected — refitting (attempt %d/%d, adapt_delta = %.3f)",
      n_divergent, refit_count, max_refits, adapt_delta
    ))

    fit <- update(
      fit,
      control    = list(adapt_delta = adapt_delta),
      seed       = seed + refit_count,
      file       = file,
      file_refit = "always",
      recompile  = FALSE
    )

    n_divergent <- count_divergences(fit)
  }

  if (n_divergent > 0) {
    warning(sprintf(
      "Model for %s still has %d divergent transition(s) after %d refit(s) (adapt_delta = %.3f)",
      target, n_divergent, refit_count, adapt_delta
    ))
  } else if (refit_count > 0) {
    message(sprintf(
      "  Resolved after %d refit(s): 0 divergent transitions (adapt_delta = %.3f)",
      refit_count, adapt_delta
    ))
  }

  list(fit = fit, target = target)
}
