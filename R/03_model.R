# 03_model.R
# Fit a GP model for log(bacteria) using rstan directly.
#
# Likelihood: Student-t on log-bacteria.
# Formula:    log(bacteria) ~ Intercept + HSGP(t_s) + b_rain * rain_effect(tau)
#
# Key difference from a brms-based model: the rain decay half-life tau is now
# a model parameter estimated from data (rather than fixed at 5 days).
# This requires the rain kernel to be computed inside Stan, which in turn
# requires bypassing brms's formula interface and using a hand-written Stan file.
#
# Stan file: stan/gp_model.stan
# Cached fits: <file>.rds  (delete to force refit)

library(rstan)
library(dplyr)

rstan_options(auto_write = TRUE)  # cache compiled Stan binary next to .stan file

# ── Helper: scale a vector and return attributes for later inversion ───────────
scale_vec <- function(x) {
  m <- mean(x, na.rm = TRUE); s <- sd(x, na.rm = TRUE)
  list(scaled = (x - m) / s, mean = m, sd = s)
}

# ── Fit one GP model ───────────────────────────────────────────────────────────
# grid: list returned by build_grid(), containing $season_grid and $rain_lag
# target: "log_entero" or "log_coli"
# file: path prefix for caching (NULL = no cache); saved as <file>.rds
fit_gp_model <- function(grid, target = "log_entero",
                         chains = 4, iter = 2000, seed = 42,
                         file = NULL) {

  season_grid <- grid$season_grid
  rain_lag    <- grid$rain_lag

  # ── Identify observed rows ────────────────────────────────────────────────
  obs_mask <- !is.na(season_grid[[target]])
  obs_idx  <- which(obs_mask)
  Y        <- season_grid[[target]][obs_mask]

  if (length(obs_idx) == 0) stop("No observed rows for target: ", target)

  # ── Scale time (z-score on training rows, apply to all) ───────────────────
  sc_t <- scale_vec(season_grid$t[obs_mask])
  t_s  <- (season_grid$t - sc_t$mean) / sc_t$sd

  # ── Assemble Stan data ────────────────────────────────────────────────────
  stan_data <- list(
    N        = nrow(season_grid),
    N_obs    = length(obs_idx),
    obs_idx  = obs_idx,
    Y        = Y,
    t_s      = t_s,
    rain_lag = rain_lag
  )

  # ── Cache: load if available ──────────────────────────────────────────────
  rds_path <- if (!is.null(file)) paste0(file, ".rds") else NULL

  if (!is.null(rds_path) && file.exists(rds_path)) {
    message("  Loading cached fit: ", rds_path)
    fit <- readRDS(rds_path)
    return(list(fit = fit, scales = list(t = sc_t), target = target))
  }

  # ── Compile Stan model ────────────────────────────────────────────────────
  stan_file <- file.path(
    dirname(sys.frame(1)$ofile %||% "."),
    "..", "stan", "gp_model.stan"
  )
  # Fallback: look relative to working directory
  if (!file.exists(stan_file)) stan_file <- "stan/gp_model.stan"

  sm <- stan_model(file = stan_file)

  # ── Sample ────────────────────────────────────────────────────────────────
  fit <- sampling(
    sm,
    data    = stan_data,
    chains  = chains,
    iter    = iter,
    seed    = seed,
    control = list(adapt_delta = 0.98),
    refresh = 200
  )

  # ── Save cache ────────────────────────────────────────────────────────────
  if (!is.null(rds_path)) {
    saveRDS(fit, rds_path)
    message("  Saved fit: ", rds_path)
  }

  list(fit = fit, scales = list(t = sc_t), target = target)
}

# Null-coalescing helper (base R doesn't have %||%)
`%||%` <- function(a, b) if (!is.null(a)) a else b
