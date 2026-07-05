# 02_features.R
# Build a daily grid covering swimming seasons (June–August) and derive the
# rain/flow predictors used by the regression model:
#   - rain_decay: exponentially decayed sum of trailing rainfall (captures
#     short-lived CSO-driven bacteria pulses after rain)
#   - log_flow:   same-day Vantaanjoki river flow (log scale; captures
#     watershed-wide wetness / dilution)
#   - doy:        day of year, for a within-season smooth term
#
# Rain lag matrix (used only to compute rain_decay):
#   rain_lag[i, k]  =  rain on day (date_i - (k-1))   for k = 1..W
#   Column 1 = same day (lag 0), column W = lag W-1 days back.
#   Missing rain values are treated as 0.

library(dplyr)
library(tidyr)
library(lubridate)

RAIN_LAG_WINDOW <- 21   # days — lookback window for rain lag matrix
RAIN_DECAY_TAU  <- 3    # days — fixed exponential decay half-life for rain_decay

build_grid <- function(bacteria, rain, flow,
                        window = RAIN_LAG_WINDOW, tau = RAIN_DECAY_TAU) {

  # ── Daily grid for full data range ──────────────────────────────────────────
  # A few weeks before the first bacteria date are needed to fill the lag matrix.
  # The whole point of the model is nowcasting bacteria on days without a lab
  # sample, so the grid extends to the latest date with BOTH rain and flow
  # available -- not just the last bacteria measurement, which typically lags
  # behind rain/flow by several days.
  date_min <- min(bacteria$date) - window
  date_max <- max(bacteria$date, min(max(rain$date), max(flow$date)))

  full_grid <- tibble(date = seq(date_min, date_max, by = "day"))

  # ── Join rain (treat missing as 0 mm) ───────────────────────────────────────
  full_grid <- full_grid |>
    left_join(rain, by = "date") |>
    mutate(rain = replace_na(rain, 0))

  # ── Restrict to swimming season (June–August) ────────────────────────────────
  season_grid <- full_grid |>
    filter(month(date) %in% 6:8)

  # ── Join bacteria observations (NA on unsampled days = to predict) ───────────
  # ── Join river flow (same-day, log scale) ───────────────────────────────────
  season_grid <- season_grid |>
    left_join(bacteria |> select(date, entero, coli), by = "date") |>
    left_join(flow |> select(date, flow), by = "date") |>
    mutate(
      t          = as.numeric(date - min(date)),
      doy        = yday(date),
      log_entero = log(entero),
      log_coli   = log(coli),
      log_flow   = log(flow),
      year       = factor(year(date))
    ) |>
    arrange(date)

  # ── Rain lag matrix (N × W) ──────────────────────────────────────────────────
  # For each season_grid row i, look up raw rain values for the past W days
  # (including the current day) from full_grid.
  rain_by_date <- setNames(full_grid$rain, as.character(full_grid$date))

  n   <- nrow(season_grid)
  lag_mat <- matrix(0.0, nrow = n, ncol = window)
  for (i in seq_len(n)) {
    for (k in seq_len(window)) {
      d <- as.character(season_grid$date[i] - (k - 1L))
      v <- rain_by_date[d]
      if (!is.na(v)) lag_mat[i, k] <- v
    }
  }

  # ── Rain decay feature: exponentially-weighted trailing rainfall ───────────
  decay_w <- exp(-(0:(window - 1)) / tau)
  decay_w <- decay_w / sum(decay_w)
  season_grid$rain_decay <- as.numeric(lag_mat %*% decay_w)

  list(season_grid = season_grid, rain_lag = lag_mat)
}
