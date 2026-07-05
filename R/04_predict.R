# 04_predict.R
# Extract posterior predictive samples from the fitted rstan model and
# summarise them for every swimming-season day (observed + unobserved).

library(rstan)
library(dplyr)

predict_bacteria <- function(fit_obj, grid) {

  season_grid  <- grid$season_grid
  target       <- fit_obj$target
  bacteria_col <- sub("log_", "", target)   # "entero" or "coli"

  # y_pred is vector[N] in generated quantities → extract gives S × N matrix
  # (S = posterior draws, N = all grid rows)
  y_pred <- extract(fit_obj$fit, "y_pred")$y_pred   # S × N

  tibble(
    date        = season_grid$date,
    rain        = season_grid$rain,
    observed    = season_grid[[bacteria_col]],
    pred_mean   = exp(colMeans(y_pred)),
    pred_lo90   = exp(apply(y_pred, 2, quantile, 0.05)),
    pred_hi90   = exp(apply(y_pred, 2, quantile, 0.95)),
    is_observed = !is.na(season_grid[[bacteria_col]]),
    bacteria    = bacteria_col
  )
}

save_predictions <- function(predictions, path = "output/predictions.csv") {
  readr::write_csv(predictions, path)
  message("Saved: ", path)
  invisible(predictions)
}
