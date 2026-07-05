# 04_predict.R
# Extract posterior predictive samples from the fitted brms model and
# summarise them for every swimming-season day (observed + unobserved).

library(brms)
library(dplyr)

predict_bacteria <- function(fit_obj, grid, threshold = NULL) {

  season_grid  <- grid$season_grid
  target       <- fit_obj$target
  bacteria_col <- sub("log_", "", target)   # "entero" or "coli"

  # S x N matrix of posterior predictive draws on the log scale, for every
  # grid row (observed and unobserved days alike).
  y_pred <- posterior_predict(fit_obj$fit, newdata = season_grid,
                               allow_new_levels = TRUE)

  # Share of posterior draws below the regulatory threshold, i.e. P(bacteria
  # <= threshold) for each day -- computed on the log scale so it's exact
  # (no re-exponentiating the whole draw matrix).
  prob_under_limit <- if (!is.null(threshold)) {
    colMeans(y_pred <= log(threshold))
  } else {
    NA_real_
  }

  tibble(
    date             = season_grid$date,
    rain             = season_grid$rain,
    flow             = season_grid$flow,
    observed         = season_grid[[bacteria_col]],
    pred_mean        = exp(colMeans(y_pred)),
    pred_lo90        = exp(apply(y_pred, 2, quantile, 0.05)),
    pred_hi90        = exp(apply(y_pred, 2, quantile, 0.95)),
    prob_under_limit = prob_under_limit,
    is_observed      = !is.na(season_grid[[bacteria_col]]),
    bacteria         = bacteria_col
  )
}

save_predictions <- function(predictions, path = "output/predictions.csv") {
  readr::write_csv(predictions, path)
  message("Saved: ", path)
  invisible(predictions)
}
