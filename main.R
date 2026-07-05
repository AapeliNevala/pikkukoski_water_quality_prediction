# main.R  —  Water bacteria prediction pipeline (summer months only)
# Run: Rscript main.R   OR source() interactively.
#
# Model: Bayesian regression (brms, Stan backend) predicting log(bacteria)
# from river flow and rainfall — see R/03_model.R for the formula.
#
# Required packages:
#   install.packages(c("tidyverse", "lubridate", "brms"))

source("R/01_load_data.R")
source("R/02_features.R")
source("R/03_model.R")
source("R/04_predict.R")

library(ggplot2)
library(dplyr)
library(purrr)

# ── 1. Load ────────────────────────────────────────────────────────────────────
message("Loading data...")
raw  <- load_data(data_dir = "data")

cat(sprintf("  Rain:     %d days (%s – %s)\n",
            nrow(raw$rain), min(raw$rain$date), max(raw$rain$date)))
cat(sprintf("  Flow:     %d days (%s – %s)\n",
            nrow(raw$flow), min(raw$flow$date), max(raw$flow$date)))
cat(sprintf("  Bacteria: %d measurements (%s – %s)\n",
            nrow(raw$bacteria), min(raw$bacteria$date), max(raw$bacteria$date)))

# ── 2. Feature grid (June–August only) ────────────────────────────────────────
message("Building feature grid...")
grid <- build_grid(raw$bacteria, raw$rain, raw$flow)

cat(sprintf("  Grid: %d swimming-season days | %d bacteria measurements\n",
            nrow(grid$season_grid), sum(!is.na(grid$season_grid$entero))))

# ── 3 & 4. Fit + predict — one model per bacteria type ────────────────────────
targets <- c("log_entero", "log_coli")

all_predictions <- map_dfr(targets, function(tgt) {
  bacteria_name <- sub("log_", "", tgt)
  message(sprintf("\nFitting model for %s...", bacteria_name))

  fit_obj <- fit_bacteria_model(
    grid   = grid,
    target = tgt,
    chains = 4,
    iter   = 2000,
    seed   = 42,
    file   = sprintf("output/model_%s", bacteria_name)
  )

  cat("\n--- Model summary:", bacteria_name, "---\n")
  print(summary(fit_obj$fit))

  message(sprintf("Predicting %s...", bacteria_name))
  predict_bacteria(fit_obj, grid)
})

save_predictions(all_predictions, "output/predictions.csv")

# ── 5. Quick sanity plot ───────────────────────────────────────────────────────
# Bacteria threshold lines (site-specific limits):
thresholds <- c(entero = 400, coli = 500)

p <- ggplot(all_predictions, aes(x = date)) +
  geom_ribbon(aes(ymin = pred_lo90, ymax = pred_hi90),
              alpha = 0.25, fill = "steelblue") +
  geom_line(aes(y = pred_mean), colour = "steelblue", linewidth = 0.8) +
  geom_point(data = filter(all_predictions, is_observed),
             aes(y = observed), colour = "black", size = 2) +
  geom_hline(
    data = tibble(bacteria = names(thresholds), threshold = unname(thresholds)),
    aes(yintercept = threshold),
    linetype = "dashed", colour = "red", linewidth = 0.6
  ) +
  scale_y_log10(labels = scales::comma) +
  facet_grid(bacteria ~ year(date), scales = "free_x") +
  labs(
    title    = "Bacteria levels — observed (●) vs model prediction (flow + rain)",
    subtitle = "Shaded = 90 % credible interval | dashed = EU 'sufficient' threshold",
    x = NULL, y = "Bacteria CFU/100 mL (log scale)"
  ) +
  theme_minimal(base_size = 12)

ggsave("output/bacteria_prediction.png", p, width = 14, height = 7, dpi = 150)
message("\nPlot saved: output/bacteria_prediction.png")
message("Done.")
