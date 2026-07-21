#!/usr/bin/env bash
# pipeline.sh — run the full water-bacteria pipeline in order.
# 1) fetch new data, 2) run all R scripts (via main.R), 3) render the dashboard.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

echo "==> Fetching new data (R/00_fetch_data.R)"
Rscript R/00_fetch_data.R

echo "==> Running model + prediction pipeline (main.R)"
Rscript main.R

echo "==> Rendering dashboard (dashboard.qmd)"
quarto render dashboard.qmd

echo "==> Pipeline complete"
