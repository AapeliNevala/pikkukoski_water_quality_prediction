# 01_load_data.R
# Load and clean the three Finnish data sources.

library(readr)
library(dplyr)
library(lubridate)

load_data <- function(data_dir = "data") {

  # ── Rainfall (sademaarat.csv) ────────────────────────────────────────────────
  # Date split across Vuosi (year), Kk (month), Pv (day) columns.
  # -1 encodes missing measurements → treat as NA.
  rain_raw <- read_csv(
    file.path(data_dir, "sademaarat.csv"),
    show_col_types = FALSE
  ) |>
    rename(year = Vuosi, month = Kk, day = Pv,
           rain = `Sademäärä (mm)`) |>
    mutate(
      date = make_date(year, month, day),
      rain = if_else(rain < 0, NA_real_, rain)
    ) |>
    select(date, rain) |>
    arrange(date)

  # ── River flow (virtaama.csv) ────────────────────────────────────────────────
  # Semicolon-separated; date format DD.MM.YYYY; Lippu is a quality flag (ignore).
  flow_raw <- read_delim(
    file.path(data_dir, "virtaama.csv"),
    delim = ";", show_col_types = FALSE, trim_ws = TRUE
  ) |>
    rename(date = `Päivä`, flow = virtaama) |>
    mutate(date = dmy(date)) |>
    select(date, flow) |>
    arrange(date)

  # ── Bacteria / water quality (vesilaatudata.csv) ─────────────────────────────
  # Semicolon-separated with trailing spaces; D.M.YYYY dates; comma decimals.
  bacteria_raw <- read_delim(
    file.path(data_dir, "vesilaatudata.csv"),
    delim = ";", show_col_types = FALSE, trim_ws = TRUE
  ) |>
    rename(date = pvm, quality = laatu,
           entero = entero, coli = coli) |>
    mutate(
      date   = dmy(date),
      entero = as.numeric(entero),
      coli   = as.numeric(coli)
    ) |>
    select(date, quality, entero, coli) |>
    arrange(date)

  list(rain = rain_raw, flow = flow_raw, bacteria = bacteria_raw)
}
