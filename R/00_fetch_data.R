# 00_fetch_data.R
# Incremental, idempotent data collection for the three input sources.
# Safe to re-run: each fetch function only appends rows for dates that
# aren't already present in the target CSV.
#
# Run: Rscript R/00_fetch_data.R
#
# Rain and flow come from clean structured APIs and are fully automated.
# Bacteria measurements come from Helsinki's annual PDF report, which has
# an inconsistent internal layout year to year (see fetch_bacteria_updates()
# for details) -- new rows are ALWAYS printed for review, and are only
# written to disk when review = FALSE is passed explicitly. The pipeline
# run below passes review = FALSE (auto-write), relying on that function's
# built-in sanity checks (block count, beach-name cross-check) to abort
# instead of writing anything if the PDF layout looks off; run
# fetch_bacteria_updates() interactively (review = TRUE by default) to
# inspect new rows by hand before they're appended.

# Force a UTF-8 locale: virtaama.csv's header ("Päivä") contains a non-ASCII
# character, and environments with no locale set (e.g. cron's stripped
# environment) default to the "C" locale, which breaks matching that column
# name and makes every fetch fail. Fall back through a couple of common
# UTF-8 locale names since availability differs across systems.
for (loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8")) {
  if (suppressWarnings(Sys.setlocale("LC_ALL", loc)) != "") break
}

library(dplyr)
library(readr)
library(lubridate)
library(httr)
library(jsonlite)

FMI_FMISID        <- 101004  # Helsinki Kumpula precipitation station
SYKE_PAIKKA_ID    <- 1097    # Vantaanjoki Oulunkylä flow gauge
BEACH_NAME        <- "Pikkukoski"
BEACH_ORDER       <- c("Pakila", "Pikkukoski", "Tapaninvainio")  # fixed listing order in the PDF
PDF_URL_TEMPLATE  <- "https://www.hel.fi/static/liitteet/kaupunkiymparisto/kulttuuri-ja-vapaa-aika/uimarannat/Uimavedenlaatu_%d.pdf"

# ── Shared helper: append lines to a CSV, normalising trailing newlines ──────
append_lines <- function(path, lines) {
  if (length(lines) == 0) return(invisible(NULL))
  existing <- readChar(path, file.info(path)$size, useBytes = TRUE)
  existing <- sub("\n+$", "", existing)
  writeLines(c(existing, lines), path, useBytes = TRUE)
}

# ── Format a numeric value the way the existing CSVs do: no trailing .0 ─────
fmt_num <- function(v, digits = 1) {
  r <- round(v, digits)
  ifelse(r == round(r), as.character(as.integer(round(r))), as.character(r))
}

# ══════════════════════════════════════════════════════════════════════════
# Rain: FMI open data WFS, daily precipitation (fmi::observations::weather::daily::simple)
# ══════════════════════════════════════════════════════════════════════════
fetch_rain_updates <- function(path = "data/sademaarat.csv", fmisid = FMI_FMISID) {
  existing  <- read_csv(path, show_col_types = FALSE)
  last_date <- existing |> transmute(date = make_date(Vuosi, Kk, Pv)) |> pull(date) |> max()

  start <- last_date + 1
  end   <- Sys.Date()
  if (start > end) { message("Rain: already up to date."); return(invisible(NULL)) }

  # FMI WFS caps a single request at 8928 hours (~372 days).
  chunk_starts <- seq(start, end, by = "370 days")
  new_rows <- list()

  for (cs in as.list(chunk_starts)) {
    ce  <- min(cs + 369, end)
    resp <- GET("https://opendata.fmi.fi/wfs", query = list(
      request        = "getFeature",
      storedquery_id = "fmi::observations::weather::daily::simple",
      fmisid         = fmisid,
      starttime      = paste0(format(cs, "%Y-%m-%d"), "T00:00:00Z"),
      endtime        = paste0(format(ce, "%Y-%m-%d"), "T00:00:00Z"),
      parameters     = "rrday"
    ))
    stop_for_status(resp)
    txt   <- content(resp, as = "text", encoding = "UTF-8")
    times <- regmatches(txt, gregexpr("(?<=<BsWfs:Time>)[^<]+", txt, perl = TRUE))[[1]]
    vals  <- regmatches(txt, gregexpr("(?<=<BsWfs:ParameterValue>)[^<]+", txt, perl = TRUE))[[1]]
    if (length(times) > 0) {
      new_rows[[length(new_rows) + 1]] <- tibble(date = as.Date(substr(times, 1, 10)),
                                                   rain = as.numeric(vals))
    }
  }

  if (length(new_rows) == 0) { message("Rain: no data returned."); return(invisible(NULL)) }

  new_df <- bind_rows(new_rows) |> distinct(date, .keep_all = TRUE) |> arrange(date) |>
    filter(date >= start, date <= end)

  if (nrow(new_df) == 0) { message("Rain: no new rows."); return(invisible(NULL)) }

  out_lines <- sprintf("%d,%d,%d,00:00,UTC,%s",
                        year(new_df$date), month(new_df$date), day(new_df$date),
                        fmt_num(new_df$rain))
  append_lines(path, out_lines)
  message(sprintf("Rain: appended %d day(s), %s to %s",
                   nrow(new_df), format(min(new_df$date)), format(max(new_df$date))))
  invisible(new_df)
}

# ══════════════════════════════════════════════════════════════════════════
# River flow: SYKE Hydrologiarajapinta OData API (Virtaama entity)
# ══════════════════════════════════════════════════════════════════════════
fetch_flow_updates <- function(path = "data/virtaama.csv", paikka_id = SYKE_PAIKKA_ID) {
  existing  <- read_delim(path, delim = ";", show_col_types = FALSE, trim_ws = TRUE)
  last_date <- existing |> transmute(date = dmy(`Päivä`)) |> pull(date) |> max()

  start <- last_date + 1
  end   <- Sys.Date()
  if (start > end) { message("Flow: already up to date."); return(invisible(NULL)) }

  filt <- sprintf("Paikka_Id eq %d and Aika ge datetime'%s' and Aika le datetime'%s'",
                   paikka_id, format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))
  url <- modify_url("https://rajapinnat.ymparisto.fi/api/Hydrologiarajapinta/1.0/odata/Virtaama",
                     query = list(`$filter` = filt))

  all_records <- list()
  repeat {
    resp <- GET(url); stop_for_status(resp)
    d <- fromJSON(content(resp, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
    all_records <- c(all_records, d$value)
    url <- d[["odata.nextLink"]]
    if (is.null(url)) break
  }

  if (length(all_records) == 0) { message("Flow: no data returned."); return(invisible(NULL)) }

  new_df <- tibble(
    date = as.Date(substr(vapply(all_records, `[[`, "", "Aika"), 1, 10)),
    flow = as.numeric(vapply(all_records, `[[`, "", "Arvo")),
    flag = vapply(all_records, function(r) if (is.null(r$Lippu_id)) "" else as.character(r$Lippu_id), "")
  ) |> distinct(date, .keep_all = TRUE) |> arrange(date)

  if (nrow(new_df) == 0) { message("Flow: no new rows."); return(invisible(NULL)) }

  out_lines <- sprintf("%s;%s;%s", format(new_df$date, "%d.%m.%Y"), new_df$flow, new_df$flag)
  append_lines(path, out_lines)
  message(sprintf("Flow: appended %d day(s), %s to %s",
                   nrow(new_df), format(min(new_df$date)), format(max(new_df$date))))
  invisible(new_df)
}

# ══════════════════════════════════════════════════════════════════════════
# Bacteria: Helsinki's annual Uimavedenlaatu_<year>.pdf report.
#
# The PDF's internal layout is NOT stable year to year (confirmed across
# 2023-2026): sometimes a beach's name sits on its own line, sometimes it's
# fused onto the row of its first sample; a beach's very first sample of the
# season sometimes appears as an "orphan" row one line above its name. A
# fixed line-offset heuristic breaks on at least one of the observed years.
#
# Robust signal instead: within the Vantaanjoki river section, the three
# beaches (Pakila, Pikkukoski, Tapaninvainio) are always listed in that
# fixed order, each with its own chronologically increasing run of sample
# dates. So a block boundary is detected wherever the date *resets*
# backward (a beach's April/June sample date comes after another beach's
# August date) -- this holds regardless of where the name label happens to
# sit. The beach name string is still cross-checked as a sanity guard.
#
# Given this inherent fragility, new rows are always printed for review and
# are only written to disk when review = FALSE is passed explicitly.
# ══════════════════════════════════════════════════════════════════════════
fetch_bacteria_updates <- function(path = "data/vesilaatudata.csv",
                                    beach = BEACH_NAME,
                                    beach_order = BEACH_ORDER,
                                    pdf_path = NULL,
                                    review = TRUE) {

  existing <- read_delim(path, delim = ";", show_col_types = FALSE, trim_ws = TRUE)
  existing_dates <- dmy(existing$pvm)

  if (is.null(pdf_path)) {
    year_now <- year(Sys.Date())
    pdf_path <- tempfile(fileext = ".pdf")
    url <- sprintf(PDF_URL_TEMPLATE, year_now)
    resp <- tryCatch(GET(url, write_disk(pdf_path, overwrite = TRUE)), error = function(e) NULL)
    if (is.null(resp) || http_error(resp)) {
      message(sprintf("Bacteria: no PDF available yet for %d (%s)", year_now, url))
      return(invisible(NULL))
    }
  }

  txt   <- system2("pdftotext", c("-layout", shQuote(pdf_path), "-"), stdout = TRUE)
  lines <- txt

  start_idx <- grep("Vantaanjoki/Vanda", lines)[1]
  if (is.na(start_idx)) { message("Bacteria: could not locate the Vantaanjoki section."); return(invisible(NULL)) }
  # Any of these mark the end of the Vantaanjoki block: the next named area
  # section, or the report's own repeating page header (whichever the
  # particular year's layout happens to use isn't stable, so check all).
  end_idx <- grep("Lampi\\s*/\\s*Damm|It.\\s*/\\s*.stra\\s*/\\s*east|Saaret/.ar/islands|L.nsi/v.stra/west|UIMAVEDEN LAATU", lines)
  end_idx <- end_idx[end_idx > start_idx][1]
  if (is.na(end_idx)) end_idx <- length(lines)
  section <- lines[start_idx:end_idx]

  date_re <- paste0(
    "([0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{4})",              # 1: date
    "(,\\s*[A-Za-zÄÖÅäöå\\*]+)?\\s+",                     # 2: optional note (uusinta/lisänäyte)
    "(Hyv./bra/good|Huono/d.lig/poor)\\s+",               # 3: quality
    "([0-9]+(?:,[0-9]+)?)\\s+",                           # 4: temperature
    "([<>]?\\s?[0-9]+)\\s+",                              # 5: entero
    "([<>]?\\s?[0-9]+)\\s+",                              # 6: coli
    "([0-3])\\s+",                                        # 7: algae flag
    "([0-1])"                                             # 8: other-observations flag
  )

  m    <- regmatches(section, regexec(date_re, section, perl = TRUE))
  hits <- which(lengths(m) > 1)
  if (length(hits) == 0) { message("Bacteria: no data rows matched in the Vantaanjoki section."); return(invisible(NULL)) }

  recs <- lapply(hits, function(i) {
    mm <- m[[i]]
    list(line = section[i], date = dmy(mm[2]), note = trimws(sub("^,\\s*", "", mm[3])),
         quality = mm[4], temp = mm[5],
         entero = as.numeric(gsub("[<> ]", "", mm[6])),
         coli   = as.numeric(gsub("[<> ]", "", mm[7])),
         sinileva = mm[8], muu = mm[9])
  })

  dates <- do.call(c, lapply(recs, `[[`, "date"))
  block_id <- cumsum(c(1L, as.integer(diff(dates) < 0)))
  n_blocks <- max(block_id)

  if (n_blocks != length(beach_order)) {
    message(sprintf(
      "Bacteria: expected %d beach blocks (%s) in the Vantaanjoki section, found %d. Aborting -- PDF layout may have changed, needs manual review.",
      length(beach_order), paste(beach_order, collapse = ", "), n_blocks))
    return(invisible(NULL))
  }

  target_idx  <- match(beach, beach_order)
  target_rows <- recs[block_id == target_idx]

  # Sanity cross-check: the beach's own name (or its Swedish twin, checked
  # loosely by just using the configured name) should appear somewhere in
  # the raw lines spanning this block, and NOT the neighbouring beaches'.
  block_line_range <- range(hits[block_id == target_idx])
  block_text <- paste(section[block_line_range[1]:block_line_range[2]], collapse = "\n")
  neighbours <- setdiff(beach_order, beach)
  if (!grepl(beach, block_text)) {
    message(sprintf("Bacteria: sanity check failed -- '%s' not found inside its own detected block. Aborting.", beach))
    return(invisible(NULL))
  }
  bad_neighbour <- neighbours[vapply(neighbours, function(n) grepl(n, block_text), logical(1))]
  if (length(bad_neighbour) > 0) {
    message(sprintf("Bacteria: sanity check failed -- neighbouring beach name(s) %s found inside %s's detected block. Aborting.",
                     paste(bad_neighbour, collapse = ", "), beach))
    return(invisible(NULL))
  }

  parsed <- bind_rows(lapply(target_rows, function(r) {
    quality <- if (nzchar(r$note)) paste(r$note, r$quality) else r$quality
    tibble(date = r$date, quality = quality, temp = r$temp,
           entero = r$entero, coli = r$coli, sinileva = r$sinileva, muu = r$muu)
  })) |>
    filter(month(date) %in% 6:8, !is.na(entero), !is.na(coli))

  # Idempotent: drop rows already present (matched on date + entero + coli,
  # since a beach can legitimately have 2 distinct samples on the same day).
  existing_keys <- paste(existing_dates, existing$entero, existing$coli)
  new_keys      <- paste(parsed$date, parsed$entero, parsed$coli)
  parsed_new    <- parsed[!new_keys %in% existing_keys, ]

  if (nrow(parsed_new) == 0) { message("Bacteria: no new rows for ", beach, "."); return(invisible(NULL)) }

  out_lines <- sprintf("%d.%d.%d; %s; %s; %s; %s; %s; %s",
                        day(parsed_new$date), month(parsed_new$date), year(parsed_new$date),
                        parsed_new$quality, parsed_new$temp,
                        fmt_num(parsed_new$entero, 0), fmt_num(parsed_new$coli, 0),
                        parsed_new$sinileva, parsed_new$muu)

  message(sprintf("Bacteria: %d new row(s) found for %s:", nrow(parsed_new), beach))
  print(parsed_new)

  if (review) {
    message("Bacteria: review = TRUE (default) -- NOT written to disk. Re-run with review = FALSE to append.")
    return(invisible(parsed_new))
  }

  append_lines(path, out_lines)
  message(sprintf("Bacteria: appended %d new row(s) to %s", nrow(parsed_new), path))
  invisible(parsed_new)
}

# ── Run all three when sourced as a script (not when source()'d for testing) ──
if (identical(environment(), globalenv()) && sys.nframe() == 0) {
  message("== Rain ==");     fetch_rain_updates()
  message("== Flow ==");     fetch_flow_updates()
  message("== Bacteria =="); fetch_bacteria_updates(review = FALSE)
}
