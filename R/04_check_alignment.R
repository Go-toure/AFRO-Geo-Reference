# ============================================================================
# 04_check_alignment.R   (repo: AFRO-Geo-Reference)
#
# How well do the names in a data file match the geodatabase?  Used by the LQAS
# pipeline and the IM workflow to standardise their geonames (outside the dashboard).
#
# For every distinct country / province / district in a data file it finds the
# geodatabase unit and its geo_code, and says how it matched:
#   exact          the three names are identical to the geodatabase (after trimming / capitals)
#   normalized     identical once accents, apostrophes, hyphens, spaces and "RM " are ignored
#   other province the district name exists in the country but under another province
#   ambiguous      that district name exists in several provinces of the country
#   unmatched      nothing found (a closest-name SUGGESTION is given for a person to review;
#                  suggestions are never applied automatically)
# Country aliases (e.g. TANZANIA, CAMEROUN) come from reference/crosswalk_country.csv.
# The seven countries that still use legacy shapes (reference/legacy_units.csv) are matched too.
#
# Output (reports/, not in git):
#   alignment_summary.csv        one line per data file and level
#   crosswalk_<name>.csv         every distinct country/province/district of the file with its
#                                status, geo_code and standard names (what a pipeline adopts)
#
# Run from the repository root.  Default: the four dashboard data files in ../AFRO-Dash/input
#   Rscript R/04_check_alignment.R
# One file of your own (rds or csv; the three column names after the path):
#   Rscript R/04_check_alignment.R <name> <path> <country_col> <province_col> <district_col>
# AFRO_DASH_DIR=<folder> changes where the default files are looked for.
# ============================================================================

suppressPackageStartupMessages(library(dplyr))

args     <- commandArgs(trailingOnly = TRUE)
DASH_DIR <- Sys.getenv("AFRO_DASH_DIR", unset = file.path("..", "AFRO-Dash"))
inp      <- file.path(DASH_DIR, "input")
SOURCES  <- if (length(args) == 5) {
  setNames(list(args[2:5]), args[1])
} else list(
  LQAS  = c(file.path(inp, "AFRO_LQAS_data_c.rds"), "country", "province", "district"),
  IM    = c(file.path(inp, "AFRO_IM_data.rds"),     "country", "province", "district"),
  ADMIN = c(file.path(inp, "AFRO_admin_data.rds"),  "Country", "Province", "District"),
  SCOPE = c(file.path(inp, "Scope.rds"),            "country", "province", "district")
)

# ---- normalisation: capitals, no accents, no "RM " prefix, letters and digits only ----
to_ascii <- function(x) if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_general(x, "Latin-ASCII") else iconv(x, to = "ASCII//TRANSLIT")
nk <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- toupper(to_ascii(trimws(x))); x <- sub("^RM[ -]+", "", x); gsub("[^A-Z0-9]", "", x) }
up <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; toupper(trimws(x)) }

# ---- reference: geodatabase master + legacy units ----------------------------------
rd <- function(f) read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = "")
if (!file.exists("reference/afrogdb_geoname_master.csv")) stop("reference/afrogdb_geoname_master.csv not found -- run R/01_clean_afrogdb.R first.")
master <- rd("reference/afrogdb_geoname_master.csv")
ref_c <- master |> filter(level == "country") |> transmute(country, geo_code)
ref_p <- master |> filter(level == "province") |> transmute(country, province, geo_code)
ref_d <- master |> filter(level == "district") |> transmute(country, province, district, geo_code)
if (file.exists("reference/legacy_units.csv")) {
  lg <- rd("reference/legacy_units.csv")
  ref_p <- bind_rows(ref_p, lg |> filter(level == "province") |> transmute(country, province, geo_code = NA_character_))
  ref_d <- bind_rows(ref_d, lg |> filter(level == "district") |> transmute(country, province, district, geo_code = NA_character_))
}
cw <- if (file.exists("reference/crosswalk_country.csv")) rd("reference/crosswalk_country.csv") else data.frame(raw_name = character(), std_country = character())
alias <- setNames(nk(cw$std_country), nk(cw$raw_name)); alias_name <- setNames(cw$std_country, nk(cw$raw_name))
ref_c$ck <- nk(ref_c$country)
ref_p$ck <- nk(ref_p$country); ref_p$pk <- nk(ref_p$province)
ref_d$ck <- nk(ref_d$country); ref_d$pk <- nk(ref_d$province); ref_d$dk <- nk(ref_d$district)
ref_d$ex <- paste(up(ref_d$country), up(ref_d$province), up(ref_d$district), sep = "|")

read_any <- function(p) if (grepl("\\.rds$", p, ignore.case = TRUE)) readRDS(p) else read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")

closest <- function(key, cands) {                       # nearest candidate by edit distance on the normalised names
  if (!length(cands) || !nzchar(key)) return(c(NA, NA))
  d <- as.numeric(adist(key, cands)) / pmax(nchar(key), nchar(cands))
  i <- which.min(d); c(i, round(d[i], 2))
}

dir.create("reports", showWarnings = FALSE)
summary_rows <- list()
for (nm in names(SOURCES)) {
  sp <- SOURCES[[nm]]
  if (!file.exists(sp[1])) { cat(sprintf("%-6s file not found: %s\n", nm, sp[1])); next }
  x <- read_any(sp[1])
  if (!all(sp[2:4] %in% names(x))) { cat(sprintf("%-6s columns %s not all in the file\n", nm, paste(sp[2:4], collapse = "/"))); next }
  d <- data.frame(country = as.character(x[[sp[2]]]), province = as.character(x[[sp[3]]]), district = as.character(x[[sp[4]]]), stringsAsFactors = FALSE) |>
    count(country, province, district, name = "rows")
  d$ck0 <- nk(d$country); hit <- unname(alias[d$ck0]); d$ck <- ifelse(is.na(hit), d$ck0, hit)
  d$pk <- nk(d$province); d$dk <- nk(d$district)

  d$country_status <- ifelse(up(d$country) %in% up(ref_c$country), "exact", ifelse(d$ck %in% ref_c$ck, "normalized", "unmatched"))
  d$std_country <- ref_c$country[match(d$ck, ref_c$ck)]
  d$country_geo_code <- ref_c$geo_code[match(d$ck, ref_c$ck)]
  pm <- match(paste(d$ck, d$pk), paste(ref_p$ck, ref_p$pk))
  d$province_status <- ifelse(d$country_status == "unmatched", "unmatched", ifelse(is.na(pm), "unmatched",
                        ifelse(up(d$province) == up(ref_p$province[pm]) & d$country_status == "exact", "exact", "normalized")))
  d$std_province <- ref_p$province[pm]; d$province_geo_code <- ref_p$geo_code[pm]

  full <- paste(d$ck, d$pk, d$dk); rfull <- paste(ref_d$ck, ref_d$pk, ref_d$dk)
  dm <- match(full, rfull)
  cd <- paste(d$ck, d$dk); rcd <- paste(ref_d$ck, ref_d$dk); ncd <- table(rcd)
  d$status <- "unmatched"; d$std_district <- NA_character_; d$geo_code <- NA_character_; d$std_province2 <- NA_character_
  ok <- !is.na(dm)
  d$status[ok] <- ifelse(paste(up(d$country), up(d$province), up(d$district), sep = "|")[ok] == ref_d$ex[dm[ok]] , "exact", "normalized")
  d$std_district[ok] <- ref_d$district[dm[ok]]; d$geo_code[ok] <- ref_d$geo_code[dm[ok]]; d$std_province2[ok] <- ref_d$province[dm[ok]]
  rest <- which(!ok & d$ck %in% ref_c$ck)
  for (i in rest) {
    n <- as.integer(ncd[cd[i]]); n <- ifelse(is.na(n), 0L, n)
    if (n == 1L) { j <- match(cd[i], rcd); d$status[i] <- "other province"; d$std_district[i] <- ref_d$district[j]; d$geo_code[i] <- ref_d$geo_code[j]; d$std_province2[i] <- ref_d$province[j] }
    else if (n > 1L) d$status[i] <- "ambiguous"
  }
  d$suggestion <- NA_character_; d$suggestion_geo_code <- NA_character_; d$suggestion_distance <- NA_real_
  un <- which(d$status %in% c("unmatched", "ambiguous") & d$ck %in% ref_c$ck)
  for (i in un) {
    pool <- which(ref_d$ck == d$ck[i] & (is.na(pm[i]) | ref_d$pk == d$pk[i]))
    if (!length(pool)) pool <- which(ref_d$ck == d$ck[i])
    cl <- closest(d$dk[i], ref_d$dk[pool])
    if (!is.na(cl[1]) && cl[2] <= 0.4) { j <- pool[cl[1]]; d$suggestion[i] <- paste(ref_d$province[j], ref_d$district[j], sep = " / "); d$suggestion_geo_code[i] <- ref_d$geo_code[j]; d$suggestion_distance[i] <- cl[2] }
  }
  d$std_province_final <- ifelse(!is.na(d$std_province2), d$std_province2, d$std_province)
  out <- d |> transmute(country, province, district, rows, status, std_country, std_province = std_province_final, std_district, geo_code,
                        country_status, province_status, suggestion, suggestion_geo_code, suggestion_distance) |> arrange(desc(rows))
  out_f <- file.path("reports", paste0("crosswalk_", nm, ".csv"))
  tryCatch(write.csv(out, out_f, row.names = FALSE, na = "", fileEncoding = "UTF-8"), error = function(e) cat("WARNING: could not write ", out_f, "\n"))

  tot <- sum(d$rows)
  pct <- function(v) round(100 * sum(d$rows[v]) / tot, 1)
  cat(sprintf("\n%-6s %s rows, %s distinct country/province/district\n", nm, format(tot, big.mark = ","), format(nrow(d), big.mark = ",")))
  cat(sprintf("       country  matched %5.1f%% (exact %5.1f%%)\n", pct(d$country_status != "unmatched"), pct(d$country_status == "exact")))
  cat(sprintf("       province matched %5.1f%% (exact %5.1f%%)\n", pct(d$province_status != "unmatched"), pct(d$province_status == "exact")))
  cat(sprintf("       district matched %5.1f%% (exact %5.1f%%) | other province %4.1f%% | ambiguous %4.1f%% | unmatched %4.1f%%\n",
              pct(d$status %in% c("exact", "normalized")), pct(d$status == "exact"), pct(d$status == "other province"), pct(d$status == "ambiguous"), pct(d$status == "unmatched")))
  bad <- d |> filter(country_status == "unmatched") |> group_by(country) |> summarise(rows = sum(rows)) |> arrange(desc(rows))
  if (nrow(bad)) cat("       countries not found:", paste0(bad$country, " (", bad$rows, ")", collapse = "; "), "\n")
  summary_rows[[nm]] <- data.frame(source = nm, rows = tot, distinct_units = nrow(d),
    country_matched_pct = pct(d$country_status != "unmatched"), province_matched_pct = pct(d$province_status != "unmatched"),
    district_exact_pct = pct(d$status == "exact"), district_normalized_pct = pct(d$status == "normalized"),
    district_other_province_pct = pct(d$status == "other province"), district_ambiguous_pct = pct(d$status == "ambiguous"), district_unmatched_pct = pct(d$status == "unmatched"))
}
if (length(summary_rows)) {
  tryCatch(write.csv(bind_rows(summary_rows), file.path("reports", "alignment_summary.csv"), row.names = FALSE), error = function(e) cat("WARNING: could not write reports/alignment_summary.csv\n"))
  cat("\nReports written to reports/ (crosswalk_<name>.csv lists every unit with its geo_code).\n")
}
