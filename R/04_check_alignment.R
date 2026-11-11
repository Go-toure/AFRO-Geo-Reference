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
#   relaxed        identical once generic words (DISTRICT, DC, COUNCIL, CITY, TOWN, WOREDA ...) are also
#                  ignored, and only one such unit exists: REVIEW these before relying on them
#   other province the district name exists in the country but under another province
#   ambiguous      that district name exists in several provinces of the country
#   crosswalk      matched through a decision in reference/crosswalk_geonames.csv (ok = reviewed, auto = proposed)
#   no shape       marked no_shape in that file: the geodatabase has no unit for it yet
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

# ---- matching functions and reference tables (shared with the pipelines) ----
source(file.path("R", "afrogdb_standardize.R"))
ref <- afrogdb_reference(".")

read_any <- function(p) if (grepl("\\.rds$", p, ignore.case = TRUE)) readRDS(p) else read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")

dir.create("reports", showWarnings = FALSE)
summary_rows <- list()
for (nm in names(SOURCES)) {
  sp <- SOURCES[[nm]]
  if (!file.exists(sp[1])) { cat(sprintf("%-6s file not found: %s\n", nm, sp[1])); next }
  x <- read_any(sp[1])
  if (!all(sp[2:4] %in% names(x))) { cat(sprintf("%-6s columns %s not all in the file\n", nm, paste(sp[2:4], collapse = "/"))); next }
  d <- data.frame(country = as.character(x[[sp[2]]]), province = as.character(x[[sp[3]]]), district = as.character(x[[sp[4]]]), stringsAsFactors = FALSE) |>
    count(country, province, district, name = "rows")
  d <- afrogdb_match(d, ref, suggest = TRUE)
  out <- d |> transmute(country, province, district, rows, status, std_country, std_province = std_province_final, std_district, geo_code,
                        country_status, province_status, suggestion, suggestion_geo_code, suggestion_distance) |> arrange(desc(rows))
  out_f <- file.path("reports", paste0("crosswalk_", nm, ".csv"))
  tryCatch(write.csv(out, out_f, row.names = FALSE, na = "", fileEncoding = "UTF-8"), error = function(e) cat("WARNING: could not write ", out_f, "\n"))

  tot <- sum(d$rows)
  pct <- function(v) round(100 * sum(d$rows[v]) / tot, 1)
  cat(sprintf("\n%-6s %s rows, %s distinct country/province/district\n", nm, format(tot, big.mark = ","), format(nrow(d), big.mark = ",")))
  cat(sprintf("       country  matched %5.1f%% (exact %5.1f%%)\n", pct(d$country_status != "unmatched"), pct(d$country_status == "exact")))
  cat(sprintf("       province matched %5.1f%% (exact %5.1f%%)\n", pct(d$province_status != "unmatched"), pct(d$province_status == "exact")))
  cat(sprintf("       district found   %5.1f%% (exact %5.1f%%) | other province %4.1f%% | relaxed %4.1f%% | crosswalk %4.1f%% | no shape %4.1f%% | ambiguous %4.1f%% | unmatched %4.1f%%\n",
              pct(d$status %in% c("exact", "normalized", "relaxed", "other province", "crosswalk", "crosswalk (auto)")), pct(d$status == "exact"), pct(d$status == "other province"), pct(d$status == "relaxed"), pct(d$status %in% c("crosswalk", "crosswalk (auto)")), pct(d$status == "no shape"), pct(d$status == "ambiguous"), pct(d$status == "unmatched")))
  bad <- d |> filter(country_status == "unmatched") |> group_by(country) |> summarise(rows = sum(rows)) |> arrange(desc(rows))
  if (nrow(bad)) cat("       countries not found:", paste0(bad$country, " (", bad$rows, ")", collapse = "; "), "\n")
  summary_rows[[nm]] <- data.frame(source = nm, rows = tot, distinct_units = nrow(d),
    country_matched_pct = pct(d$country_status != "unmatched"), province_matched_pct = pct(d$province_status != "unmatched"),
    district_exact_pct = pct(d$status == "exact"), district_normalized_pct = pct(d$status == "normalized"),
    district_other_province_pct = pct(d$status == "other province"), district_relaxed_pct = pct(d$status == "relaxed"), district_crosswalk_pct = pct(d$status %in% c("crosswalk", "crosswalk (auto)")), district_no_shape_pct = pct(d$status == "no shape"), district_ambiguous_pct = pct(d$status == "ambiguous"), district_unmatched_pct = pct(d$status == "unmatched"))
}
if (length(summary_rows)) {
  tryCatch(write.csv(bind_rows(summary_rows), file.path("reports", "alignment_summary.csv"), row.names = FALSE), error = function(e) cat("WARNING: could not write reports/alignment_summary.csv\n"))
  cat("\nReports written to reports/ (crosswalk_<name>.csv lists every unit with its geo_code).\n")
}
