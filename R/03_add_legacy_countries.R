# ============================================================================
# 03_add_legacy_countries.R   (repo: AFRO-Geo-Reference)
#
# The new geodatabase has provinces and districts for 47 countries only.  Seven
# countries are in it as country outlines only: Egypt, Libya, Morocco, Tunisia,
# Sudan, Djibouti and Somalia.  Until their provinces and districts arrive, the
# shapes the dashboard used before are added to the simplified layers, so those
# countries keep their maps.
#
# Rule: a legacy country is added only when the new layer has NO rows for it.
# When the geodatabase gets real shapes for a country, step 01 + 02 pick them up
# and this step leaves that country alone.  Re-running this step is safe.
#
# Legacy shapes are kept in data/legacy/ (small, in git): one row per province /
# district (repeated rows of the old layer were merged), valid, EPSG:4326,
# GEO_CODE = NA, WHO_REGION = EMRO.
#
# Run after R/02_simplify_for_dashboard.R, from the repository root:
#   Rscript R/03_add_legacy_countries.R
# It also rebuilds ambiguous_district_pairs.rds from the final district layer
# (needs ../AFRO-Dash/shapefile_prep_utils.R, or AFRO_DASH_DIR) and lists the
# legacy units in reference/legacy_units.csv.
# ============================================================================

suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf::sf_use_s2(FALSE)

DASH_DIR   <- Sys.getenv("AFRO_DASH_DIR", unset = file.path("..", "AFRO-Dash"))
CLEAN_DIR  <- Sys.getenv("AFROGDB_CLEAN_DIR", unset = file.path("data", "clean"))
LEGACY_DIR <- file.path("data", "legacy")

added <- list()
for (layer in c("provinces", "districts")) {
  f_new <- file.path(CLEAN_DIR, paste0("africa_", layer, "_simplified.rds"))
  f_leg <- file.path(LEGACY_DIR, paste0("legacy_", layer, ".rds"))
  if (!file.exists(f_new)) stop("Missing ", f_new, " -- run R/01_clean_afrogdb.R and R/02_simplify_for_dashboard.R first.")
  if (!file.exists(f_leg)) stop("Missing ", f_leg)
  new <- readRDS(f_new); leg <- readRDS(f_leg)
  add <- leg[!leg$ADM0_NAME %in% unique(new$ADM0_NAME), , drop = FALSE]
  cat(sprintf("%-9s new layer: %d features in %d countries | legacy countries added: %s\n", layer, nrow(new),
              length(unique(new$ADM0_NAME)), if (nrow(add)) paste(sort(unique(add$ADM0_NAME)), collapse = ", ") else "none"))
  if (!nrow(add)) next
  for (cn in setdiff(names(new), names(add))) add[[cn]] <- NA                      # same columns as the new layer
  add <- add[, names(new)]
  if (!identical(sf::st_crs(add), sf::st_crs(new))) add <- sf::st_transform(add, sf::st_crs(new))
  out <- rbind(new, add)
  saveRDS(out, f_new)
  cat(sprintf("          + %d legacy features -> %d features\n", nrow(add), nrow(out)))
  added[[layer]] <- sf::st_drop_geometry(add) |>
    transmute(level = sub("s$", "", layer), country = ADM0_NAME, province = ADM1_NAME,
              district = if ("ADM2_NAME" %in% names(add)) ADM2_NAME else NA_character_)
}

if (length(added)) {
  dir.create("reference", showWarnings = FALSE)
  tryCatch(write.csv(bind_rows(added), file.path("reference", "legacy_units.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8"),
           error = function(e) cat("WARNING: could not write reference/legacy_units.csv (open in Excel?)\n"))
}

prep <- file.path(DASH_DIR, "shapefile_prep_utils.R")
f_d  <- file.path(CLEAN_DIR, "africa_districts_simplified.rds")
if (file.exists(prep)) {
  source(prep)
  saveRDS(compute_ambiguous_district_pairs(readRDS(f_d)), file.path(CLEAN_DIR, "ambiguous_district_pairs.rds"))
  cat("ambiguous_district_pairs.rds rebuilt from the final district layer\n")
} else cat("NOTE: ", prep, " not found -- ambiguous_district_pairs.rds was not rebuilt (set AFRO_DASH_DIR).\n", sep = "")

cat("\nFile sizes (MB):\n")
for (f in c("africa_countries_simplified.rds", "africa_provinces_simplified.rds", "africa_districts_simplified.rds"))
  cat(sprintf("  %-34s %6.1f\n", f, file.size(file.path(CLEAN_DIR, f)) / 1024^2))
