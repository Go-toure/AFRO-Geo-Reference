# ============================================================================
# 01_clean_afrogdb.R   (repo: AFRO-Geo-Reference)
#
# Turn the raw AFRO geodatabase export (latest_boundary_adm0/1/2.shp +
# AFROGDB.xlsx) into clean layers plus a geoname "master" table.  The layers use
# the schema the AFRO-Dash dashboard already reads (ADM0_NAME, ADM1_NAME,
# ADM2_NAME, WHO_REGION, WORLD_CONTINENTS, + GEO_CODE, ISO_3_CODE, IN_WORKBOOK,
# geometry).  The master table is what the LQAS pipeline and the IM workflow use
# to standardise their own geonames.
#
# Nothing here touches the dashboard.  The simplified copies the dashboard reads
# are built by the next step, R/02_simplify_for_dashboard.R (README.md).
#
# Run from the repository root:
#   Rscript R/01_clean_afrogdb.R
# or with explicit folders:
#   Rscript R/01_clean_afrogdb.R <gdb_dir> <out_dir>
#
# CLEANING RULES (also written to cleaning_report.txt on every run)
#  1. Keep only CURRENT records (no end date, or an end date in the future /
#     "9999-12-31").  Retired versions are dropped.
#  2. The export repeats each unit several times (same code, sometimes slightly
#     different geometry).  One row is kept per geo code (level_id): the current
#     record with the latest start date, then the highest batch id.
#  3. Mayotte, Reunion and Saint Helena are removed.
#  4. Invalid geometries are repaired (st_make_valid); empty ones are dropped.
#  5. WHO_REGION comes from the workbook (country sheet); WORLD_CONTINENTS is
#     "AFRICA" for every kept country.
#  6. Names are NOT altered.  They stay exactly as in the geodatabase, so the
#     geodatabase is the single source of truth for spelling.
#  7. Units that are in the shapefile but not in the workbook are KEPT and
#     flagged (IN_WORKBOOK = FALSE) -- they are real units (e.g. Cotonou).
# ============================================================================

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readxl); library(jsonlite)
})
sf::sf_use_s2(FALSE)
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

args    <- commandArgs(trailingOnly = TRUE)
GDB_DIR <- args[1] %||% "data/raw/AFROGDB"
OUT_DIR <- args[2] %||% "data/clean"
EXCLUDE_COUNTRIES <- c("MAYOTTE", "REUNION", "SAINT HELENA")   # rule 3
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

report <- character(0)
say <- function(...) { line <- paste0(...); cat(line, "\n"); report <<- c(report, line) }
say("AFRO geodatabase cleaning -- ", format(Sys.time(), "%Y-%m-%d %H:%M"))
say("GDB folder: ", normalizePath(GDB_DIR, mustWork = FALSE))

# ---- locate files --------------------------------------------------------
find1 <- function(pattern) {
  f <- list.files(GDB_DIR, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(f)) stop("Cannot find ", pattern, " under ", GDB_DIR)
  f[1]
}
shp <- c(adm0 = find1("latest_boundary_adm0\\.shp$"),
         adm1 = find1("latest_boundary_adm1\\.shp$"),
         adm2 = find1("latest_boundary_adm2\\.shp$"))
xlsx <- find1("\\.xlsx$")

# ---- helpers -------------------------------------------------------------
to_date <- function(x) suppressWarnings(as.Date(as.character(x)))
is_current <- function(end_date) {
  end <- to_date(end_date)                       # "9999-12-31" can come through as NA / year 0: both mean "no end"
  yr  <- suppressWarnings(as.integer(format(end, "%Y")))
  is.na(end) | is.na(yr) | yr < 1 | end > Sys.Date()
}
up <- function(x) toupper(trimws(as.character(x)))

clean_layer <- function(x, level, id_col = "level_id") {
  n0 <- nrow(x)
  x <- x[is_current(x$end_date), , drop = FALSE]
  n1 <- nrow(x)
  x <- x[order(x[[id_col]], to_date(x$start_date), x$batch_id, decreasing = TRUE), , drop = FALSE]
  x <- x[!duplicated(x[[id_col]]), , drop = FALSE]
  n2 <- nrow(x)
  say(sprintf("  %-9s rows in file %6d | current %6d (-%d retired) | one per code %6d (-%d repeats)",
              level, n0, n1, n0 - n1, n2, n1 - n2))
  x
}

fix_geometry <- function(x, level) {
  inv <- !st_is_valid(x)
  inv[is.na(inv)] <- TRUE
  if (any(inv)) { x[inv, ] <- st_make_valid(x[inv, ]); say(sprintf("  %-9s repaired %d invalid geometries", level, sum(inv))) }
  emp <- st_is_empty(x)
  if (any(emp)) { x <- x[!emp, ]; say(sprintf("  %-9s dropped %d empty geometries", level, sum(emp))) }
  if (is.na(st_crs(x))) st_crs(x) <- 4326
  if (st_crs(x) != st_crs(4326)) x <- st_transform(x, 4326)
  x
}

# ---- workbook: country attributes + code lists ----------------------------
say("\nReading workbook: ", basename(xlsx))
wb0 <- readxl::read_excel(xlsx, sheet = "latest_boundary_adm0")
wb1 <- readxl::read_excel(xlsx, sheet = "latest_boundary_adm1")
wb2 <- readxl::read_excel(xlsx, sheet = "latest_boundary_adm2")
att <- lapply(wb0$attributes, function(s) tryCatch(fromJSON(s), error = function(e) list()))
wb_country <- data.frame(
  ADM0_NAME  = up(wb0$name),
  WHO_REGION = vapply(att, function(a) a$WHO_REGION %||% NA_character_, character(1)),
  stringsAsFactors = FALSE
)
wb_country <- wb_country[!duplicated(wb_country$ADM0_NAME), ]

# ---- 1. countries ----------------------------------------------------------
say("\nCountries")
s0 <- st_read(shp[["adm0"]], quiet = TRUE)
s0 <- clean_layer(s0, "adm0")
drop0 <- up(s0$name) %in% EXCLUDE_COUNTRIES
say("  removed: ", paste(sort(unique(s0$name[drop0])), collapse = ", "))
s0 <- s0[!drop0, ]
countries <- s0 %>%
  transmute(ADM0_NAME = up(name), ISO_3_CODE = iso3_code, GEO_CODE = level_id) %>%
  left_join(wb_country, by = "ADM0_NAME") %>%
  mutate(WORLD_CONTINENTS = "AFRICA") %>%
  select(ADM0_NAME, WHO_REGION, WORLD_CONTINENTS, ISO_3_CODE, GEO_CODE)
countries <- fix_geometry(countries, "adm0")
if (anyNA(countries$WHO_REGION)) say("  WARNING: no WHO_REGION in workbook for: ", paste(countries$ADM0_NAME[is.na(countries$WHO_REGION)], collapse = ", "))
say("  kept ", nrow(countries), " countries (", sum(countries$WHO_REGION == "AFRO", na.rm = TRUE), " AFRO)")
region_of <- setNames(countries$WHO_REGION, countries$ADM0_NAME)

# ---- 2. provinces ----------------------------------------------------------
say("\nProvinces")
s1 <- st_read(shp[["adm1"]], quiet = TRUE)
s1 <- clean_layer(s1, "adm1")
s1 <- s1[!(up(s1$adm0_name) %in% EXCLUDE_COUNTRIES), ]
provinces <- s1 %>%
  transmute(ADM0_NAME = up(adm0_name), ADM1_NAME = up(name), ISO_3_CODE = iso3_code, GEO_CODE = level_id,
            IN_WORKBOOK = level_id %in% wb1$level_id) %>%
  mutate(WHO_REGION = unname(region_of[ADM0_NAME]), WORLD_CONTINENTS = "AFRICA") %>%
  select(ADM0_NAME, ADM1_NAME, WHO_REGION, WORLD_CONTINENTS, ISO_3_CODE, GEO_CODE, IN_WORKBOOK)
provinces <- fix_geometry(provinces, "adm1")
say("  kept ", nrow(provinces), " provinces in ", n_distinct(provinces$ADM0_NAME), " countries; ",
    sum(!provinces$IN_WORKBOOK), " are in the shapefile but not in the workbook")

# ---- 3. districts ----------------------------------------------------------
say("\nDistricts")
s2 <- st_read(shp[["adm2"]], quiet = TRUE)
s2 <- clean_layer(s2, "adm2")
s2 <- s2[!(up(s2$adm0_name) %in% EXCLUDE_COUNTRIES), ]
districts <- s2 %>%
  transmute(ADM0_NAME = up(adm0_name), ADM1_NAME = up(adm1_name), ADM2_NAME = up(name), ISO_3_CODE = iso3_code,
            GEO_CODE = level_id, IN_WORKBOOK = level_id %in% wb2$level_id) %>%
  mutate(WHO_REGION = unname(region_of[ADM0_NAME]), WORLD_CONTINENTS = "AFRICA") %>%
  select(ADM0_NAME, ADM1_NAME, ADM2_NAME, WHO_REGION, WORLD_CONTINENTS, ISO_3_CODE, GEO_CODE, IN_WORKBOOK)
districts <- fix_geometry(districts, "adm2")
say("  kept ", nrow(districts), " districts in ", n_distinct(districts$ADM0_NAME), " countries; ",
    sum(!districts$IN_WORKBOOK), " are in the shapefile but not in the workbook")

# ---- 4. consistency checks (reported, nothing is silently changed) ----------
say("\nChecks")
d_prov <- unique(paste(districts$ADM0_NAME, districts$ADM1_NAME, sep = "||"))
p_prov <- unique(paste(provinces$ADM0_NAME, provinces$ADM1_NAME, sep = "||"))
say("  districts whose province is not in the province layer: ", sum(!paste(districts$ADM0_NAME, districts$ADM1_NAME, sep = "||") %in% p_prov))
say("  countries with districts but no country shape: ", paste(setdiff(unique(districts$ADM0_NAME), countries$ADM0_NAME), collapse = ", ") %||% "none")
dn <- districts %>% st_drop_geometry() %>% count(ADM0_NAME, ADM1_NAME, ADM2_NAME) %>% filter(n > 1)
say("  district names repeated inside the same province (different codes): ", nrow(dn))
amb <- districts %>% st_drop_geometry() %>% distinct(ADM0_NAME, ADM1_NAME, ADM2_NAME) %>% count(ADM0_NAME, ADM2_NAME) %>% filter(n > 1)
say("  district names that recur in more than one province of the same country: ", nrow(amb))

# ---- 5. geoname master table (for the LQAS pipeline / IM workflow) -----------
master <- bind_rows(
  countries %>% st_drop_geometry() %>% transmute(level = "country", geo_code = GEO_CODE, country = ADM0_NAME, province = NA_character_, district = NA_character_, iso3 = ISO_3_CODE, who_region = WHO_REGION, in_workbook = TRUE),
  provinces %>% st_drop_geometry() %>% transmute(level = "province", geo_code = GEO_CODE, country = ADM0_NAME, province = ADM1_NAME, district = NA_character_, iso3 = ISO_3_CODE, who_region = WHO_REGION, in_workbook = IN_WORKBOOK),
  districts %>% st_drop_geometry() %>% transmute(level = "district", geo_code = GEO_CODE, country = ADM0_NAME, province = ADM1_NAME, district = ADM2_NAME, iso3 = ISO_3_CODE, who_region = WHO_REGION, in_workbook = IN_WORKBOOK)
)
write.csv(master, file.path(OUT_DIR, "afrogdb_geoname_master.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
saveRDS(master, file.path(OUT_DIR, "afrogdb_geoname_master.rds"))
dir.create("reference", showWarnings = FALSE)   # small, versioned in git
write.csv(master, file.path("reference", "afrogdb_geoname_master.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
say("\nGeoname master table: ", nrow(master), " rows -> afrogdb_geoname_master.csv / .rds")

# ---- 6. write layers --------------------------------------------------------
saveRDS(countries, file.path(OUT_DIR, "africa_countries.rds"))
saveRDS(provinces, file.path(OUT_DIR, "africa_provinces.rds"))
saveRDS(districts, file.path(OUT_DIR, "africa_districts.rds"))
say("Full-resolution layers written to ", normalizePath(OUT_DIR, mustWork = FALSE))

say("\nNext step: Rscript R/02_simplify_for_dashboard.R  (simplified copies for the dashboard)")

writeLines(report, file.path(OUT_DIR, "cleaning_report.txt"), useBytes = TRUE)
cat("\nDone. Report: ", file.path(OUT_DIR, "cleaning_report.txt"), "\n")
