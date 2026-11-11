# ============================================================================
# 01b_apply_country_updates.R   (repo: AFRO-Geo-Reference)
#
# Replaces the districts of a country in the clean layers with a newer shapefile that
# the country sent before the geodatabase export contains it (for example Benin, where
# Abomey-Calavi, Cotonou and Porto-Novo are split into several districts).
#
# Put the shapefile in:  data/updates/<COUNTRY>/<anything>.shp   (geodatabase columns:
# ADM0_NAME, ADM1_NAME, ADM2_NAME). The country is read from ADM0_NAME.
#
# For every district of the update:
#   - geo_code: the one already used in the layer for that name; else the one in the
#     geoname master for that name; else a NEW PROVISIONAL code (e.g. BEN-ATL-AC1) that
#     is listed in reference/country_updates_log.csv until the geodatabase issues its own.
#   - name: the spelling already used in the layer when the unit exists there, else the file's.
# Districts of the old layer that are not in the update are removed (for Benin: the three
# parents that were split). Provinces and the country outline are not changed; a warning
# says so if the update has a province that the layer does not know.
#
# Run it after R/01_clean_afrogdb.R and before R/02_simplify_for_dashboard.R. It can be run
# again at any time (it replaces the same units), and it must be run again after step 1 is
# re-run, because step 1 rebuilds the layers from the geodatabase export.
#
#   Rscript R/01b_apply_country_updates.R [clean_dir] [updates_dir]
# Defaults: data/clean  data/updates.   Remove a country's folder from data/updates once the
# geodatabase export contains its new boundaries.
# ============================================================================

suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf_use_s2(FALSE)

args <- commandArgs(trailingOnly = TRUE)
clean_dir   <- if (length(args) >= 1) args[1] else Sys.getenv("AFROGDB_CLEAN_DIR", unset = "data/clean")
updates_dir <- if (length(args) >= 2) args[2] else "data/updates"
ref_dir     <- Sys.getenv("AFROGDB_REFERENCE_DIR", unset = "reference")

shp_files <- list.files(updates_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
if (!length(shp_files)) { cat("No shapefiles in", updates_dir, "- nothing to do.\n"); quit(save = "no") }

f_dist <- file.path(clean_dir, "africa_districts.rds")
f_prov <- file.path(clean_dir, "africa_provinces.rds")
if (!file.exists(f_dist)) stop("Missing ", f_dist, " - run R/01_clean_afrogdb.R first.")
dist <- readRDS(f_dist)
prov <- if (file.exists(f_prov)) readRDS(f_prov) else NULL

rd_csv <- function(f) read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "")
master_files <- Filter(file.exists, c(file.path(ref_dir, "afrogdb_geoname_master.csv")))
master <- if (length(master_files)) rd_csv(master_files[1]) else NULL
nk <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; gsub("[^A-Z0-9]", "", toupper(stringi::stri_trans_general(x, "Latin-ASCII"))) }

log_rows <- list()

for (shp in shp_files) {
  n <- st_read(shp, quiet = TRUE)
  need <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")
  if (!all(need %in% names(n))) { cat("SKIPPED", shp, ": missing columns", paste(setdiff(need, names(n)), collapse = ", "), "\n"); next }
  if (is.na(st_crs(n))) st_crs(n) <- 4326 else n <- st_transform(n, 4326)
  n <- st_make_valid(n); n <- n[!st_is_empty(n), ]
  n$ADM0_NAME <- toupper(trimws(as.character(n$ADM0_NAME)))
  n$ADM1_NAME <- toupper(trimws(as.character(n$ADM1_NAME)))
  n$ADM2_NAME <- toupper(trimws(as.character(n$ADM2_NAME)))
  if (anyDuplicated(paste(n$ADM1_NAME, n$ADM2_NAME))) stop(shp, ": a province / district name appears twice")

  for (cty in unique(n$ADM0_NAME)) {
    nn  <- n[n$ADM0_NAME == cty, ]
    old <- dist[dist$ADM0_NAME == cty, ]
    cat(sprintf("\n%s: %d districts in the update, %d in the layer\n", cty, nrow(nn), nrow(old)))
    if (nrow(old) == 0) cat("  (country not in the layer: the units are added)\n")

    mc  <- if (!is.null(master)) master[master$country == cty & master$level == "district", ] else NULL
    iso <- if (!is.null(master)) { z <- master$iso3[master$country == cty & master$level == "country"]; if (length(z)) z[1] else substr(cty, 1, 3) } else substr(cty, 1, 3)
    taken <- unique(stats::na.omit(c(dist$GEO_CODE, if (!is.null(master)) master$geo_code)))

    wb_codes <- if (!is.null(master)) master$geo_code[master$in_workbook %in% c(TRUE, "TRUE")] else character()
    # when the layer / master hold the same name twice (current + retired), the workbook (current) one comes first
    old <- old[order(!(old$GEO_CODE %in% wb_codes)), ]
    if (!is.null(mc)) mc <- mc[order(!(mc$in_workbook %in% c(TRUE, "TRUE"))), ]
    key_new <- paste(nk(nn$ADM1_NAME), nk(nn$ADM2_NAME))
    key_old <- paste(nk(old$ADM1_NAME), nk(old$ADM2_NAME))
    key_mc  <- if (!is.null(mc)) paste(nk(mc$province), nk(mc$district)) else character()

    # names that differ by a number or a direction word are different units (ABOMEY-CALAVI 1 is not ABOMEY-CALAVI)
    tok <- function(x) { t <- strsplit(toupper(stringi::stri_trans_general(as.character(x), "Latin-ASCII")), "[^A-Z0-9]+")[[1]]
                         sort(unique(t[grepl("^([0-9]+|I|II|III|IV|V|VI|NORD|SUD|EST|OUEST|CENTRE|CENTRAL|NORTH|SOUTH|EAST|WEST|URBAN|RURAL|URBAIN|RURALE|HAUT|HAUTE|BAS|BASSE)$", t)])) }
    code <- rep(NA_character_, nrow(nn)); how <- rep(NA_character_, nrow(nn)); name2 <- nn$ADM2_NAME
    for (i in seq_len(nrow(nn))) {                                   # 1) same name
      j <- match(key_new[i], key_old); k <- match(key_new[i], key_mc)
      if (!is.na(j)) { code[i] <- old$GEO_CODE[j]; how[i] <- "layer"; name2[i] <- old$ADM2_NAME[j] }
      else if (!is.na(k)) { code[i] <- mc$geo_code[k]; how[i] <- "master"; name2[i] <- mc$district[k] }
    }
    for (i in seq_len(nrow(nn))) {                                   # 2) spelling variant of a current (workbook) unit not yet used
      if (!is.na(code[i]) && code[i] %in% wb_codes) next
      if (is.null(mc)) next
      pool <- which(mc$province == nn$ADM1_NAME[i] & mc$in_workbook %in% c(TRUE, "TRUE") & !mc$geo_code %in% code)
      pool <- pool[vapply(pool, function(p) identical(tok(mc$district[p]), tok(nn$ADM2_NAME[i])), logical(1))]
      if (!length(pool)) next
      d <- as.numeric(adist(nk(nn$ADM2_NAME[i]), nk(mc$district[pool]))) / pmax(nchar(nk(nn$ADM2_NAME[i])), nchar(nk(mc$district[pool])))
      if (min(d) <= 0.15) { p <- pool[which.min(d)]; code[i] <- mc$geo_code[p]; how[i] <- "spelling variant"; name2[i] <- mc$district[p] }
    }
    for (i in which(is.na(code))) {                                  # 3) new unit: provisional code
      sib <- c(old$GEO_CODE[old$ADM1_NAME == nn$ADM1_NAME[i]], if (!is.null(mc)) mc$geo_code[mc$province == nn$ADM1_NAME[i]])
      p3  <- if (length(sib)) { t <- strsplit(sib[1], "-")[[1]]; if (length(t) >= 3) t[2] else substr(gsub("[^A-Z]", "", nn$ADM1_NAME[i]), 1, 3) } else substr(gsub("[^A-Z]", "", nn$ADM1_NAME[i]), 1, 3)
      words <- strsplit(gsub("[^A-Z0-9 -]", "", nn$ADM2_NAME[i]), "[ -]+")[[1]]
      num   <- regmatches(nn$ADM2_NAME[i], regexpr("[0-9]+$", nn$ADM2_NAME[i]))
      words <- words[!grepl("^[0-9]+$", words)]
      base  <- toupper(paste0(if (length(words) >= 2) paste0(substr(words[1], 1, 1), substr(words[2], 1, 1)) else substr(words[1], 1, 2),
                              if (length(num)) substr(num, 1, 1) else ""))
      if (nchar(base) < 3) base <- substr(paste0(base, substr(words[1], 3, 5)), 1, 3)
      cand <- paste(iso, p3, base, sep = "-"); sfx <- 1
      while (cand %in% taken) { sfx <- sfx + 1; cand <- paste(iso, p3, paste0(substr(base, 1, 2), sfx), sep = "-") }
      code[i] <- cand; how[i] <- "provisional"; taken <- c(taken, cand)
    }
    if (anyDuplicated(code)) stop(cty, ": two districts got the same geo_code: ", paste(code[duplicated(code)], collapse = ", "))

    # province check
    if (!is.null(prov)) { unk <- setdiff(unique(nn$ADM1_NAME), prov$ADM1_NAME[prov$ADM0_NAME == cty]); if (length(unk)) cat("  WARNING: provinces not in the province layer:", paste(unk, collapse = ", "), "\n") }

    # build rows with the same columns as the layer
    gcol <- attr(dist, "sf_column")
    cols <- setdiff(names(dist), gcol)
    att  <- data.frame(ADM0_NAME = cty, ADM1_NAME = nn$ADM1_NAME, ADM2_NAME = name2, GEO_CODE = code, stringsAsFactors = FALSE)
    att$WORLD_CONTINENTS <- if (nrow(old) && "WORLD_CONTINENTS" %in% names(old)) old$WORLD_CONTINENTS[1] else "AFRICA"
    att$WHO_REGION <- if (nrow(old) && "WHO_REGION" %in% names(old)) old$WHO_REGION[1] else if (!is.null(master) && any(master$country == cty)) master$who_region[master$country == cty][1] else NA_character_
    if ("IN_WORKBOOK" %in% cols) att$IN_WORKBOOK <- if (!is.null(master)) code %in% master$geo_code[master$in_workbook %in% c(TRUE, "TRUE")] else FALSE
    for (cn in setdiff(cols, names(att))) att[[cn]] <- NA
    att <- att[, cols, drop = FALSE]
    new <- st_sf(att, geometry = st_geometry(nn))
    names(new)[names(new) == "geometry"] <- gcol; st_geometry(new) <- gcol
    new <- st_zm(new)
    new <- suppressWarnings(st_cast(new, "MULTIPOLYGON"))

    retired <- setdiff(old$GEO_CODE, code)
    dist <- rbind(dist[dist$ADM0_NAME != cty, ], new)

    log_rows[[length(log_rows) + 1]] <- rbind(
      data.frame(country = cty, province = nn$ADM1_NAME, district = name2, geo_code = code, code_from = how, action = "in update", stringsAsFactors = FALSE),
      if (length(retired)) data.frame(country = cty, province = old$ADM1_NAME[match(retired, old$GEO_CODE)], district = old$ADM2_NAME[match(retired, old$GEO_CODE)], geo_code = retired, code_from = "layer", action = "retired (not in update)", stringsAsFactors = FALSE))
    cat(sprintf("  geo_code from layer: %d, from master: %d, spelling variant of a current unit: %d, provisional: %d; retired: %s\n", sum(how == "layer"), sum(how == "master"), sum(how == "spelling variant"), sum(how == "provisional"),
                if (length(retired)) paste(old$ADM2_NAME[match(retired, old$GEO_CODE)], collapse = ", ") else "none"))
    if (any(how == "spelling variant")) cat("  spelling variants (the geodatabase spelling is kept):", paste(sprintf("%s -> %s", nn$ADM2_NAME[how == "spelling variant"], name2[how == "spelling variant"]), collapse = "; "), "\n")
    if (any(how == "provisional")) cat("  provisional codes:", paste(sprintf("%s = %s", name2[how == "provisional"], code[how == "provisional"]), collapse = "; "), "\n")

    # master table: drop retired shape-only units, add provisional ones
    if (!is.null(master)) {
      drop <- master$level == "district" & master$country == cty & master$geo_code %in% retired & !(master$in_workbook %in% c(TRUE, "TRUE"))
      add  <- how == "provisional"
      master <- master[!drop, ]
      if (any(add)) master <- rbind(master, data.frame(level = "district", geo_code = code[add], country = cty, province = nn$ADM1_NAME[add], district = name2[add], iso3 = iso,
                                                       who_region = new$WHO_REGION[1], in_workbook = FALSE, stringsAsFactors = FALSE))
    }
  }
}

gc <- dist$GEO_CODE[!is.na(dist$GEO_CODE)]                       # legacy units added by step 3 have no geo_code
if (anyDuplicated(gc)) stop("duplicated GEO_CODE after the update: ", paste(head(unique(gc[duplicated(gc)])), collapse = ", "))
saveRDS(dist, f_dist)
cat("\nWrote", f_dist, "-", nrow(dist), "districts\n")

if (!is.null(master)) {
  master <- master[order(match(master$level, c("country", "province", "district")), master$country, master$province, master$district), ]
  for (f in c(file.path(ref_dir, "afrogdb_geoname_master.csv"), file.path(clean_dir, "afrogdb_geoname_master.csv"))) {
    if (!dir.exists(dirname(f))) next
    tryCatch({ con <- file(f, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
               suppressWarnings(write.table(master, f, sep = ",", row.names = FALSE, na = "", qmethod = "double", append = TRUE, fileEncoding = "UTF-8")); cat("Wrote", f, "\n") },
             error = function(e) cat("WARNING: could not write ", f, " (open in Excel?)\n", sep = ""))
  }
  f_rds <- file.path(clean_dir, "afrogdb_geoname_master.rds"); if (file.exists(f_rds)) saveRDS(master, f_rds)
}

lg <- do.call(rbind, log_rows)
if (!is.null(lg)) {
  lf <- file.path(ref_dir, "country_updates_log.csv")
  tryCatch({ con <- file(lf, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)
             suppressWarnings(write.table(lg, lf, sep = ",", row.names = FALSE, na = "", qmethod = "double", append = TRUE, fileEncoding = "UTF-8")); cat("Wrote", lf, "\n") },
           error = function(e) cat("WARNING: could not write", lf, "\n"))
}
cat("\nNext: Rscript R/02_simplify_for_dashboard.R, then R/03_add_legacy_countries.R\n")
