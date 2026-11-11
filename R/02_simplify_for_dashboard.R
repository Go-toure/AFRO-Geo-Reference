# ============================================================================
# 02_simplify_for_dashboard.R   (repo: AFRO-Geo-Reference)
#
# Build the lighter copies of the clean layers that the dashboard reads:
#   data/clean/africa_countries_simplified.rds
#   data/clean/africa_provinces_simplified.rds
#   data/clean/africa_districts_simplified.rds
#   data/clean/ambiguous_district_pairs.rds
#
# It reads the full-resolution layers written by R/01_clean_afrogdb.R and uses the
# dashboard's own preprocess_shapefile() (AFRO-Dash/shapefile_prep_utils.R), so
# the result is built exactly the way the app expects.  Needs the rmapshaper package.
#
# Why a separate step: simplifying the district layer runs in rmapshaper's
# JavaScript engine, which can run out of memory and stop R completely.  Here
# every layer is simplified in a fresh R process.  The district layer is simplified
# in groups of countries (borders between countries in different groups can differ
# very slightly; the country layer drawn on top hides this).  A group that fails
# or that the dashboard's safety check leaves at full resolution is split and
# retried; a single country that still fails is kept at full resolution and listed.
#
# Run from the repository root:
#   Rscript R/02_simplify_for_dashboard.R
# Optional environment variables:
#   AFRO_DASH_DIR=<folder>      AFRO-Dash checkout (default ../AFRO-Dash)
#   AFROGDB_CLEAN_DIR=<folder>  where the layers are (default data/clean)
#   AFROGDB_CHUNK=<n>           countries per group for the district layer (default 6)
#   AFROGDB_KEEP_COUNTRIES / AFROGDB_KEEP_PROVINCES / AFROGDB_KEEP_DISTRICTS
#                               share of detail kept, 0-1 (defaults 0.01 / 0.04 / 0.05)
#   AFROGDB_TRY_WHOLE=1         try the whole district layer in one go first (can crash rmapshaper)
# ============================================================================

suppressPackageStartupMessages({ library(sf); library(dplyr) })
sf::sf_use_s2(FALSE)

args       <- commandArgs(trailingOnly = TRUE)
DASH_DIR   <- Sys.getenv("AFRO_DASH_DIR", unset = file.path("..", "AFRO-Dash"))
CLEAN_DIR  <- Sys.getenv("AFROGDB_CLEAN_DIR", unset = file.path("data", "clean"))
CHUNK      <- suppressWarnings(as.integer(Sys.getenv("AFROGDB_CHUNK", unset = "6")))
if (is.na(CHUNK) || CHUNK < 1) CHUNK <- 6L
PREP       <- file.path(DASH_DIR, "shapefile_prep_utils.R")
if (!file.exists(PREP)) stop("Cannot find ", PREP, " -- set AFRO_DASH_DIR to your AFRO-Dash folder.")
if (!requireNamespace("rmapshaper", quietly = TRUE)) stop("Install the rmapshaper package first: install.packages(\"rmapshaper\")")
source(PREP)

# ---- child mode: simplify one file in this (fresh) process ---------------------
# Rscript R/02_simplify_for_dashboard.R --child <layer> <keep> <in.rds> <out.rds>
if (length(args) == 5 && args[1] == "--child") {
  x <- readRDS(args[4])
  saveRDS(preprocess_shapefile(x, args[2], keep = as.numeric(args[3])), args[5])
  quit(save = "no", status = 0)
}

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
rscript <- file.path(R.home("bin"), "Rscript")
LOG <- file.path(CLEAN_DIR, "simplify_log.txt")      # warnings / engine crashes of the child processes go here
cat("simplification log ", format(Sys.time()), "\n", file = LOG)
# share of the detail kept (rmapshaper "keep"). The new geodatabase is much more detailed than the
# layers the dashboard used before, so these are lower than the dashboard's old 0.15.
# Override with AFROGDB_KEEP_COUNTRIES / _PROVINCES / _DISTRICTS (e.g. 0.08).
keep_env <- function(name, default) { v <- suppressWarnings(as.numeric(Sys.getenv(name, unset = NA))); if (is.na(v) || v <= 0 || v > 1) default else v }
KEEP_LAYER <- list(countries = keep_env("AFROGDB_KEEP_COUNTRIES", 0.01),
                   provinces = keep_env("AFROGDB_KEEP_PROVINCES", 0.04),
                   districts = keep_env("AFROGDB_KEEP_DISTRICTS", 0.05))
REDUCED <- 0.7    # a simplified file must be smaller than 70% of its input, otherwise the group counts as not simplified
KEEP <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "WORLD_CONTINENTS", "WHO_REGION", "GEO_CODE")

run_child <- function(layer, infile, outfile) {
  if (file.exists(outfile)) file.remove(outfile)
  st <- suppressWarnings(system2(rscript, c(shQuote(script), "--child", layer, KEEP_LAYER[[layer]], shQuote(infile), shQuote(outfile)),
                                 stdout = "", stderr = LOG))
  isTRUE(st == 0) && file.exists(outfile)
}

# Simplify a set of districts in a fresh process.  If that fails, or the dashboard's
# safety check kept the full resolution, split the countries in two and try again.
# A single country that still fails is kept at full resolution and reported.
simplify_group <- function(x, layer) {
  cin <- tempfile(fileext = ".rds"); cout <- tempfile(fileext = ".rds")
  on.exit(unlink(c(cin, cout)), add = TRUE)
  saveRDS(x, cin)
  ran <- run_child(layer, cin, cout)
  if (ran && file.size(cout) < REDUCED * file.size(cin)) return(list(sf = readRDS(cout), full_res = character(0)))
  cs <- sort(unique(x$ADM0_NAME))
  if (length(cs) > 1) {
    cat("  retrying in two smaller groups (", paste(cs, collapse = ", "), ")\n", sep = "")
    h <- ceiling(length(cs) / 2)
    a <- simplify_group(x[x$ADM0_NAME %in% cs[seq_len(h)], ], layer)
    b <- simplify_group(x[x$ADM0_NAME %in% cs[-seq_len(h)], ], layer)
    return(list(sf = rbind(a$sf, b$sf), full_res = c(a$full_res, b$full_res)))
  }
  cat("  ", cs, ": could not be simplified safely -- kept at full resolution\n", sep = "")
  list(sf = if (ran) readRDS(cout) else x[, intersect(names(x), KEEP)], full_res = cs)
}

simplify_layer <- function(layer, try_whole = TRUE, chunked_ok = FALSE) {
  infile  <- file.path(CLEAN_DIR, paste0("africa_", layer, ".rds"))
  outfile <- file.path(CLEAN_DIR, paste0("africa_", layer, "_simplified.rds"))
  if (!file.exists(infile)) stop("Missing ", infile, " -- run R/01_clean_afrogdb.R first.")
  if (try_whole) {
    cat("\n", layer, ": simplifying the whole layer ...\n", sep = "")
    if (run_child(layer, infile, outfile)) { cat("  done (whole layer)\n"); return(invisible(readRDS(outfile))) }
    if (!chunked_ok) stop("Simplifying the ", layer, " layer failed (see ", LOG, ").")
    cat("  the whole layer did not fit; ")
  } else cat("\n", layer, ": ", sep = "")
  cat("simplifying groups of ", CHUNK, " countries ...\n", sep = "")

  x <- readRDS(infile)
  countries <- sort(unique(x$ADM0_NAME))
  groups <- split(countries, ceiling(seq_along(countries) / CHUNK))
  parts <- vector("list", length(groups)); full_res <- character(0)
  for (i in seq_along(groups)) {
    cat(sprintf("  group %d/%d: %s\n", i, length(groups), paste(groups[[i]], collapse = ", ")))
    r <- simplify_group(x[x$ADM0_NAME %in% groups[[i]], ], layer)
    parts[[i]] <- r$sf; full_res <- c(full_res, r$full_res)
  }
  out <- do.call(rbind, parts)
  stopifnot(nrow(out) == nrow(x))
  saveRDS(out, outfile)
  cat("  done: ", nrow(out), " features", sep = "")
  if (length(full_res)) cat("; kept at full resolution: ", paste(full_res, collapse = ", "), sep = "")
  cat("\n")
  invisible(out)
}

simplify_layer("countries")
simplify_layer("provinces")
# The whole district layer crashes rmapshaper's engine on some machines, so groups are the default;
# set AFROGDB_TRY_WHOLE=1 to try the whole layer first.
dist_s <- simplify_layer("districts", try_whole = identical(Sys.getenv("AFROGDB_TRY_WHOLE"), "1"), chunked_ok = TRUE)

saveRDS(compute_ambiguous_district_pairs(dist_s), file.path(CLEAN_DIR, "ambiguous_district_pairs.rds"))
cat("\nDone. Simplified layers and ambiguous_district_pairs.rds are in ", normalizePath(CLEAN_DIR, mustWork = FALSE), "\n", sep = "")
cat("Details of any warnings: ", LOG, "\n", sep = "")
cat("\nFile sizes (MB) -- keep them small for fast maps:\n")
for (f in c("africa_countries_simplified.rds", "africa_provinces_simplified.rds", "africa_districts_simplified.rds"))
  cat(sprintf("  %-34s %6.1f\n", f, file.size(file.path(CLEAN_DIR, f)) / 1024^2))
