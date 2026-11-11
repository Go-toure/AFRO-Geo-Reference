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
# the result is built the way the app expects.  Needs the rmapshaper package.
#
# How it keeps the files light
#  * Each layer gets a VERTEX BUDGET (provinces 250,000; districts 1,000,000 points
#    in total, spread evenly over the features).  rmapshaper's "keep" does not give a
#    predictable share (the new geodatabase is very dense in some countries), so a
#    dense file is simplified again, with a stronger setting, until it is within its
#    budget (5 passes at most).  Files that are already light get a single pass.
#  * Every layer is simplified in a fresh R process: rmapshaper's JavaScript engine
#    can run out of memory and stop R completely on the big district layer.  The
#    district layer is done in groups of countries (borders between countries in
#    different groups can differ very slightly; the country layer drawn on top hides this).
#  * If the dashboard's safety check refuses a simplification because it makes a
#    few shapes invalid (it then keeps the full resolution), the shapes are repaired
#    (st_make_valid) and accepted when nothing is lost: same number of features,
#    none empty, none invalid.  Only if that fails too is the group split, and a
#    single country that still fails is kept at full resolution and listed.
#
# Run from the repository root:
#   Rscript R/02_simplify_for_dashboard.R
# Optional environment variables:
#   AFRO_DASH_DIR=<folder>      AFRO-Dash checkout (default ../AFRO-Dash)
#   AFROGDB_CLEAN_DIR=<folder>  where the layers are (default data/clean)
#   AFROGDB_CHUNK=<n>           countries per group for the district layer (default 6)
#   AFROGDB_KEEP_COUNTRIES / AFROGDB_KEEP_PROVINCES / AFROGDB_KEEP_DISTRICTS
#                               first-pass share of detail kept, 0-1 (defaults 0.01 / 0.04 / 0.05)
#   AFROGDB_TARGET_PROVINCES / AFROGDB_TARGET_DISTRICTS
#                               vertex budget of the layer (defaults 250000 / 1000000);
#                               raise it for sharper maps, lower it for lighter files
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

KEEP <- c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME", "WORLD_CONTINENTS", "WHO_REGION", "GEO_CODE")

# number of vertices of an sf layer (cheap, no big matrices)
n_vertices <- function(x) sum(vapply(sf::st_geometry(x), function(g) length(unlist(g, use.names = FALSE)) / 2, numeric(1)))

# ---- child mode: simplify one file in this (fresh) process ---------------------
# Rscript R/02_simplify_for_dashboard.R --child <layer> <keep> <vertices per feature> <in.rds> <out.rds>
#   vertices per feature = the budget (Inf = a single pass is enough)
simplify_one_pass <- function(x, layer, k) {
  out <- preprocess_shapefile(x, layer, keep = k)
  if (n_vertices(out) < 0.98 * n_vertices(x)) return(out)
  # the dashboard's safety check kept the full resolution (it refuses any new invalid shape):
  # simplify, repair, and accept only if nothing is lost
  y <- x[, intersect(names(x), KEEP)]
  s <- tryCatch(rmapshaper::ms_simplify(y, keep = k, keep_shapes = TRUE, sys = FALSE), error = function(e) NULL)
  if (is.null(s)) return(out)
  s <- sf::st_make_valid(s)
  if (any(!as.character(sf::st_geometry_type(s)) %in% c("POLYGON", "MULTIPOLYGON"))) s <- sf::st_collection_extract(s, "POLYGON")
  if (nrow(s) != nrow(y) || any(sf::st_is_empty(s)) || any(!sf::st_is_valid(s))) return(out)
  cat("   repaired the invalid shapes after simplifying\n")
  s[, intersect(names(s), names(out))]      # same columns as the normal path
}

if (length(args) == 6 && args[1] == "--child") {
  layer <- args[2]; k <- as.numeric(args[3]); per_feature <- as.numeric(args[4])
  x <- readRDS(args[5]); n0 <- n_vertices(x); cur <- x
  ratio <- min(1, per_feature * nrow(x) / n0)       # share of the vertices this file may keep
  for (pass in 1:5) {
    nxt <- simplify_one_pass(cur, layer, k)
    r <- n_vertices(nxt) / n0
    cat(sprintf("   pass %d: %.1f%% of the vertices kept\n", pass, 100 * r))
    if (n_vertices(nxt) >= 0.98 * n_vertices(cur)) break          # no further progress
    cur <- nxt
    if (r <= 1.25 * ratio) break
    k <- min(0.5, max(0.02, ratio / r))
  }
  saveRDS(cur[, intersect(names(cur), KEEP)], args[6])
  quit(save = "no", status = 0)
}

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
rscript <- file.path(R.home("bin"), "Rscript")
LOG <- tempfile("simplify_log_", fileext = ".txt")   # warnings / engine crashes of the child processes (copied to data/clean at the end)
cat("simplification log ", format(Sys.time()), "\n", file = LOG)

num_env <- function(name, default) { v <- suppressWarnings(as.numeric(Sys.getenv(name, unset = NA))); if (is.na(v) || v <= 0) default else v }
KEEP_LAYER   <- list(countries = min(1, num_env("AFROGDB_KEEP_COUNTRIES", 0.01)),
                     provinces = min(1, num_env("AFROGDB_KEEP_PROVINCES", 0.04)),
                     districts = min(1, num_env("AFROGDB_KEEP_DISTRICTS", 0.05)))
TARGET_VERTS <- list(countries = Inf,
                     provinces = num_env("AFROGDB_TARGET_PROVINCES", 250000),
                     districts = num_env("AFROGDB_TARGET_DISTRICTS", 1000000))
PER_FEATURE <- list(countries = Inf, provinces = Inf, districts = Inf)   # vertices per feature, filled in per layer
REDUCED <- 0.7    # a simplified file must be smaller than 70% of its input, otherwise the group counts as not simplified

run_child <- function(layer, infile, outfile) {
  if (file.exists(outfile)) file.remove(outfile)
  st <- suppressWarnings(system2(rscript, c(shQuote(script), "--child", layer, KEEP_LAYER[[layer]], PER_FEATURE[[layer]],
                                            shQuote(infile), shQuote(outfile)), stdout = "", stderr = LOG))
  isTRUE(st == 0) && file.exists(outfile)
}

# Simplify a set of districts in a fresh process.  If that fails, or nothing was gained,
# split the countries in two and try again.  A single country that still fails is kept
# at full resolution and reported.
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

set_budget <- function(layer, x) {
  PER_FEATURE[[layer]] <<- TARGET_VERTS[[layer]] / nrow(x)
  if (is.finite(TARGET_VERTS[[layer]])) cat(sprintf("  %s: %s vertices in %s features; budget %s (about %.0f per feature)\n", layer,
      format(round(n_vertices(x)), big.mark = ","), format(nrow(x), big.mark = ","), format(TARGET_VERTS[[layer]], big.mark = ","), PER_FEATURE[[layer]]))
}

simplify_layer <- function(layer, try_whole = TRUE, chunked_ok = FALSE) {
  infile  <- file.path(CLEAN_DIR, paste0("africa_", layer, ".rds"))
  outfile <- file.path(CLEAN_DIR, paste0("africa_", layer, "_simplified.rds"))
  if (!file.exists(infile)) stop("Missing ", infile, " -- run R/01_clean_afrogdb.R first.")
  cat("\n", layer, ":\n", sep = "")
  x <- readRDS(infile)
  set_budget(layer, x)
  if (try_whole) {
    cat("  simplifying the whole layer ...\n")
    if (run_child(layer, infile, outfile)) { cat("  done (whole layer)\n"); return(invisible(readRDS(outfile))) }
    if (!chunked_ok) stop("Simplifying the ", layer, " layer failed (see ", LOG, ").")
    cat("  the whole layer did not fit; ")
  } else cat("  ")
  cat("simplifying groups of ", CHUNK, " countries ...\n", sep = "")

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
  cat("  done: ", nrow(out), " features, ", format(round(n_vertices(out)), big.mark = ","), " vertices", sep = "")
  if (length(full_res)) cat("; kept at full resolution: ", paste(full_res, collapse = ", "), sep = "")
  cat("\n")
  invisible(out)
}

simplify_layer("countries")
prov_s <- simplify_layer("provinces", try_whole = FALSE, chunked_ok = TRUE)
# The whole district layer crashes rmapshaper's engine on some machines, so groups are the default;
# set AFROGDB_TRY_WHOLE=1 to try the whole layer first.
dist_s <- simplify_layer("districts", try_whole = identical(Sys.getenv("AFROGDB_TRY_WHOLE"), "1"), chunked_ok = TRUE)

saveRDS(compute_ambiguous_district_pairs(dist_s), file.path(CLEAN_DIR, "ambiguous_district_pairs.rds"))
cat("\nDone. Simplified layers and ambiguous_district_pairs.rds are in ", normalizePath(CLEAN_DIR, mustWork = FALSE), "\n", sep = "")
tryCatch({ file.copy(LOG, file.path(CLEAN_DIR, "simplify_log.txt"), overwrite = TRUE); cat("Details of any warnings: ", file.path(CLEAN_DIR, "simplify_log.txt"), "\n", sep = "") },
         warning = function(w) cat("Details of any warnings: ", LOG, "\n", sep = ""),
         error = function(e) cat("Details of any warnings: ", LOG, "\n", sep = ""))
cat("\nFile sizes (MB) -- keep them small for fast maps:\n")
for (f in c("africa_countries_simplified.rds", "africa_provinces_simplified.rds", "africa_districts_simplified.rds"))
  cat(sprintf("  %-34s %6.1f\n", f, file.size(file.path(CLEAN_DIR, f)) / 1024^2))
