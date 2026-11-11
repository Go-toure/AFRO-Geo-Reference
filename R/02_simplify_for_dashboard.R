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
# every layer is simplified in a fresh R process.  If the whole district layer is
# too big for that, it is simplified in groups of countries (borders between
# countries in different groups can then differ very slightly; the country layer
# drawn on top hides this).
#
# Run from the repository root:
#   Rscript R/02_simplify_for_dashboard.R
# Optional environment variables:
#   AFRO_DASH_DIR=<folder>      AFRO-Dash checkout (default ../AFRO-Dash)
#   AFROGDB_CLEAN_DIR=<folder>  where the layers are (default data/clean)
#   AFROGDB_CHUNK=<n>           countries per group in the fallback (default 6)
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
# Rscript R/02_simplify_for_dashboard.R --child <layer> <in.rds> <out.rds>
if (length(args) == 4 && args[1] == "--child") {
  x <- readRDS(args[3])
  saveRDS(preprocess_shapefile(x, args[2]), args[4])
  quit(save = "no", status = 0)
}

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
rscript <- file.path(R.home("bin"), "Rscript")

run_child <- function(layer, infile, outfile) {
  if (file.exists(outfile)) file.remove(outfile)
  st <- suppressWarnings(system2(rscript, c(shQuote(script), "--child", layer, shQuote(infile), shQuote(outfile)),
                                 stdout = "", stderr = ""))
  isTRUE(st == 0) && file.exists(outfile)
}

simplify_layer <- function(layer, chunked_ok = FALSE) {
  infile  <- file.path(CLEAN_DIR, paste0("africa_", layer, ".rds"))
  outfile <- file.path(CLEAN_DIR, paste0("africa_", layer, "_simplified.rds"))
  if (!file.exists(infile)) stop("Missing ", infile, " -- run R/01_clean_afrogdb.R first.")
  cat("\n", layer, ": simplifying the whole layer ...\n", sep = "")
  if (run_child(layer, infile, outfile)) { cat("  done (whole layer)\n"); return(invisible(readRDS(outfile))) }
  if (!chunked_ok) stop("Simplifying the ", layer, " layer failed.")

  cat("  the whole layer did not fit; simplifying groups of ", CHUNK, " countries instead ...\n", sep = "")
  x <- readRDS(infile)
  countries <- sort(unique(x$ADM0_NAME))
  groups <- split(countries, ceiling(seq_along(countries) / CHUNK))
  tmp <- tempfile("afrogdb_chunk_"); dir.create(tmp)
  parts <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    cin <- file.path(tmp, sprintf("in_%02d.rds", i)); cout <- file.path(tmp, sprintf("out_%02d.rds", i))
    saveRDS(x[x$ADM0_NAME %in% groups[[i]], ], cin)
    cat(sprintf("  group %d/%d: %s\n", i, length(groups), paste(groups[[i]], collapse = ", ")))
    if (!run_child(layer, cin, cout)) {
      stop("Group ", i, " failed. Try a smaller group size, e.g. AFROGDB_CHUNK=3 Rscript R/02_simplify_for_dashboard.R")
    }
    parts[[i]] <- readRDS(cout)
  }
  out <- do.call(rbind, parts)
  stopifnot(nrow(out) == nrow(x))
  saveRDS(out, outfile)
  cat("  done (", length(groups), " groups, ", nrow(out), " features)\n", sep = "")
  invisible(out)
}

simplify_layer("countries")
simplify_layer("provinces")
dist_s <- simplify_layer("districts", chunked_ok = TRUE)

saveRDS(compute_ambiguous_district_pairs(dist_s), file.path(CLEAN_DIR, "ambiguous_district_pairs.rds"))
cat("\nDone. Simplified layers and ambiguous_district_pairs.rds are in ", normalizePath(CLEAN_DIR, mustWork = FALSE), "\n", sep = "")
