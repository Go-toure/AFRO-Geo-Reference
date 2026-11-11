# ============================================================================
# 05_build_review_file.R   (repo: AFRO-Geo-Reference)
#
# Builds the file where geoname decisions are made once, for every data source:
#   reference/crosswalk_geonames.csv
# One line per raw country / province / district that did not match the
# geodatabase cleanly (from the reports of R/04_check_alignment.R, all sources together,
# largest first).  Open it in Excel, check the proposals, and fill the gaps:
#
#   geo_code   the geodatabase unit (district geo_code from reference/afrogdb_geoname_master.csv)
#   decision   ok       reviewed and correct
#              auto     proposed by the script (same name in another province, generic words
#                       ignored, or a very close name): used by the check, please review
#              no_shape the geodatabase has no unit for it yet (a boundary to ask for)
#              (empty)  not decided yet
#   note       free text
#
# Lines you have already decided are never changed; running this again only adds new raw names.
# R/04_check_alignment.R reads the file, so its numbers show the effect of your decisions, and the
# LQAS pipeline / IM workflow use the same file as their crosswalk.
#
# Run from the repository root after R/04_check_alignment.R:
#   Rscript R/05_build_review_file.R
# ============================================================================

suppressPackageStartupMessages(library(dplyr))
files <- list.files("reports", "^crosswalk_.*\\.csv$", full.names = TRUE)
if (!length(files)) stop("No reports/crosswalk_*.csv -- run R/04_check_alignment.R first.")
rd <- function(f) read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "")
all <- bind_rows(lapply(files, function(f) { x <- rd(f); x$source <- gsub("^crosswalk_|\\.csv$", "", basename(f)); x }))

need <- all |> filter(status %in% c("unmatched", "ambiguous", "other province", "relaxed"))
agg <- need |> group_by(country, province, district) |>
  summarise(rows = sum(rows), sources = paste(sort(unique(source)), collapse = "+"), status = first(status),
            geo_code = first(geo_code), std_country = first(std_country), std_province = first(std_province), std_district = first(std_district),
            suggestion = first(suggestion), suggestion_geo_code = first(suggestion_geo_code), suggestion_distance = first(suggestion_distance), .groups = "drop")

prop_code <- ifelse(agg$status %in% c("other province", "relaxed"), agg$geo_code,
             ifelse(!is.na(agg$suggestion_distance) & agg$suggestion_distance <= 0.15, agg$suggestion_geo_code, NA))
has_prop  <- agg$status %in% c("other province", "relaxed") | (!is.na(agg$suggestion_distance) & agg$suggestion_distance <= 0.15)

# A close name is NOT proposed when the two names differ by a number or a direction word:
# "ABOMEY-CALAVI 1" is one of the units that replaced "ABOMEY-CALAVI", and "N'DJAMENA SUD" is
# not "N'DJAMENA". Those are split (or merged) districts, not spelling mistakes; they need the
# new boundary, or a decision by a person. They stay undecided and the reason is written in the note.
split_tokens <- function(x) {
  x <- toupper(iconv(as.character(x), to = "ASCII//TRANSLIT")); x[is.na(x)] <- ""
  lapply(strsplit(x, "[^A-Z0-9]+"), function(t) sort(unique(t[grepl("^([0-9]+|I|II|III|IV|V|VI|VII|VIII|IX|X|NORD|SUD|EST|OUEST|CENTRE|CENTRAL|NORTH|SOUTH|EAST|WEST|URBAN|RURAL|URBAIN|RURALE|HAUT|HAUTE|BAS|BASSE)$", t)])))
}
sug_only  <- has_prop & !agg$status %in% c("other province", "relaxed")
sug_name  <- sub("^.* / ", "", agg$suggestion)
split_like <- sug_only & !mapply(identical, split_tokens(agg$district), split_tokens(sug_name))
if (any(split_like)) cat(sprintf("%d close names not proposed because they differ by a number or direction (split districts), e.g. %s\n",
                                sum(split_like), paste(head(agg$district[split_like], 3), collapse = ", ")))
has_prop[split_like] <- FALSE
prop_code[split_like] <- NA
new <- agg |> transmute(country, province, district, rows, sources, status_in_check = status,
                        geo_code = prop_code,
                        std_country, std_province = ifelse(has_prop, std_province, NA), std_district = ifelse(has_prop, std_district, NA),
                        decision = ifelse(has_prop, "auto", NA_character_),
                        suggestion, suggestion_distance,
                        note = ifelse(split_like, paste0("looks like a split or merged district of ", agg$suggestion, ": needs the new boundary or a decision"), NA_character_))
# a proposal found through the suggestion carries the suggested names
sug <- !agg$status %in% c("other province", "relaxed") & has_prop
if (any(sug)) {
  m <- read.csv("reference/afrogdb_geoname_master.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "") |> filter(level == "district")
  j <- match(new$geo_code[sug], m$geo_code)
  new$std_province[sug] <- m$province[j]; new$std_district[sug] <- m$district[j]
}

nk <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- toupper(trimws(x)); gsub("[^A-Z0-9]", "", iconv(x, to = "ASCII//TRANSLIT")) }
out_f <- file.path("reference", "crosswalk_geonames.csv")
old <- if (file.exists(out_f)) rd(out_f) else NULL
if (!is.null(old)) {
  keep_new <- !paste(nk(new$country), nk(new$province), nk(new$district)) %in% paste(nk(old$country), nk(old$province), nk(old$district))
  cat(sprintf("%d existing lines kept, %d new lines added\n", nrow(old), sum(keep_new)))
  for (cn in setdiff(names(new), names(old))) old[[cn]] <- NA
  res <- bind_rows(old, new[keep_new, ])
} else res <- new
res <- res |> arrange(is.na(decision) & TRUE, desc(rows))
res <- res[order(-res$rows), ]

# Hand-written decisions that are kept in the repository: reference/crosswalk_manual.csv
# (country, province [blank = any], district, geo_code, decision, note). They fill lines that are empty
# or only proposed by the script; a line you already marked "ok" yourself is never changed.
man_f <- file.path("reference", "crosswalk_manual.csv")
if (file.exists(man_f)) {
  man <- rd(man_f)
  mst <- read.csv("reference/afrogdb_geoname_master.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "")
  n_applied <- 0
  for (i in seq_len(nrow(man))) {
    hit <- nk(res$country) == nk(man$country[i]) & nk(res$district) == nk(man$district[i])
    if (!is.na(man$province[i])) hit <- hit & nk(res$province) == nk(man$province[i])
    hit <- hit & (is.na(res$decision) | tolower(trimws(res$decision)) == "auto")
    if (!any(hit)) next
    res$geo_code[hit] <- man$geo_code[i]; res$decision[hit] <- man$decision[i]; res$note[hit] <- man$note[i]
    j <- match(man$geo_code[i], mst$geo_code[mst$level == "district"]); md <- mst[mst$level == "district", ]
    res$std_province[hit] <- if (is.na(j)) NA else md$province[j]; res$std_district[hit] <- if (is.na(j)) NA else md$district[j]
    n_applied <- n_applied + sum(hit)
  }
  cat(sprintf("%d lines set from %s\n", n_applied, man_f))
}

dir.create("reference", showWarnings = FALSE)
tryCatch({
  con <- file(out_f, "wb"); writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con); close(con)      # BOM so Excel reads the accents
  suppressWarnings(write.table(res, out_f, sep = ",", row.names = FALSE, na = "", qmethod = "double", append = TRUE, fileEncoding = "UTF-8"))
}, error = function(e) cat("WARNING: could not write ", out_f, " (open in Excel?) -- close it and run again.\n", sep = ""))

dec <- tolower(trimws(ifelse(is.na(res$decision), "", res$decision)))
cat(sprintf("\n%s: %d lines, %s data rows\n", out_f, nrow(res), format(sum(res$rows), big.mark = ",")))
for (k in c("ok", "auto", "no_shape", "")) cat(sprintf("  decision %-9s %5d lines  %s rows\n", ifelse(k == "", "(empty)", k), sum(dec == k), format(sum(res$rows[dec == k]), big.mark = ",")))
cat("\nLargest lines still to decide:\n")
print(head(res[dec == "", c("country", "province", "district", "rows", "sources", "suggestion")], 12), row.names = FALSE)
