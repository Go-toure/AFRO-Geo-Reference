# ============================================================================
# afrogdb_standardize.R   (repo: AFRO-Geo-Reference)
#
# Functions that match country / province / district names to the AFRO geodatabase.
# Base R only.  Used by R/04_check_alignment.R and by the LQAS pipeline and the IM
# workflow (source this file, then call afrogdb_standardize()).
#
#   ref <- afrogdb_reference("path/to/AFRO-Geo-Reference")
#   df  <- afrogdb_standardize(df, "country", "province", "district", ref)
#
# afrogdb_standardize() returns the data with
#   country / province / district   the geodatabase names where the unit was found
#   country_raw / province_raw / district_raw   the names as they were
#   geo_code      geodatabase district code (NA when not found, and for the legacy-shape countries)
#   geo_match     how it was found: exact, normalized, other province, crosswalk, crosswalk (auto)
#                 (relaxed if relaxed = TRUE) ; or unmatched / ambiguous / no shape
# Names that were not found are left as they were.  attr(result, "unmatched") lists them.
#
# How a unit is found (in this order): exact; same after ignoring accents, apostrophes, hyphens,
# spaces and "RM "; the district name exists once in the country under another province;
# the decision in reference/crosswalk_geonames.csv (ok / auto; no_shape = known gap);
# optionally "relaxed" (generic words such as DISTRICT, DC, COUNCIL, CITY, TOWN ignored).
# Country aliases (TANZANIA, CAMEROUN ...) are in reference/crosswalk_country.csv.
# ============================================================================

.afro_ascii <- function(x) if (requireNamespace("stringi", quietly = TRUE)) stringi::stri_trans_general(x, "Latin-ASCII") else iconv(x, to = "ASCII//TRANSLIT")
afro_nk <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x <- toupper(.afro_ascii(trimws(x))); x <- sub("^RM[ -]+", "", x); gsub("[^A-Z0-9]", "", x) }
afro_up <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; toupper(trimws(x)) }
afro_rl <- function(x) {                       # relaxed: also ignores generic words that data files add or drop
  x <- as.character(x); x[is.na(x)] <- ""; x <- toupper(.afro_ascii(trimws(x))); x <- sub("^RM[ -]+", "", x)
  y <- gsub("\\b(DISTRICTS?|DIST|DC|COUNCIL|MUNICIPALITY|MUNICIPAL|MC|TC|CITY|TOWN|WOREDA|WOHO|WORHO)\\b", " ", x)
  y <- gsub("[^A-Z0-9]", "", y); ifelse(nchar(y) >= 3, y, gsub("[^A-Z0-9]", "", x))
}

afrogdb_reference <- function(dir = Sys.getenv("AFRO_GEO_REFERENCE_DIR", unset = file.path("..", "AFRO-Geo-Reference"))) {
  rd <- function(f) read.csv(file.path(dir, "reference", f), stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM", na.strings = "")
  ex <- function(f) file.exists(file.path(dir, "reference", f))
  if (!ex("afrogdb_geoname_master.csv")) stop("afrogdb_geoname_master.csv not found in ", file.path(dir, "reference"), " -- set AFRO_GEO_REFERENCE_DIR to the AFRO-Geo-Reference folder.")
  m <- rd("afrogdb_geoname_master.csv")
  ref_c <- m[m$level == "country", c("country", "geo_code")]
  ref_p <- m[m$level == "province", c("country", "province", "geo_code")]
  ref_d <- m[m$level == "district", c("country", "province", "district", "geo_code")]
  if (ex("legacy_units.csv")) {                # countries that still use legacy shapes (no geo_code)
    lg <- rd("legacy_units.csv")
    p <- lg[lg$level == "province", ]; d <- lg[lg$level == "district", ]
    if (nrow(p)) ref_p <- rbind(ref_p, data.frame(country = p$country, province = p$province, geo_code = NA_character_, stringsAsFactors = FALSE))
    if (nrow(d)) ref_d <- rbind(ref_d, data.frame(country = d$country, province = d$province, district = d$district, geo_code = NA_character_, stringsAsFactors = FALSE))
  }
  cw <- if (ex("crosswalk_country.csv")) rd("crosswalk_country.csv") else data.frame(raw_name = character(), std_country = character())
  xw <- if (ex("crosswalk_geonames.csv")) rd("crosswalk_geonames.csv") else NULL
  if (!is.null(xw) && nrow(xw)) xw$key <- paste(afro_nk(xw$country), afro_nk(xw$province), afro_nk(xw$district)) else xw <- NULL
  ref_c$ck <- afro_nk(ref_c$country)
  ref_p$ck <- afro_nk(ref_p$country); ref_p$pk <- afro_nk(ref_p$province)
  ref_d$ck <- afro_nk(ref_d$country); ref_d$pk <- afro_nk(ref_d$province); ref_d$dk <- afro_nk(ref_d$district); ref_d$rk <- afro_rl(ref_d$district)
  ref_d$ex <- paste(afro_up(ref_d$country), afro_up(ref_d$province), afro_up(ref_d$district), sep = "|")
  list(c = ref_c, p = ref_p, d = ref_d, xw = xw, alias = setNames(afro_nk(cw$std_country), afro_nk(cw$raw_name)), dir = dir)
}

# d: data frame with columns country, province, district (distinct units; a "rows" column is kept if present)
afrogdb_match <- function(d, ref, suggest = FALSE, relaxed = TRUE) {
  d$ck0 <- afro_nk(d$country); hit <- unname(ref$alias[d$ck0]); d$ck <- ifelse(is.na(hit), d$ck0, hit)
  d$pk <- afro_nk(d$province); d$dk <- afro_nk(d$district)
  rc <- ref$c; rp <- ref$p; rd_ <- ref$d

  d$country_status <- ifelse(afro_up(d$country) %in% afro_up(rc$country), "exact", ifelse(d$ck %in% rc$ck, "normalized", "unmatched"))
  d$std_country <- rc$country[match(d$ck, rc$ck)]
  d$country_geo_code <- rc$geo_code[match(d$ck, rc$ck)]
  pm <- match(paste(d$ck, d$pk), paste(rp$ck, rp$pk))
  d$province_status <- ifelse(d$country_status == "unmatched", "unmatched", ifelse(is.na(pm), "unmatched",
                        ifelse(afro_up(d$province) == afro_up(rp$province[pm]) & d$country_status == "exact", "exact", "normalized")))
  d$std_province <- rp$province[pm]; d$province_geo_code <- rp$geo_code[pm]

  dm <- match(paste(d$ck, d$pk, d$dk), paste(rd_$ck, rd_$pk, rd_$dk))
  cd <- paste(d$ck, d$dk); rcd <- paste(rd_$ck, rd_$dk); ncd <- table(rcd)
  d$status <- "unmatched"; d$std_district <- NA_character_; d$geo_code <- NA_character_; d$std_province2 <- NA_character_
  ok <- !is.na(dm)
  d$status[ok] <- ifelse(paste(afro_up(d$country), afro_up(d$province), afro_up(d$district), sep = "|")[ok] == rd_$ex[dm[ok]], "exact", "normalized")
  d$std_district[ok] <- rd_$district[dm[ok]]; d$geo_code[ok] <- rd_$geo_code[dm[ok]]; d$std_province2[ok] <- rd_$province[dm[ok]]
  for (i in which(!ok & d$ck %in% rc$ck)) {
    n <- as.integer(ncd[cd[i]]); n <- ifelse(is.na(n), 0L, n)
    if (n == 1L) { j <- match(cd[i], rcd); d$status[i] <- "other province"; d$std_district[i] <- rd_$district[j]; d$geo_code[i] <- rd_$geo_code[j]; d$std_province2[i] <- rd_$province[j] }
    else if (n > 1L) d$status[i] <- "ambiguous"
  }
  xw <- ref$xw                                  # reviewed decisions
  if (!is.null(xw)) {
    xi <- match(paste(d$ck0, d$pk, d$dk), xw$key)
    for (i in which(!is.na(xi) & d$status %in% c("unmatched", "ambiguous", "other province"))) {
      r <- xw[xi[i], ]; dec <- tolower(trimws(ifelse(is.na(r$decision), "", r$decision)))
      if (dec == "no_shape") { d$status[i] <- "no shape"; next }
      if (!dec %in% c("ok", "auto")) next
      lab <- ifelse(dec == "ok", "crosswalk", "crosswalk (auto)")
      j <- if (!is.na(r$geo_code)) match(r$geo_code, rd_$geo_code) else NA
      if (!is.na(j)) { d$std_district[i] <- rd_$district[j]; d$std_province2[i] <- rd_$province[j]; d$geo_code[i] <- rd_$geo_code[j]; d$status[i] <- lab }
      else if (!is.na(r$std_district)) { d$std_district[i] <- r$std_district; d$std_province2[i] <- r$std_province; d$status[i] <- lab }
    }
  }
  if (relaxed) {
    d$rk <- afro_rl(d$district)
    for (i in which(d$status %in% c("unmatched", "ambiguous") & d$ck %in% rc$ck)) {
      pool <- which(rd_$ck == d$ck[i] & rd_$rk == d$rk[i])
      if (length(pool) > 1) pool <- pool[rd_$pk[pool] == d$pk[i]]
      if (length(pool) == 1) { j <- pool; d$status[i] <- "relaxed"; d$std_district[i] <- rd_$district[j]; d$geo_code[i] <- rd_$geo_code[j]; d$std_province2[i] <- rd_$province[j] }
    }
  }
  d$std_province_final <- ifelse(!is.na(d$std_province2), d$std_province2, d$std_province)
  d$suggestion <- NA_character_; d$suggestion_geo_code <- NA_character_; d$suggestion_distance <- NA_real_
  if (suggest) {                                 # closest names, for a person to review: never applied automatically
    for (i in which(d$status %in% c("unmatched", "ambiguous") & d$ck %in% rc$ck)) {
      pool <- which(rd_$ck == d$ck[i] & (is.na(pm[i]) | rd_$pk == d$pk[i]))
      if (!length(pool)) pool <- which(rd_$ck == d$ck[i])
      if (!length(pool) || !nzchar(d$dk[i])) next
      dist <- as.numeric(adist(d$dk[i], rd_$dk[pool])) / pmax(nchar(d$dk[i]), nchar(rd_$dk[pool]))
      k <- which.min(dist)
      if (dist[k] <= 0.4) { j <- pool[k]; d$suggestion[i] <- paste(rd_$province[j], rd_$district[j], sep = " / "); d$suggestion_geo_code[i] <- rd_$geo_code[j]; d$suggestion_distance[i] <- round(dist[k], 2) }
    }
  }
  d
}

FOUND <- c("exact", "normalized", "other province", "crosswalk", "crosswalk (auto)")

afrogdb_standardize <- function(df, country = "country", province = "province", district = "district",
                                ref = afrogdb_reference(), relaxed = FALSE, replace_names = TRUE, verbose = TRUE) {
  stopifnot(all(c(country, province, district) %in% names(df)))
  raw <- data.frame(country = as.character(df[[country]]), province = as.character(df[[province]]), district = as.character(df[[district]]), stringsAsFactors = FALSE)
  key <- paste(raw$country, raw$province, raw$district, sep = "\r")
  u <- raw[!duplicated(key), , drop = FALSE]; ukey <- key[!duplicated(key)]
  m <- afrogdb_match(u, ref, suggest = FALSE, relaxed = relaxed)
  i <- match(key, ukey)
  found <- m$status %in% c(FOUND, if (relaxed) "relaxed")
  df$geo_code  <- ifelse(found, m$geo_code, NA_character_)[i]
  df$geo_match <- m$status[i]
  if (replace_names) {
    df[[paste0(country, "_raw")]] <- raw$country; df[[paste0(province, "_raw")]] <- raw$province; df[[paste0(district, "_raw")]] <- raw$district
    c_ok <- (m$country_status != "unmatched")[i]
    df[[country]]  <- ifelse(c_ok, m$std_country[i], raw$country)
    d_ok <- found[i] & !is.na(m$std_district[i])
    df[[province]] <- ifelse(d_ok, m$std_province_final[i], ifelse((m$province_status != "unmatched")[i], m$std_province[i], raw$province))
    df[[district]] <- ifelse(d_ok, m$std_district[i], raw$district)
  }
  un <- m[!found, c("country", "province", "district", "status"), drop = FALSE]
  un$rows <- as.integer(table(factor(key, levels = ukey))[!found])
  un <- un[order(-un$rows), ]
  attr(df, "unmatched") <- un
  if (verbose) {
    cat(sprintf("AFROGDB alignment: %s rows; %s found in the geodatabase (%.1f%%); not found: %s rows in %d units\n",
                format(nrow(df), big.mark = ","), format(sum(found[i]), big.mark = ","), 100 * mean(found[i]), format(sum(!found[i]), big.mark = ","), nrow(un)))
  }
  df
}

# ----------------------------------------------------------------------------
# afrogdb_standardize_best()
# Maximum-match version used by the pipelines. A pipeline renames districts with its own
# (older) rules before alignment; those rules sometimes help (they reach a geodatabase name
# the original spelling does not) and sometimes hurt (they rename a name AWAY from the
# geodatabase spelling). So every row is matched twice and the better result is kept:
#   current  = the names as they are now (after the pipeline's own renames)
#   original = the names before those renames (alt_province / alt_district, same row order)
# Quality of a match: 3 = exact / normalized / reviewed crosswalk, 2 = proposed crosswalk or other
# province, 1 = relaxed, 0 = not found. The original names win only when strictly better;
# on a tie the current names are kept (today's behaviour). Rows where both match but to a
# different geodatabase unit are counted and listed in attr(, "conflicts") for review.
# ----------------------------------------------------------------------------
afrogdb_standardize_best <- function(df, alt_province, alt_district,
                                     country = "country", province = "province", district = "district",
                                     ref = afrogdb_reference(), relaxed = FALSE, replace_names = TRUE, verbose = TRUE) {
  stopifnot(length(alt_province) == nrow(df), length(alt_district) == nrow(df))
  b <- afrogdb_standardize(df, country, province, district, ref = ref, relaxed = relaxed, replace_names = replace_names, verbose = FALSE)
  alt <- df
  alt[[province]] <- alt_province
  alt[[district]] <- alt_district
  a <- afrogdb_standardize(alt, country, province, district, ref = ref, relaxed = relaxed, replace_names = replace_names, verbose = FALSE)
  tier <- function(s) {
    out <- rep(0L, length(s))
    out[s %in% c("exact", "normalized", "crosswalk")] <- 3L
    out[s %in% c("crosswalk (auto)", "other province")] <- 2L
    out[s %in% "relaxed"] <- 1L
    out
  }
  use_a <- tier(a$geo_match) > tier(b$geo_match)
  conflict <- !use_a & tier(a$geo_match) > 0L & tier(b$geo_match) > 0L & !is.na(a$geo_code) & !is.na(b$geo_code) & a$geo_code != b$geo_code
  res <- b
  if (any(use_a)) {
    for (cl in c("geo_code", "geo_match", if (replace_names) c(country, province, district))) res[[cl]][use_a] <- a[[cl]][use_a]
  }
  found <- !is.na(res$geo_code)
  nf <- which(!found)
  un <- data.frame(country = res[[country]][nf], province = res[[province]][nf], district = res[[district]][nf],
                   status = res$geo_match[nf], stringsAsFactors = FALSE)
  if (nrow(un)) {
    k <- paste(un$country, un$province, un$district, sep = "\r")
    un <- cbind(un[!duplicated(k), , drop = FALSE], rows = as.integer(table(factor(k, levels = unique(k)))))
    un <- un[order(-un$rows), ]
  } else un$rows <- integer(0)
  attr(res, "unmatched") <- un
  if (any(conflict)) {
    cf <- data.frame(country = res[[country]][conflict], province_current = df[[province]][conflict], district_current = df[[district]][conflict],
                     province_original = alt_province[conflict], district_original = alt_district[conflict],
                     geo_code_current = b$geo_code[conflict], geo_code_original = a$geo_code[conflict], stringsAsFactors = FALSE)
    attr(res, "conflicts") <- cf[!duplicated(cf), , drop = FALSE]
  }
  if (verbose) {
    cat(sprintf("AFROGDB alignment: %s rows; %s found in the geodatabase (%.1f%%); not found: %s rows in %d units; %s rows matched only through the original names; %s rows match two different units (kept the current)\n",
                format(nrow(res), big.mark = ","), format(sum(found), big.mark = ","), 100 * mean(found), format(sum(!found), big.mark = ","), nrow(un),
                format(sum(use_a & found), big.mark = ","), format(sum(conflict), big.mark = ",")))
  }
  res
}
