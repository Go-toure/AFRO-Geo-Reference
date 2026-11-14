#!/usr/bin/env Rscript
# ============================================================
# STEP 6: independent cross-check, old shapes against AFROGDB
# ============================================================
# The crosswalk (reference/crosswalk_geonames.csv + crosswalk_manual.csv) says which AFROGDB unit each raw name belongs to.
# This script checks every decision a second way, with no names involved: it overlays the OLD district shapes
# (the layer the data was spelled against) on the NEW AFROGDB districts and asks "does the unit the crosswalk chose
# cover the place where the old unit was?". It then does the same for names that are still undecided and proposes a unit
# with a probability, learned from the decisions that people already confirmed.
#
#   Rscript R/06_crosscheck_shapes.R
#
# Needs: sf, dplyr, stringi, data.table. Run from the repository root of AFRO-Geo-Reference.
# Input:  old layer   AFROGDB_OLD_SHAPE   (default: ../AFRO-Dash/_to_delete/old_shape_check/old_africa_districts_simplified.rds,
#                                           or the file africa_districts_simplified.rds of AFRO-Dash at commit 158d753)
#         new layer   data/clean/africa_districts_simplified.rds
#         decisions   reference/crosswalk_geonames.csv, reference/crosswalk_manual.csv
# Output: reference/crosscheck_old_vs_new.csv   every decision with its spatial evidence and verdict
#         reference/crosscheck_review.csv       only what needs a person: disagreements and proposals
#         reference/crosscheck_model.txt        how well the proposal model works (measured on confirmed decisions)
# Nothing in the crosswalk is changed. A person decides.
# ============================================================

suppressPackageStartupMessages({ library(sf); library(dplyr); library(stringi); library(data.table) })
sf_use_s2(FALSE)

OLD_CANDIDATES <- c(Sys.getenv("AFROGDB_OLD_SHAPE", ""),
                    "data/legacy/old_africa_districts_simplified_158d753.rds",
                    "../AFRO-Dash/_to_delete/old_shape_check/old_africa_districts_simplified.rds")
OLD_FILE <- OLD_CANDIDATES[nzchar(OLD_CANDIDATES) & file.exists(OLD_CANDIDATES)][1]
if (is.na(OLD_FILE)) OLD_FILE <- OLD_CANDIDATES[2]
NEW_FILE <- Sys.getenv("AFROGDB_NEW_SHAPE", "data/clean/africa_districts_simplified.rds")
REF_DIR  <- Sys.getenv("AFROGDB_REF_DIR", "reference")
MIN_PART <- 0.02       # overlaps smaller than 2% of either unit are ignored (slivers)
AUTO_P   <- 0.90       # a proposal is called a strong candidate from this probability

if (!file.exists(OLD_FILE)) {
  tmp <- tempfile(fileext = ".rds")
  ok <- tryCatch(system2("git", c("-C", "../AFRO-Dash", "show", "158d753:input/africa_districts_simplified.rds"), stdout = tmp) == 0, error = function(e) FALSE)
  if (ok && file.exists(tmp) && file.size(tmp) > 1e6) OLD_FILE <- tmp else stop("old shapes not found: set AFROGDB_OLD_SHAPE")
}
msg <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), paste0(...)))

norm <- function(x) {
  x <- toupper(stri_trans_general(as.character(x), "Latin-ASCII"))
  x <- gsub("[^A-Z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}
sim <- function(a, b) {   # name similarity 0..1 from the edit distance of the normalised names
  a <- norm(a); b <- norm(b)
  mapply(function(x, y) if (is.na(x) || is.na(y) || !nzchar(x) || !nzchar(y)) NA_real_ else 1 - drop(adist(x, y)) / max(nchar(x), nchar(y)), a, b, USE.NAMES = FALSE)
}

# ---- 1 overlay old against new, country by country ------------------------------------------------------
msg("reading the layers")
old <- readRDS(OLD_FILE); new <- readRDS(NEW_FILE)
for (nm in c("ADM0_NAME", "ADM1_NAME", "ADM2_NAME")) { stopifnot(nm %in% names(old), nm %in% names(new)) }
stopifnot("GEO_CODE" %in% names(new))
old$c0 <- norm(old$ADM0_NAME); new$c0 <- norm(new$ADM0_NAME)
countries <- intersect(unique(old$c0), unique(new$c0))
msg(length(countries), " countries in both layers")

fix <- function(x) { x <- st_make_valid(x); x <- st_buffer(x, 0); st_transform(x, 3857) }
links <- list()
for (cn in countries) {
  o <- old[old$c0 == cn, c("ADM1_NAME", "ADM2_NAME")]; n <- new[new$c0 == cn, c("GEO_CODE", "ADM1_NAME", "ADM2_NAME")]
  o <- tryCatch(fix(o), error = function(e) NULL); n <- tryCatch(fix(n), error = function(e) NULL)
  if (is.null(o) || is.null(n) || !nrow(o) || !nrow(n)) next
  o$oid <- seq_len(nrow(o)); n$nid <- seq_len(nrow(n))
  o$oa <- as.numeric(st_area(o)); n$na <- as.numeric(st_area(n))
  i <- tryCatch(suppressWarnings(st_intersection(o[, c("oid", "oa")], n[, c("nid", "GEO_CODE", "na")])), error = function(e) NULL)
  if (is.null(i) || !nrow(i)) next
  i$ia <- as.numeric(st_area(i)); i <- st_drop_geometry(i)
  i <- i[i$ia / i$oa >= MIN_PART | i$ia / i$na >= MIN_PART, ]
  i$country <- cn
  od <- st_drop_geometry(o)[, c("oid", "ADM1_NAME", "ADM2_NAME")]; names(od) <- c("oid", "old_province", "old_district")
  nd <- st_drop_geometry(n)[, c("nid", "ADM1_NAME", "ADM2_NAME")]; names(nd) <- c("nid", "new_province", "new_district")
  links[[cn]] <- i %>% left_join(od, by = "oid") %>% left_join(nd, by = "nid")
}
L <- bind_rows(links)
L <- L %>% mutate(contain = ia / oa, cover = ia / na, iou = ia / (oa + na - ia))
msg(nrow(L), " old-new overlaps")

# best new unit for every old unit, and how the old unit relates to the new layer
best <- L %>% group_by(country, oid, old_province, old_district) %>%
  arrange(desc(ia), .by_group = TRUE) %>%
  summarise(best_code = GEO_CODE[1], best_province = new_province[1], best_district = new_district[1],
            contain = contain[1], cover = cover[1], iou = iou[1],
            n_new = sum(contain >= 0.10), .groups = "drop") %>%
  mutate(link_type = case_when(
    contain >= 0.8 & cover >= 0.8 ~ "SAME",
    contain >= 0.8 & cover <  0.8 ~ "OLD_INSIDE_NEWER_UNIT",
    n_new >= 2 & contain < 0.8    ~ "OLD_SPLIT",
    TRUE                          ~ "SHIFTED"))
best$k_old <- paste(best$country, norm(best$old_province), norm(best$old_district))

# ---- 2 the decisions already taken ----------------------------------------------------------------------
rd <- function(f) as.data.frame(data.table::fread(file.path(REF_DIR, f), encoding = "UTF-8", na.strings = c("", "NA"), data.table = FALSE))   # reads the BOM and any locale
cw <- rd("crosswalk_geonames.csv"); mn <- rd("crosswalk_manual.csv")
cw <- cw %>% transmute(country, province, district, rows = suppressWarnings(as.integer(rows)), geo_code, decision, source = "crosswalk_geonames", sources_in = sources, suggestion)
mn <- mn %>% transmute(country, province, district, rows = NA_integer_, geo_code, decision, source = "crosswalk_manual", sources_in = NA_character_, suggestion = NA_character_)
key <- function(d) paste(toupper(d$country), toupper(d$province), toupper(d$district))
cw$key <- key(cw); mn$key <- key(mn)
mn_keys <- mn$key[!is.na(mn$geo_code)]
X <- bind_rows(mn, cw[!(cw$key %in% mn_keys), ])    # the manual file wins over the generated one
X$rows[is.na(X$rows)] <- 0L
X$c0 <- norm(X$country)
X$k_old <- paste(X$c0, norm(X$province), norm(X$district))

# find the old unit of each raw name: exact (country, province, district), else (country, district) when it is unique
dist_key <- paste(best$country, norm(best$old_district))
uniq_dist <- names(which(table(dist_key) == 1))
X$old_row <- match(X$k_old, best$k_old)
alt <- match(paste(X$c0, norm(X$district)), dist_key)
use_alt <- is.na(X$old_row) & !is.na(alt) & paste(X$c0, norm(X$district)) %in% uniq_dist
X$old_row[use_alt] <- alt[use_alt]
X$old_found <- !is.na(X$old_row)
B <- best[X$old_row, c("best_code", "best_province", "best_district", "contain", "cover", "iou", "n_new", "link_type")]
names(B) <- c("sp_code", "sp_province", "sp_district", "sp_contain", "sp_cover", "sp_iou", "sp_n_new", "sp_link")
X <- bind_cols(X, B)
X$name_sim <- sim(X$district, X$sp_district)
X$same_prov <- as.numeric(norm(X$province) == norm(X$sp_province))

# how much of the old unit lies inside the unit the crosswalk chose (independent of which unit was "best")
Lk <- L %>% mutate(k_old = paste(country, norm(old_province), norm(old_district))) %>% select(k_old, GEO_CODE, contain_in_chosen = contain)
Lk <- Lk[!duplicated(paste(Lk$k_old, Lk$GEO_CODE)), ]
X <- left_join(X, Lk, by = c("k_old" = "k_old", "geo_code" = "GEO_CODE"))
X$contain_in_chosen[is.na(X$contain_in_chosen) & X$old_found & !is.na(X$geo_code)] <- 0

# ---- 3 verdict on every decision ------------------------------------------------------------------------
X$verdict <- case_when(
  is.na(X$geo_code) & !X$old_found ~ "UNDECIDED_NO_OLD_SHAPE",
  is.na(X$geo_code)                ~ "UNDECIDED",
  !X$old_found                     ~ "NO_OLD_SHAPE",
  X$sp_code == X$geo_code | X$contain_in_chosen >= 0.5 ~ "AGREE",
  X$contain_in_chosen >= 0.2       ~ "PARTLY",
  X$sp_contain >= 0.6              ~ "DISAGREE",
  TRUE                             ~ "WEAK")

# ---- 4 proposal model: how likely is the best spatial unit to be the right one? ----------------------------
gold <- X %>% filter(decision == "ok", !is.na(geo_code), old_found, !is.na(sp_code))
gold$y <- as.integer(gold$sp_code == gold$geo_code | gold$contain_in_chosen >= 0.5)
feat <- function(d) data.frame(contain = d$sp_contain, iou = d$sp_iou, name_sim = ifelse(is.na(d$name_sim), 0, d$name_sim), same_prov = ifelse(is.na(d$same_prov), 0, d$same_prov), n_new = pmin(d$sp_n_new, 5))
auc <- function(p, y) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); if (n1 == 0 || n0 == 0) NA else (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
model <- NULL; rep <- character()
rep <- c(rep, sprintf("Confirmed decisions with an old shape: %d (spatial best = chosen unit in %d, %.1f%%)", nrow(gold), sum(gold$y), 100 * mean(gold$y)))
if (nrow(gold) >= 40 && sum(gold$y == 0) >= 8 && sum(gold$y == 1) >= 8) {
  F <- feat(gold); F$y <- gold$y
  set.seed(1); fold <- sample(rep(1:5, length.out = nrow(F)))
  pcv <- numeric(nrow(F))
  for (k in 1:5) { m <- suppressWarnings(glm(y ~ ., data = F[fold != k, ], family = binomial)); pcv[fold == k] <- suppressWarnings(predict(m, F[fold == k, ], type = "response")) }
  model <- suppressWarnings(glm(y ~ ., data = F, family = binomial))
  hi <- pcv >= AUTO_P
  rep <- c(rep, sprintf("5-fold cross-validation: AUC %.3f; accuracy at 0.5 = %.1f%%", auc(pcv, F$y), 100 * mean((pcv >= 0.5) == (F$y == 1))),
           sprintf("Proposals with probability >= %.2f: %d of %d, of which right: %.1f%%", AUTO_P, sum(hi), nrow(F), if (sum(hi)) 100 * mean(F$y[hi] == 1) else NA),
           "Coefficients: " %>% paste0(paste(sprintf("%s=%.2f", names(coef(model)), coef(model)), collapse = ", ")))
} else {
  rep <- c(rep, "Too few confirmed decisions that disagree with the spatial best unit to fit a model: proposals use a fixed score (0.5 containment + 0.3 overlap + 0.2 name similarity), not a probability.")
}
X$p_right <- NA_real_
ok <- X$old_found & !is.na(X$sp_code)
if (!is.null(model)) X$p_right[ok] <- suppressWarnings(predict(model, feat(X[ok, ]), type = "response")) else
  X$p_right[ok] <- 0.5 * X$sp_contain[ok] + 0.3 * X$sp_iou[ok] + 0.2 * ifelse(is.na(X$name_sim[ok]), 0, X$name_sim[ok])

# ---- 5 outputs ---------------------------------------------------------------------------------------
out <- X %>% transmute(country, province, district, rows, decision, source, geo_code_crosswalk = geo_code,
                       old_unit_found = old_found, spatial_best_code = sp_code, spatial_best_district = sp_district, spatial_best_province = sp_province,
                       old_inside_chosen = round(contain_in_chosen, 3), old_inside_best = round(sp_contain, 3), overlap_iou = round(sp_iou, 3),
                       link_type = sp_link, name_similarity = round(name_sim, 2), p_best_is_right = round(p_right, 3), verdict) %>%
  arrange(factor(verdict, c("DISAGREE", "WEAK", "PARTLY", "UNDECIDED", "UNDECIDED_NO_OLD_SHAPE", "NO_OLD_SHAPE", "AGREE")), desc(rows))
write.csv(out, file.path(REF_DIR, "crosscheck_old_vs_new.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")
rv <- out %>% filter(verdict %in% c("DISAGREE", "WEAK", "PARTLY") | (verdict == "UNDECIDED" & !is.na(p_best_is_right))) %>%
  mutate(action = case_when(verdict == "UNDECIDED" & p_best_is_right >= AUTO_P ~ "STRONG_CANDIDATE: check and accept",
                            verdict == "UNDECIDED" ~ "CANDIDATE: check",
                            verdict == "DISAGREE" ~ "CHECK: the chosen unit does not cover the old place",
                            TRUE ~ "LOOK: partial overlap"))
write.csv(rv, file.path(REF_DIR, "crosscheck_review.csv"), row.names = FALSE, na = "", fileEncoding = "UTF-8")

tab <- table(out$verdict)
w <- tapply(out$rows, out$verdict, sum)
summ <- c(sprintf("Cross-check old shapes vs AFROGDB, %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
          sprintf("Decisions and names checked: %d (%d data rows)", nrow(out), sum(out$rows)),
          sprintf("  %-26s %5d names, %7d rows", names(tab), as.integer(tab), as.integer(w[names(tab)])), "", rep)
writeLines(summ, file.path(REF_DIR, "crosscheck_model.txt"))
cat(paste(summ, collapse = "\n"), "\n")
msg("written: crosscheck_old_vs_new.csv, crosscheck_review.csv, crosscheck_model.txt")
