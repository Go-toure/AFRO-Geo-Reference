# AFRO-Geo-Reference

One place for the WHO AFRO geodatabase (AFROGDB): checking it, cleaning it, and
publishing a single geoname reference that the other projects use.

It exists so that the dashboard (AFRO-Dash), the LQAS pipeline and the IM workflow
all spell and code administrative units the same way, instead of each one fixing
names on its own.

## What it produces

| Output | Where | Used by |
|---|---|---|
| `reference/afrogdb_geoname_master.csv` | versioned in git (small) | LQAS pipeline, IM workflow: match raw names to a `geo_code` |
| `data/clean/africa_countries / provinces / districts.rds` | not in git, rebuilt | full-resolution clean layers |
| `data/clean/africa_*_simplified.rds`, `ambiguous_district_pairs.rds` | not in git, rebuilt | AFRO-Dash maps |
| `data/clean/cleaning_report.txt` | not in git, rebuilt | what was kept, removed and flagged on each run |

## Run it

1. Put the raw export in `data/raw/AFROGDB/` (`AFROGDB.xlsx` and
   `AFROGDB/latest_boundary_adm0|1|2.shp` with their sidecar files).
2. From the repository root, run the steps in order:

```
Rscript R/01_clean_afrogdb.R           # clean layers, geoname master table, report
Rscript R/01b_apply_country_updates.R  # newer country shapefiles from data/updates/ (Benin) replace that country's districts
Rscript R/02_simplify_for_dashboard.R  # lighter copies for the dashboard maps
Rscript R/03_add_legacy_countries.R    # old shapes for the countries the geodatabase lacks
Rscript R/04_check_alignment.R         # how well the data files match the geodatabase
Rscript R/05_build_review_file.R       # builds / updates reference/crosswalk_geonames.csv for review
```

Step 1 takes a few minutes (the district layer is about 530 MB). It accepts explicit
folders: `Rscript R/01_clean_afrogdb.R <gdb_dir> <out_dir>`.

Step 1b: a country can send updated district boundaries before the geodatabase export has them (Benin split
Abomey-Calavi, Cotonou and Porto-Novo). Put the shapefile in `data/updates/<COUNTRY>/`; the script replaces that country's
districts in the clean layers, keeps the existing `geo_code` and spelling of units that already exist, and gives units
that are new a provisional code (e.g. `BEN-ATL-AC1`) until the geodatabase issues its own. Everything it did is in
`reference/country_updates_log.csv`. Run it again after every step 1, and delete the country's folder once the
export contains the new boundaries.

Step 2 needs the `rmapshaper` package and AFRO-Dash's `shapefile_prep_utils.R`
(found in `../AFRO-Dash`, or set `AFRO_DASH_DIR`). Each layer gets a vertex budget
(provinces 250,000, districts 1,000,000) and is simplified in a fresh R process, in groups of
countries for the district layer, with a repair for the few shapes that become invalid.
`AFROGDB_TARGET_PROVINCES` / `AFROGDB_TARGET_DISTRICTS` change the budgets.

Step 3: the geodatabase has provinces and districts for 47 countries. Egypt, Libya, Morocco,
Tunisia, Sudan, Djibouti and Somalia are only country outlines. Their old shapes
(`data/legacy/`) are added until the real ones arrive; a country is added only when the new
layer has no rows for it, so nothing needs changing when the geodatabase is completed.
The added units are listed in `reference/legacy_units.csv`. Step 3 also rebuilds
`ambiguous_district_pairs.rds`.

Steps 4 and 5 are for the LQAS pipeline and the IM workflow (see below).

Packages: sf, dplyr, readxl, jsonlite, rmapshaper (steps 1-3); dplyr (steps 4-5).

## Cleaning rules

1. Keep only current records (no end date, or an end date in the future / 9999-12-31).
2. One row per geo code (`level_id`): the current record with the latest start date, then the highest batch id.
3. Mayotte, Reunion and Saint Helena are removed.
4. Invalid geometries are repaired; empty ones are dropped; CRS is EPSG:4326.
5. `WHO_REGION` comes from the workbook; `WORLD_CONTINENTS` is AFRICA.
6. Names are not altered: the geodatabase is the source of truth for spelling.
7. Units in the shapefile but not in the workbook are kept and flagged `IN_WORKBOOK = FALSE`.

## Switching the dashboard to the new layers

After steps 1-3 and a look at `cleaning_report.txt`, copy into `AFRO-Dash/input/`:
`africa_countries_simplified.rds`, `africa_provinces_simplified.rds`,
`africa_districts_simplified.rds` and `ambiguous_district_pairs.rds`.
Restart the app.

## Standardizing LQAS and IM geonames (outside the dashboard)

`R/04_check_alignment.R` compares a data file with the geodatabase and writes, in `reports/`:

- `alignment_summary.csv`: how much of each file matches, per level;
- `crosswalk_<name>.csv`: every distinct country / province / district of the file with its status
  (exact, normalized, other province, ambiguous, unmatched), the standard names and the `geo_code`;
  for unmatched names a closest-name suggestion, to be reviewed by a person and never applied automatically.

Run it on one file: `Rscript R/04_check_alignment.R <name> <path> <country_col> <province_col> <district_col>`.

### The shared crosswalk (`reference/crosswalk_geonames.csv`)

`R/05_build_review_file.R` collects every name from `reports/crosswalk_*.csv` that did not match
exactly, and writes one line per distinct country / province / district with a proposed `geo_code`.
Each line has a `decision`:

- `auto`: proposed by the script (unique district name in the country, or a very close spelling). Skim and correct.
- `ok`: reviewed by a person. Never overwritten by later runs.
- `no_shape`: the geodatabase has no such unit (for example a health district that is not an admin unit). The data stays unmapped.
- empty: needs a decision. Names that differ from a geodatabase unit only by a number or a direction (ABOMEY-CALAVI 1, N'DJAMENA SUD) are never proposed automatically: they are split districts and need the new boundary (see Step 1b). Fill `geo_code` (and the standard names) from `reference/afrogdb_geoname_master.csv`, then set `decision` to `ok`.

Re-running step 5 after new data keeps all decisions and only adds new lines.

Decisions that are known and should survive any rebuild are written in `reference/crosswalk_manual.csv`
(country, province or blank, district, geo_code, decision, note); step 5 applies them to lines that are empty or
only proposed by the script. Example: Chad's old N'Djamena districts. NORD (1er) and TOUKRA (9e) became one arrondissement each and are
mapped; CENTRE (2e-5e), SUD (6e-7e) and EST (8e, 10e) were split into several, so an old record cannot be assigned to
one unit: they are `no_shape` with the new units in the note.

### Using it in a pipeline (`R/afrogdb_standardize.R`)

```r
source("../AFRO-Geo-Reference/R/afrogdb_standardize.R")   # or set AFRO_GEO_REFERENCE_DIR
ref  <- afrogdb_reference()
data <- afrogdb_standardize(data, "country", "province", "district", ref)
```

The result has the geodatabase spelling in the name columns, the original in `country_raw`,
`province_raw`, `district_raw`, plus `geo_code` and `geo_match`. Unmatched names are logged on every run.
It can be run twice safely.

The dashboard then joins shapes by `geo_code`.

Until the pipelines output the geodatabase names, `app.R` renames the two countries spelled differently
(COTE D'IVOIRE, GUINEA BISSAU) to the data spelling when the shapes load. Remove that bridge when the pipelines are aligned.

## Known points

- `level_id` looks name-derived, so it is less stable than a GUID. Ask the geodatabase owner to add the GUID to the shapefile export.
- Some shapefile units are missing from the workbook (kept and flagged).
- Ethiopia (zone vs woreda) and Botswana ("DISTRICT COUNCIL" suffix) need crosswalk entries.
- There is no adm3 shapefile.
- Keep a before/after diff of the master table at each geodatabase update (git history of `reference/` gives this).
