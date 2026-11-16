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

- Ethiopia: the geodatabase maps Ethiopia at zone level (101 zones, 14 regions), one level above the woreda. Decision (Nov 2026): keep zones
  for LQAS, IM, Scope and Admin for now. Most LQAS / IM / Scope records carry zone names; only about 11-18% match a woreda name, so a woreda layer
  would leave most of those maps empty. Admin data is woreda level and stays unmapped where it has no zone name. When the source data is woreda level,
  switch Ethiopia with step 1b: the 1,141 woredas are in the OCHA "Admin Level 3" shapefile, and `reference/ethiopia_woreda_to_zone.csv` lists every
  woreda with its zone and the zone's geo code. The spelling decisions for zone names are in `reference/crosswalk_manual.csv`.

- `level_id` looks name-derived, so it is less stable than a GUID. Ask the geodatabase owner to add the GUID to the shapefile export.
- Some shapefile units are missing from the workbook (kept and flagged).
- Ethiopia (zone vs woreda) and Botswana ("DISTRICT COUNCIL" suffix) need crosswalk entries.
- There is no adm3 shapefile.
- Keep a before/after diff of the master table at each geodatabase update (git history of `reference/` gives this).

## Ethiopia LQAS: woreda link

The Ethiopia LQAS form (6837) asks the team to pick the woreda (field `facility`) after the zone, then a lot number.
`reference/ethiopia_facility_to_woreda.csv` links each zone + facility pair, as typed in the raw data, to the OCHA woreda name and code.
The link is taken from the GPS points: the woreda where most points of that pair fall. Pairs with fewer than 3 records, or where less than
80% of the points agree, are not linked (`needs_review = yes`); they keep the name typed on the form and get no code.
The LQAS pipeline (`Posit-LQAS-Workflow/R/02_process_lqas.r`) reads this file to give each lot of 60 children its woreda.
To rebuild it: `python tools/build_ethiopia_facility_crosswalk.py <6837.parquet> <OCHA admin 3 shapefile> reference/ethiopia_facility_to_woreda.csv`.

## Step 6: independent cross-check, old shapes against AFROGDB

```
Rscript R/06_crosscheck_shapes.R       # needs sf, dplyr, stringi, data.table
```

The crosswalk is built from names. Step 6 checks every decision a second way, with no names: it overlays the old district shapes (the layer the data was
spelled against, AFRO-Dash at commit 158d753) on the AFROGDB districts and asks whether the unit the crosswalk chose covers the place where the old unit was.
For names still undecided it proposes the best-overlapping unit with a probability learned from the decisions people already confirmed.
It changes nothing: a person decides. Outputs in `reference/`: `crosscheck_old_vs_new.csv` (everything), `crosscheck_review.csv` (what needs a person),
`crosscheck_model.txt` (how well the proposal works). Verdicts: AGREE, PARTLY, DISAGREE, WEAK, NO_OLD_SHAPE, UNDECIDED. A pair of districts that is a twin in the layer
(Burkina Faso) shows up as DISAGREE: the question is which twin is the current one.

## Step 7: GPS check of the district names (IM raw households)

```
python py/gps_district_check.py --form 4445 --country CHAD     # needs geopandas, shapely, pandas, pyarrow
```

The raw IM forms carry a GPS point for every household (about 98.7% of 3.7 million records are plausible). The script places each point in the AFROGDB district
that contains it and compares that with the district the name-based pipeline gave the same raw Region and District. It needs `data/clean/africa_districts_simplified.gpkg`
(the districts layer exported from the `.rds`). The households' points stay on the computer: the files written hold counts and shares only.
`reference/gps_check/<country>_gps_vs_names.csv`: verdict per raw district name (CONFIRMED, CONFIRMED_MAJORITY, CONFIRMED_WEAK, MIXED, CONTRADICTED) and, for names not matched,
the district where the households are (NAME_UNMATCHED_GPS_PROPOSAL). `data/gps_check_local/<country>_settlements.csv`: named settlements with a median position and spread (local only).
Together with step 6 (old shapes), names, old shapes and GPS are three independent checks of the same match.

## Step 8 - Anomaly check on the raw IM records (`py/anomaly_check.py`)

Finds raw records that look copied, invented or impossible, for all 35 IM forms (3.7 million records).

```bash
python py/anomaly_check.py --only TCD,NGA     # chosen countries (DR Congo needs about 80 s on its own)
```

* Exact rules: same GPS point on different days or by different monitors, a device that "travels" over 150 km/h, forms filled in under 60 s, impossible dates, negative or extreme counts, and one monitor repeating the same set of counts far more than chance explains.
* A model (Isolation Forest) marks the 1% most unusual records per country. It is tested by planting known errors: it finds 85-100% of counts that are 10 times too high, but almost none of the 15-second forms, which is why those are caught by an exact rule.
* A flag means "look at this", not "this is wrong". Shareable output is in `reference/anomaly_check/` (by country and district, no names). Monitor names and device ids stay in `data/anomaly_local/` (not in git).

## Extended matching layers (added 16 Nov 2026)

`py/build_extended_crosswalk.py` writes `reference/crosswalk_extended.csv`: the last answers for names that the standard matching leaves without a unit (manual decisions, Ethiopia woreda to zone, relaxed, old-shape overlay with probability 0.90 or more, name similarity). The last two are marked probable. Use `--names` with the unmatched-name lists of the SCOPE and admin builds so that new names are included. The R functions use it only with `extended = TRUE`.
