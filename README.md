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
2. From the repository root:

```
Rscript R/01_clean_afrogdb.R
```

Or with explicit folders: `Rscript R/01_clean_afrogdb.R <gdb_dir> <out_dir>`.
Packages: sf, dplyr, readxl, jsonlite, and rmapshaper for the simplified copies.
The simplified copies are built with AFRO-Dash's own `shapefile_prep_utils.R`
(found in `../AFRO-Dash`, or set `AFRO_DASH_DIR`); set `AFROGDB_NO_SIMPLIFY=1` to skip them.

The district layer is large (about 530 MB, 33 million vertices). Allow some minutes and memory.

## Cleaning rules

1. Keep only current records (no end date, or an end date in the future / 9999-12-31).
2. One row per geo code (`level_id`): the current record with the latest start date, then the highest batch id.
3. Mayotte, Reunion and Saint Helena are removed.
4. Invalid geometries are repaired; empty ones are dropped; CRS is EPSG:4326.
5. `WHO_REGION` comes from the workbook; `WORLD_CONTINENTS` is AFRICA.
6. Names are not altered: the geodatabase is the source of truth for spelling.
7. Units in the shapefile but not in the workbook are kept and flagged `IN_WORKBOOK = FALSE`.

## Switching the dashboard to the new layers

After checking `cleaning_report.txt`, copy into `AFRO-Dash/input/`:
`africa_countries_simplified.rds`, `africa_provinces_simplified.rds`,
`africa_districts_simplified.rds` and `ambiguous_district_pairs.rds`.
Restart the app.

## Standardizing LQAS and IM geonames (outside the dashboard)

Done in each pipeline, against `reference/afrogdb_geoname_master.csv`:

1. exact match, then match after normalization (case, accents, apostrophes, hyphens);
2. a maintained crosswalk CSV per source (raw country / province / district to `geo_code`), reviewed by a person;
3. fuzzy matching only suggests crosswalk entries for review;
4. every run lists the names that did not match;
5. the pipeline output keeps the raw names and adds the standard names and `geo_code`.

The dashboard then joins shapes by `geo_code`.

## Known points

- `level_id` looks name-derived, so it is less stable than a GUID. Ask the geodatabase owner to add the GUID to the shapefile export.
- Some shapefile units are missing from the workbook (kept and flagged).
- Ethiopia (zone vs woreda) and Botswana ("DISTRICT COUNCIL" suffix) need crosswalk entries.
- There is no adm3 shapefile.
- Keep a before/after diff of the master table at each geodatabase update (git history of `reference/` gives this).
