#!/usr/bin/env python3
"""
gps_district_check.py - checks the district names of one country against the GPS points of the raw IM households.

    python py/gps_district_check.py --form 4445 --country CHAD

For every household record of the raw ONA form, the GPS point is placed in the AFROGDB district polygon that contains it.
That is compared with the district the name-based pipeline gave the same raw (Region, District) pair (geo_code of the cleaned IM
repository). Where the names are not matched (no geo_code), the GPS shows which district the records fall in: a proposal with its share.

Privacy: the households' points are read and used on this computer only. The files written hold COUNTS and shares per district name and per settlement name,
never a household, a coordinate of a household, or a finding. The settlement file holds the median position of a named settlement (not a household).

Needs: geopandas, shapely, pandas, pyarrow. Reads (never changes):
  ../AFRO-Independant-Monitoring-data-workflow/data/raw/<form>.parquet      raw household records (GPS)
  ../AFRO-Independant-Monitoring-data-workflow/data/final/Regional_IM_repository_cleaned.parquet   cleaned district rows with geo_code and the raw names
  data/clean/africa_districts_simplified.gpkg                               AFROGDB districts (written from the .rds layer)
Writes:
  reference/gps_check/<country>_gps_vs_names.csv      one line per raw district name: verdict, shares, proposal      (safe to share, can be committed)
  data/gps_check_local/<country>_settlements.csv      one line per named settlement: n, median position, spread   (local only, not in git)
  reference/gps_check/<country>_gps_summary.txt       the numbers in words
"""
import argparse, re, sys, unicodedata
from pathlib import Path
import numpy as np, pandas as pd, geopandas as gpd, pyarrow.parquet as pq

HERE = Path(__file__).resolve().parent.parent
IM = HERE.parent / "AFRO-Independant-Monitoring-data-workflow"


def norm(s):
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode().upper()
    return re.sub(r"\s+", " ", re.sub(r"[^A-Z0-9]+", " ", s)).strip()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--form", required=True)
    ap.add_argument("--country", required=True, help="country name as in AFROGDB, e.g. CHAD")
    ap.add_argument("--iso", default="", help="ISO3 code (prefix of the geo_code), e.g. TCD: used to pick the districts when given")
    ap.add_argument("--boundary-km", type=float, default=1.0, help="points closer to a district boundary than this are 'near the boundary'")
    a = ap.parse_args()
    country = a.country.upper()
    out_ref = HERE / "reference" / "gps_check"; out_loc = HERE / "data" / "gps_check_local"
    out_ref.mkdir(parents=True, exist_ok=True); out_loc.mkdir(parents=True, exist_ok=True)

    # 1 raw records: names, GPS, settlement
    raw_file = IM / "data" / "raw" / f"{a.form}.parquet"
    names = pq.ParquetFile(raw_file).schema_arrow.names
    alias = {"Region": ("Region", "states", "state"), "District": ("District", "lgas", "lga")}   # Nigeria names its columns states and lgas
    pick = {k: next((c for c in v if c in names), None) for k, v in alias.items()}
    for need in ("Region", "District"):
        if not pick[need]: sys.exit(f"SKIPPED: no column for {need} in {raw_file.name}")
    for need in ("_geolocation[1]", "_geolocation[2]"):
        if need not in names: sys.exit(f"SKIPPED: column {need} not found in {raw_file.name}")
    want = sorted({c for c in (pick["Region"], pick["District"], "Settlement_name", "Settlement_Type", "_geolocation[1]", "_geolocation[2]") if c in names})
    d = pq.read_table(raw_file, columns=want).to_pandas()
    d = d.rename(columns={pick["Region"]: "Region", pick["District"]: "District"})
    d["lat"] = pd.to_numeric(d["_geolocation[1]"], errors="coerce"); d["lon"] = pd.to_numeric(d["_geolocation[2]"], errors="coerce")
    n_all = len(d)
    d["gps_ok"] = d["lat"].between(-36, 38) & d["lon"].between(-20, 56) & ~((d["lat"] == 0) & (d["lon"] == 0))
    d["prov_n"] = d["Region"].map(norm); d["dist_n"] = d["District"].map(norm)

    # 2 name-based geo_code from the cleaned repository
    cl = pq.read_table(IM / "data" / "final" / "Regional_IM_repository_cleaned.parquet", columns=["country_raw", "province_raw", "district_raw", "geo_code", "country"]).to_pandas()
    cl = cl[cl["country"].map(norm) == norm(country)]
    cl["prov_n"] = cl["province_raw"].map(norm); cl["dist_n"] = cl["district_raw"].map(norm)
    name_code = cl.dropna(subset=["geo_code"]).groupby(["prov_n", "dist_n"])["geo_code"].agg(lambda s: s.value_counts().index[0]).rename("name_code")
    d = d.join(name_code, on=["prov_n", "dist_n"])

    # 3 point in polygon
    dist = gpd.read_file(HERE / "data" / "clean" / "africa_districts_simplified.gpkg")
    dist = (dist[dist["GEO_CODE"].astype(str).str.startswith(a.iso.upper() + "-", na=False)] if a.iso else dist[dist["ADM0_NAME"].map(norm) == norm(country)]).reset_index(drop=True)
    if dist.empty: sys.exit(f"no districts for {country} in the layer")
    pts = gpd.GeoDataFrame(d.loc[d["gps_ok"], ["lat", "lon"]].copy(), geometry=gpd.points_from_xy(d.loc[d["gps_ok"], "lon"], d.loc[d["gps_ok"], "lat"]), crs=4326)
    j = gpd.sjoin(pts, dist[["GEO_CODE", "ADM1_NAME", "ADM2_NAME", "geometry"]], how="left", predicate="within")
    j = j[~j.index.duplicated(keep="first")]
    d.loc[j.index, "gps_code"] = j["GEO_CODE"]; d.loc[j.index, "gps_district"] = j["ADM2_NAME"]; d.loc[j.index, "gps_province"] = j["ADM1_NAME"]
    # distance to the nearest boundary (metres, UTM of the country centre)
    cen = dist.union_all().centroid; zone = int((cen.x + 180) // 6) + 1
    crs_m = f"EPSG:{(32600 if cen.y >= 0 else 32700) + zone}"
    inside = d["gps_code"].notna()
    bnd = dist.to_crs(crs_m).boundary
    pm = gpd.GeoSeries(gpd.points_from_xy(d.loc[inside, "lon"], d.loc[inside, "lat"]), index=d.index[inside], crs=4326).to_crs(crs_m)
    from shapely import STRtree   # indexed nearest-boundary lookup (same distance as against the merged boundary, much faster)
    tree = STRtree(np.asarray(bnd.values))
    _, dist_m = tree.query_nearest(np.asarray(pm.values), return_distance=True, all_matches=False)
    d.loc[inside, "bnd_m"] = dist_m
    d["near_bnd"] = d["bnd_m"] < a.boundary_km * 1000

    # 4 one line per raw district name
    rows = []
    for (pv, dv), g in d.groupby(["prov_n", "dist_n"], dropna=False):
        n = len(g); gg = g[g["gps_ok"]]; ins = gg[gg["gps_code"].notna()]
        nc = g["name_code"].dropna().iloc[0] if g["name_code"].notna().any() else None
        top = ins["gps_code"].value_counts()
        tcode = top.index[0] if len(top) else None; tshare = top.iloc[0] / len(ins) if len(ins) else np.nan
        agree = (ins["gps_code"] == nc).mean() if (nc and len(ins)) else np.nan
        far = ins[~ins["near_bnd"].fillna(False)]; agree_far = (far["gps_code"] == nc).mean() if (nc and len(far)) else np.nan
        if nc is None: v = "NAME_UNMATCHED_GPS_PROPOSAL" if (tcode and tshare >= 0.8 and len(ins) >= 5) else "NAME_UNMATCHED_NO_CLEAR_GPS"
        elif not len(ins): v = "NO_USABLE_GPS"
        elif tcode == nc:   # the name-based district is where most of the households are
            v = "CONFIRMED" if agree >= 0.9 else ("CONFIRMED_MAJORITY" if agree >= 0.5 else "CONFIRMED_WEAK")
        else:               # most households are in another district
            v = "MIXED" if agree >= 0.25 else "CONTRADICTED"
        t2 = top.index[1] if len(top) > 1 else None
        rows.append(dict(region_raw=g["Region"].iloc[0], district_raw=g["District"].iloc[0], records=n, with_gps=len(gg), inside_country=len(ins),
                         name_code=nc, share_gps_in_name_district=round(agree, 3) if agree == agree else None, share_excl_boundary_zone=round(agree_far, 3) if agree_far == agree_far else None,
                         gps_top_code=tcode, gps_top_district=ins["gps_district"][ins["gps_code"] == tcode].iloc[0] if tcode else None, gps_top_share=round(tshare, 3) if tshare == tshare else None,
                         gps_second_code=t2, near_boundary_share=round(float(ins["near_bnd"].mean()), 3) if len(ins) else None, verdict=v))
    res = pd.DataFrame(rows).sort_values(["verdict", "records"], ascending=[True, False])
    res.to_csv(out_ref / f"{country.lower()}_gps_vs_names.csv", index=False, encoding="utf-8")

    # 5 settlements (local only)
    if "Settlement_name" in d.columns:
        s = d[d["gps_ok"] & d["gps_code"].notna() & d["Settlement_name"].notna()].copy()
        s["set_n"] = s["Settlement_name"].map(norm)
        gp = gpd.GeoSeries(gpd.points_from_xy(s["lon"], s["lat"]), index=s.index, crs=4326).to_crs(crs_m)
        s["x"] = gp.x; s["y"] = gp.y
        agg = s.groupby(["gps_code", "set_n"]).agg(records=("x", "size"), x=("x", "median"), y=("y", "median"), sx=("x", "std"), sy=("y", "std"),
                                                    settlement=("Settlement_name", lambda v: v.value_counts().index[0]), type=("Settlement_Type", lambda v: v.value_counts().index[0] if v.notna().any() else None)).reset_index()
        agg["spread_km"] = (np.sqrt(agg["sx"].fillna(0) ** 2 + agg["sy"].fillna(0) ** 2) / 1000).round(2)
        ll = gpd.GeoSeries(gpd.points_from_xy(agg["x"], agg["y"]), crs=crs_m).to_crs(4326)
        agg["lat_median"] = ll.y.round(5); agg["lon_median"] = ll.x.round(5)
        agg.drop(columns=["x", "y", "sx", "sy"]).to_csv(out_loc / f"{country.lower()}_settlements.csv", index=False, encoding="utf-8")
        # a settlement name used in several districts
        multi = s.groupby("set_n")["gps_code"].nunique()
        n_multi = int((multi > 1).sum())
    else:
        agg = pd.DataFrame(); n_multi = 0

    # 6 summary
    inside_n = int(inside.sum()); out_n = int((d["gps_ok"] & ~inside).sum())
    cnt = res.groupby("verdict")["records"].agg(["count", "sum"])
    L = [f"GPS check of district names, {country}, form {a.form}",
         f"Records: {n_all:,}; plausible GPS: {int(d['gps_ok'].sum()):,} ({100*d['gps_ok'].mean():.1f}%); inside a {country} district: {inside_n:,}; plausible GPS but outside {country}: {out_n:,}",
         f"Exact repeated points (same coordinates): {int(d.loc[d['gps_ok']].duplicated(['lat','lon']).sum()):,}",
         f"Points within {a.boundary_km:g} km of a district boundary (shape tolerance): {100*float(d['near_bnd'].mean()):.1f}% of those inside", "",
         "Raw district names by verdict:"] + [f"  {k:30s} {int(r['count']):5d} names, {int(r['sum']):9,d} records" for k, r in cnt.iterrows()]
    L += ["", "CONFIRMED = at least 90% of the households inside the name's district; CONFIRMED_MAJORITY 50-90%; CONFIRMED_WEAK under 50% but still the main district;",
          "MIXED = most households in another district but 25% or more in the name's; CONTRADICTED = under 25% in the name's district."]
    if len(agg): L += ["", f"Named settlements: {len(agg):,}; used in more than one district: {n_multi}; spread above 10 km (same name far apart or bad GPS): {int((agg['spread_km'] > 10).sum())}"]
    (out_ref / f"{country.lower()}_gps_summary.txt").write_text("\n".join(L) + "\n", encoding="utf-8")
    print("\n".join(L))


if __name__ == "__main__":
    main()
