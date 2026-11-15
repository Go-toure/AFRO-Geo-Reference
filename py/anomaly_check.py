#!/usr/bin/env python3
"""
anomaly_check.py - finds suspicious raw IM records (copied or invented data, GPS that cannot be real, wrong dates, impossible counts).

    python py/anomaly_check.py --only TCD,NGA        some countries (ISO codes of config/country_forms.csv)
    python py/anomaly_check.py                       all countries

Two layers, kept apart so each flag can be explained:
  1. RULES (exact, explainable). Hard flags: GPS_EXACT_REPEAT (same point to 1 m on 3+ records on different days or by different monitors),
     TELEPORT (same device 5+ km from its previous point at over 150 km/h), TOO_FAST (form filled in under 60 s), DATE_IMPLAUSIBLE (monitored
     before 2018 or after submission), COUNT_NEGATIVE, COUNT_EXTREME (far beyond the country's own spread), DUP_COUNTS (one monitor holds a set of
     counts 4+ times, far more than its share of the country's records explains). Soft flags: GPS_COARSE (2 decimals or fewer), GPS_POOR_ACCURACY (over 100 m), TOO_LONG (over 24 h),
     LATE_SUBMIT (over 60 days), MODEL_OUTLIER.
  2. MODEL: an Isolation Forest on durations, counts, ratios, GPS accuracy and submission lag marks the 1% most unusual records of the
     country. It is checked by planting known corruptions in a copy of the data and measuring how many it finds (recall), and the result
     is written next to the findings. A flag says "look at this", never "this is wrong".
Output (shareable, no names, no points): reference/anomaly_check/<iso>_by_district.csv, all_countries_summary.csv, all_countries_by_district.csv.
Local only (monitor names, device ids): data/anomaly_local/<iso>_records.csv and <iso>_monitors.csv.
"""
import argparse, csv, re, time, unicodedata
from pathlib import Path
import numpy as np, pandas as pd, pyarrow.parquet as pq
from scipy.stats import binom, chi2

HERE = Path(__file__).resolve().parent.parent
IM = HERE.parent / "AFRO-Independant-Monitoring-data-workflow"
OUT = HERE / "reference" / "anomaly_check"; LOC = HERE / "data" / "anomaly_local"
CAND = dict(id=("_uuid", "_id"), start=("starttime", "start"), end=("endtime", "end"), dur=("_duration",), device=("deviceid", "device_id"),
            monitor=("Name_of_Monitor", "Monitor_Name"), sub=("_submission_time",), dm=("date_monitored", "today"), lat=("_geolocation[1]",),
            lon=("_geolocation[2]",), prec=("_GPS_hh_precision",), region=("Region", "states", "state"), district=("District", "lgas", "lga"),
            present=("Total_U5_Present",), absent=("Total_Absent",), refusal=("Total_refusal",), missed=("sum_missed_children",), fm=("TotalFM",))
HARD = ["GPS_EXACT_REPEAT", "TELEPORT", "TOO_FAST", "DATE_IMPLAUSIBLE", "COUNT_NEGATIVE", "COUNT_EXTREME", "DUP_COUNTS"]
SOFT = ["GPS_COARSE", "GPS_POOR_ACCURACY", "TOO_LONG", "LATE_SUBMIT", "MODEL_OUTLIER"]


def norm(s):
    if s is None or (isinstance(s, float) and np.isnan(s)): return ""
    s = unicodedata.normalize("NFKD", str(s)).encode("ascii", "ignore").decode().upper()
    return re.sub(r"[^A-Z0-9]+", " ", s).strip()


def haversine(la1, lo1, la2, lo2):
    p = np.pi / 180; a = np.sin((la2 - la1) * p / 2) ** 2 + np.cos(la1 * p) * np.cos(la2 * p) * np.sin((lo2 - lo1) * p / 2) ** 2
    return 12742 * np.arcsin(np.sqrt(a))


def load(form):
    f = IM / "data" / "raw" / f"{form}.parquet"
    names = pq.ParquetFile(f).schema_arrow.names
    pick = {k: next((c for c in v if c in names), None) for k, v in CAND.items()}
    raw = pq.read_table(f, columns=sorted({c for c in pick.values() if c})).to_pandas()
    d = pd.DataFrame({k: raw[c].astype("object") for k, c in pick.items() if c})
    for k in CAND:
        if k not in d: d[k] = np.nan
    return d


def features(d):
    X = pd.DataFrame(index=d.index)
    X["dur"] = np.log1p(d["dur_s"].clip(0, 86400))
    for c in ("present", "absent", "refusal", "missed"): X[c] = np.log1p(d[c].clip(0, None))
    p = d["present"].where(d["present"] > 0)
    for c in ("missed", "absent", "refusal"): X[c + "_ratio"] = (d[c] / p).clip(0, 5)
    X["lag"] = d["lag_days"].clip(-30, 60); X["prec"] = np.log1p(d["prec"].clip(0, 5000))
    X = X.loc[:, X.notna().sum() > 0.2 * len(X)]
    return X.fillna(X.median())


def run(iso, name, form):
    from sklearn.ensemble import IsolationForest
    d = load(form); n = len(d); t0 = time.time()
    for c in ("present", "absent", "refusal", "missed", "fm", "prec", "lat", "lon", "dur"): d[c] = pd.to_numeric(d[c], errors="coerce")
    d["t"] = pd.to_datetime(d["start"], utc=True, errors="coerce", format="ISO8601")
    d["t_end"] = pd.to_datetime(d["end"], utc=True, errors="coerce", format="ISO8601")
    d["t_sub"] = pd.to_datetime(d["sub"], utc=True, errors="coerce", format="ISO8601")
    d["dur_s"] = (d["t_end"] - d["t"]).dt.total_seconds().where(d["t"].notna() & d["t_end"].notna(), d["dur"])
    d["dm_d"] = pd.to_datetime(d["dm"], errors="coerce").dt.normalize()
    sub_d = d["t_sub"].dt.tz_localize(None).dt.normalize()
    d["lag_days"] = (sub_d - d["dm_d"]).dt.days
    d["mon_n"] = d["monitor"].map(norm); d["dev"] = d["device"].astype("object").where(d["device"].notna(), "")
    F = pd.DataFrame(False, index=d.index, columns=HARD + SOFT)

    # GPS
    ok = d["lat"].between(-36, 38) & d["lon"].between(-20, 56) & ~((d["lat"] == 0) & (d["lon"] == 0))
    dec = lambda s: s.astype(str).str.extract(r"\.(\d+)")[0].str.len().fillna(0)
    fine = ok & (dec(d["lat"]) >= 4) & (dec(d["lon"]) >= 4)
    F.loc[ok & ~fine, "GPS_COARSE"] = True
    F.loc[d["prec"] > 100, "GPS_POOR_ACCURACY"] = True
    key = pd.Series(np.where(fine, d["lat"].round(5).astype(str) + "|" + d["lon"].round(5).astype(str), None), index=d.index)
    g = key[fine]; cnt = g.map(g.value_counts())
    day = d.loc[fine, "dm_d"].astype(str); mon = d.loc[fine, "mon_n"]
    nday = day.groupby(g).transform("nunique"); nmon = mon.groupby(g).transform("nunique")
    F.loc[fine, "GPS_EXACT_REPEAT"] = ((cnt >= 3) & ((nday >= 2) | (nmon >= 2))).values
    tp = d.loc[ok & d["t"].notna() & (d["dev"] != ""), ["dev", "t", "lat", "lon"]].sort_values(["dev", "t"])
    prev = tp.groupby("dev")[["t", "lat", "lon"]].shift(1)
    km = haversine(prev["lat"], prev["lon"], tp["lat"], tp["lon"]); hrs = (tp["t"] - prev["t"]).dt.total_seconds() / 3600
    F.loc[tp.index[((km > 5) & (hrs > 1 / 60) & (km / hrs > 150)).values], "TELEPORT"] = True

    # time and dates
    F.loc[d["dur_s"] < 60, "TOO_FAST"] = True; F.loc[d["dur_s"] > 86400, "TOO_LONG"] = True
    F.loc[(d["dm_d"] < "2018-01-01") | (d["lag_days"] < -1), "DATE_IMPLAUSIBLE"] = True
    F.loc[d["lag_days"] > 60, "LATE_SUBMIT"] = True

    # counts
    cols = [c for c in ("present", "absent", "refusal", "missed") if d[c].notna().any()]
    F.loc[(d[cols] < 0).any(axis=1), "COUNT_NEGATIVE"] = True
    if d["present"].notna().any():
        q1, q3 = d["present"].quantile([.25, .75]); F.loc[d["present"] > max(150, q3 + 6 * (q3 - q1)), "COUNT_EXTREME"] = True
    vec = d[["present", "absent", "refusal", "missed", "fm"]]
    has = vec.notna().all(axis=1) & (d["present"] > 0) & (d["mon_n"] != "")
    if has.sum() >= 50:   # needs monitors and counts
        vk = vec[has].astype(int).astype(str).agg("-".join, axis=1)
        mm = d.loc[has, "mon_n"]; N = len(vk)
        kk = vk.groupby([vk, mm]).transform("size"); Nv = vk.map(vk.value_counts()); pm = mm.map(mm.value_counts()) / N
        # a set of counts is only suspicious when one monitor holds far more of it than that monitor's share of the country's records explains
        F.loc[has, "DUP_COUNTS"] = ((kk >= 4) & (binom.sf(kk - 1, Nv, pm) < 1e-6)).values

    # model + check by planting known corruptions
    X = features(d); sc = np.zeros(n); recall = np.nan; recall_fast = np.nan
    if len(X) >= 200 and X.shape[1] >= 3:
        m = IsolationForest(n_estimators=200, max_samples=256, random_state=7, n_jobs=-1).fit(X)
        sc = -m.score_samples(X); thr = np.quantile(sc, 0.99); F["MODEL_OUTLIER"] = sc > thr
        S = X.sample(min(5000, len(X)), random_state=7)
        cnt_cols = [c for c in ("present", "absent", "refusal", "missed") if c in S]
        A = S.copy(); A[cnt_cols] = A[cnt_cols] + np.log(10)                      # every count 10 times too high
        Bq = S.copy()
        if "dur" in Bq: Bq["dur"] = np.log1p(15)                                   # form filled in 15 seconds
        recall = float((-m.score_samples(A) > thr).mean()); recall_fast = float((-m.score_samples(Bq) > thr).mean()) if "dur" in Bq else np.nan
    F["any_hard"] = F[HARD].any(axis=1); F["any_flag"] = F[HARD + SOFT].any(axis=1)

    # outputs
    OUT.mkdir(parents=True, exist_ok=True); LOC.mkdir(parents=True, exist_ok=True)
    R = pd.concat([d[["region", "district"]], F], axis=1).fillna({"region": "", "district": ""})
    by = R.groupby(["region", "district"]).agg(records=("any_flag", "size"), **{c: (c, "sum") for c in HARD + SOFT + ["any_hard", "any_flag"]}).reset_index()
    by.insert(0, "country", name); by.insert(0, "iso", iso); by["share_hard"] = (by["any_hard"] / by["records"]).round(3)
    by.sort_values("share_hard", ascending=False).to_csv(OUT / f"{iso.lower()}_by_district.csv", index=False, encoding="utf-8")
    loc = pd.concat([d[["id", "region", "district", "monitor", "dev", "dm_d", "dur_s", "lat", "lon"]], F, pd.Series(sc, name="score", index=d.index)], axis=1)
    loc[loc["any_flag"]].to_csv(LOC / f"{iso.lower()}_records.csv", index=False, encoding="utf-8")
    mt = pd.concat([d["mon_n"], F[["any_hard", "any_flag"] + HARD]], axis=1); mt = mt[mt["mon_n"] != ""]
    ms = mt.groupby("mon_n").agg(records=("any_flag", "size"), share_hard=("any_hard", "mean"), share_any=("any_flag", "mean")).reset_index()
    last = (d["present"].dropna().astype(int) % 10).groupby(d.loc[d["present"].notna(), "mon_n"])
    def dp(s):   # terminal-digit preference: chi-square of the last digit against a uniform spread
        if len(s) < 30: return np.nan
        o = np.bincount(s, minlength=10); e = len(s) / 10; return float(chi2.sf(((o - e) ** 2 / e).sum(), 9))
    ms["digit_pref_p"] = ms["mon_n"].map(last.apply(lambda s: dp(s.values))); ms.sort_values("share_hard", ascending=False).to_csv(LOC / f"{iso.lower()}_monitors.csv", index=False, encoding="utf-8")
    one = dict(iso=iso, country=name, records=n, any_hard_pct=round(100 * F["any_hard"].mean(), 2), any_flag_pct=round(100 * F["any_flag"].mean(), 2),
               recall_counts_x10=round(recall, 3) if recall == recall else None, recall_15s_form=round(recall_fast, 3) if recall_fast == recall_fast else None, seconds=round(time.time() - t0))
    one.update({c + "_pct": round(100 * F[c].mean(), 2) for c in HARD + SOFT})
    return one


def merge(summary_rows):
    p = OUT / "all_countries_summary.csv"
    old = pd.read_csv(p).drop(columns=["model_recall_planted"], errors="ignore") if p.exists() else pd.DataFrame()
    new = pd.DataFrame(summary_rows)
    allr = pd.concat([old[~old["iso"].isin(new["iso"])] if len(old) else old, new]).sort_values("any_hard_pct", ascending=False); allr.to_csv(p, index=False, encoding="utf-8")
    parts = [pd.read_csv(f) for f in sorted(OUT.glob("??*_by_district.csv")) if not f.name.startswith("all_")]
    pd.concat(parts).sort_values("share_hard", ascending=False).to_csv(OUT / "all_countries_by_district.csv", index=False, encoding="utf-8")


def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--only", default=""); a = ap.parse_args()
    only = {x.strip().upper() for x in a.only.split(",") if x.strip()}
    rows = []
    for f in csv.DictReader((IM / "config" / "country_forms.csv").open(encoding="utf-8")):
        if only and f["country_key"] not in only: continue
        try:
            r = run(f["country_key"], f["country_name"], f["form_id"]); rows.append(r)
            print(f"{r['iso']} ok in {r['seconds']}s | {r['records']:,} records | hard flags {r['any_hard_pct']}% | recall counts x10 {r['recall_counts_x10']}, 15 s form {r['recall_15s_form']}", flush=True)
        except Exception as e:
            print(f"{f['country_key']} FAILED | {type(e).__name__}: {e}", flush=True)
    if rows: merge(rows)


if __name__ == "__main__":
    main()
