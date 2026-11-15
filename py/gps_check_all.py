#!/usr/bin/env python3
"""
gps_check_all.py - runs gps_district_check.py for every IM form of config/country_forms.csv, then writes one ranked list.

    python py/gps_check_all.py                 all countries
    python py/gps_check_all.py --only TCD,NGA  some countries
Each country is a separate process with a time limit, so one large form cannot stop the others. Progress goes to reference/gps_check/run_log.txt.
Output: reference/gps_check/all_countries_gps_vs_names.csv (every raw district name with its verdict) and all_countries_review.csv
(CONTRADICTED, MIXED and the unmatched names with a GPS proposal, largest first). Counts and shares only: no household point is written.
"""
import argparse, csv, subprocess, sys, time
from pathlib import Path
import pandas as pd

HERE = Path(__file__).resolve().parent.parent
IM = HERE.parent / "AFRO-Independant-Monitoring-data-workflow"
OUT = HERE / "reference" / "gps_check"


def main():
    ap = argparse.ArgumentParser(); ap.add_argument("--only", default=""); ap.add_argument("--timeout-min", type=float, default=25)
    a = ap.parse_args()
    only = {x.strip().upper() for x in a.only.split(",") if x.strip()}
    OUT.mkdir(parents=True, exist_ok=True)
    forms = list(csv.DictReader((IM / "config" / "country_forms.csv").open(encoding="utf-8")))
    log = (OUT / "run_log.txt").open("a", encoding="utf-8")
    def say(m):
        line = time.strftime("%H:%M:%S ") + m; print(line, flush=True); log.write(line + "\n"); log.flush()
    frames = []
    for f in forms:
        iso, name, fid = f["country_key"], f["country_name"], f["form_id"]
        if only and iso not in only: continue
        t0 = time.time()
        try:
            r = subprocess.run([sys.executable, str(HERE / "py" / "gps_district_check.py"), "--form", fid, "--country", name, "--iso", iso],
                               capture_output=True, text=True, timeout=a.timeout_min * 60)
            msg = (r.stdout + r.stderr).strip().splitlines()
            ok = r.returncode == 0
            say(f"{iso} {name} form {fid}: {'ok' if ok else 'FAILED'} in {time.time()-t0:.0f}s" + ("" if ok else " | " + (msg[-1] if msg else "")))
        except subprocess.TimeoutExpired:
            say(f"{iso} {name} form {fid}: TIMEOUT after {a.timeout_min:g} min"); continue
        p = OUT / f"{name.lower()}_gps_vs_names.csv"
        if ok and p.exists():
            d = pd.read_csv(p); d.insert(0, "country", name); d.insert(1, "iso", iso); frames.append(d)
    if frames:
        allc = pd.concat(frames, ignore_index=True)
        # a partial run (--only) keeps the countries already done
        prev = OUT / "all_countries_gps_vs_names.csv"
        if only and prev.exists():
            old = pd.read_csv(prev); old = old[~old["iso"].isin(allc["iso"].unique())]; allc = pd.concat([old, allc], ignore_index=True)
        allc.to_csv(prev, index=False, encoding="utf-8")
        rv = allc[allc["verdict"].isin(["CONTRADICTED", "MIXED", "NAME_UNMATCHED_GPS_PROPOSAL"])].sort_values("records", ascending=False)
        rv.to_csv(OUT / "all_countries_review.csv", index=False, encoding="utf-8")
        say(f"written all_countries_gps_vs_names.csv ({len(allc)} names) and all_countries_review.csv ({len(rv)} names)")
        print(allc.groupby("verdict")["records"].agg(["count", "sum"]).to_string())


if __name__ == "__main__":
    main()
