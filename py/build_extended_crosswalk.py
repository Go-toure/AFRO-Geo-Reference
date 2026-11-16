#!/usr/bin/env python3
"""
build_extended_crosswalk.py - the last layers of name matching, written to reference/crosswalk_extended.csv.

    python py/build_extended_crosswalk.py                       names of the crosswalk and the manual file
    python py/build_extended_crosswalk.py --names A.csv B.csv   also the names a pipeline could not place
                                                                (columns Country, Province, District in any case)

The standard matching (exact, same after ignoring accents and punctuation, the one district of that name in another
province, the reviewed crosswalk) stays as it is. This file answers only for names that it leaves without a unit, in this
order, first answer wins:
  manual           reference/crosswalk_manual.csv: decisions taken by a person (ok / auto / no_shape)
  woreda to zone   Ethiopia: the woreda (also written without SUB CITY, SC, TOWN ...) or "ZONE n" is in
                   ethiopia_woreda_to_zone.csv / the zone list, so it takes the code of its zone
  relaxed          the same name once generic words (DISTRICT, DC, COUNCIL, CITY, TOWN, WOREDA ...) are ignored
  old shape        crosscheck_old_vs_new.csv: the old AFRO-Dash shape of this name falls mostly inside one AFROGDB unit,
                   and the measured probability is at least 0.90 (97% right on the confirmed decisions)
  name similarity  the closest AFROGDB name of the same country (the same province first), at most 0.30 apart and clearly
                   closer than the next one (about 93% right on the confirmed decisions; Ethiopia, against woreda and
                   zone names, 18 of 21 = 86%, names of 6 letters or more)
The two last layers are 'probable'; the others are 'certain'. Nothing here is applied unless a pipeline asks for it
(R: afrogdb_standardize(..., extended = TRUE); admin build: on by default, --no-afrogdb-extended).
Output: crosswalk_extended.csv (country, province, district, geo_code, method, confidence, evidence) and
crosswalk_extended_report.txt (counts). Names and counts only.
"""
import argparse, re, sys, unicodedata
from pathlib import Path
import numpy as np
import pandas as pd

REF = Path(__file__).resolve().parent.parent / "reference"
GENERIC = r"\b(DISTRICTS?|DIST|DC|COUNCIL|MUNICIPALITY|MUNICIPAL|MC|TC|CITY|TOWN|WOREDA|WOHO|WORHO)\b"
P_OLD, D_SIM, MARGIN = 0.90, 0.30, 0.10


def asc(s):
    return "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))


def nk(x):
    s = "" if x is None or (isinstance(x, float) and x != x) else str(x)
    s = re.sub(r"^RM[ -]+", "", asc(s.strip()).upper())
    return re.sub(r"[^A-Z0-9]", "", s)


def rl(x):
    s = "" if x is None or (isinstance(x, float) and x != x) else str(x)
    s = re.sub(r"^RM[ -]+", "", asc(s.strip()).upper())
    y = re.sub(r"[^A-Z0-9]", "", re.sub(GENERIC, " ", s))
    return y if len(y) >= 3 else re.sub(r"[^A-Z0-9]", "", s)


def eth_key(x):
    """Ethiopia: a woreda / zone name without the words that files add or drop (SUB CITY, SC, KALITY, TOWN, ZONE ...)."""
    s = re.sub(r"\b(SUB ?CITY|SC|KALITY|KALITI|CITY|TOWN|WOREDA|WOR|ZONE|ZUR|SPECIAL|ADMINISTRATION|ADMIN)\b", " ", asc(str(x)).upper())
    return re.sub(r"[^A-Z0-9]", "", s)


def lev(a, b):
    if a == b:
        return 0
    prev = list(range(len(b) + 1))
    for i, ca in enumerate(a, 1):
        cur = [i]
        for j, cb in enumerate(b, 1):
            cur.append(min(prev[j] + 1, cur[-1] + 1, prev[j - 1] + (ca != cb)))
        prev = cur
    return prev[-1]


def rd(name):
    p = REF / name
    return pd.read_csv(p, encoding="utf-8-sig", dtype=str, keep_default_na=False, na_values=[""]) if p.exists() else None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--names", nargs="*", default=[], help="CSV files with Country, Province, District of names to place")
    a = ap.parse_args()

    mst = rd("afrogdb_geoname_master.csv")
    D = mst[mst["level"] == "district"][["country", "province", "district", "geo_code"]].copy()
    cw = rd("crosswalk_country.csv")
    alias = {nk(r): nk(s) for r, s in zip(cw["raw_name"], cw["std_country"])} if cw is not None else {}
    D["ck"], D["pk"], D["dk"], D["rk"] = D.country.map(nk), D.province.map(nk), D.district.map(nk), D.district.map(rl)
    code_ok = set(D.geo_code)
    by_c = {c: g for c, g in D.groupby("ck")}
    exact = set(zip(D.ck, D.pk, D.dk))

    xw, man = rd("crosswalk_geonames.csv"), rd("crosswalk_manual.csv")
    xo = rd("crosscheck_old_vs_new.csv")
    eth = rd("ethiopia_woreda_to_zone.csv")

    # universe of names: crosswalk names without a decision, manual names, names given with --names
    names = {}
    def add(c, p, d, rows=0):
        k = (nk(c), nk(p), nk(d))
        if k not in names:
            names[k] = [c, p, d, 0]
        names[k][3] += int(rows or 0)
    if xw is not None:
        for r in xw.itertuples(index=False):
            if pd.isna(r.decision) or str(r.decision).strip() == "":
                add(r.country, r.province, r.district, r.rows if pd.notna(r.rows) else 0)
    if man is not None:
        for r in man.itertuples(index=False):
            add(r.country, r.province, r.district)
    for f in a.names:
        x = pd.read_csv(f, encoding="utf-8-sig", dtype=str, keep_default_na=False, na_values=[""])
        x.columns = [c.strip().lower() for c in x.columns]
        for r in x.itertuples(index=False):
            add(getattr(r, "country"), getattr(r, "province", None), getattr(r, "district", None), getattr(r, "rows", 0))

    mdict = {}
    if man is not None:
        for r in man.itertuples(index=False):
            mdict.setdefault((nk(r.country), nk(r.province), nk(r.district)), r)
    old = {}
    if xo is not None:
        for r in xo.itertuples(index=False):
            old[(nk(r.country), nk(r.province), nk(r.district))] = r
    wz = {}
    if eth is not None:
        for r in eth.itertuples(index=False):
            wz.setdefault(nk(r.woreda), []).append(r)

    # Ethiopia: every woreda (and every zone) points to its AFROGDB zone
    eth_pool = {}
    if eth is not None:
        for r in eth.itertuples(index=False):
            if pd.notna(r.zone_geo_code) and r.zone_geo_code in code_ok:
                eth_pool.setdefault(eth_key(r.woreda), set()).add(r.zone_geo_code)
    for r in D[D.ck == nk("ETHIOPIA")].itertuples(index=False):
        eth_pool.setdefault(eth_key(r.district), set()).add(r.geo_code)
    eth_zone_no = {}
    for r in D[D.ck == nk("ETHIOPIA")].itertuples(index=False):
        mm = re.search(r"ZONE ?(\d+)", asc(r.district).upper())
        if mm:
            eth_zone_no.setdefault((r.pk, mm.group(1)), set()).add(r.geo_code)

    out = []
    for (ck0, pk, dk), (c, p, d, rows) in names.items():
        ck = alias.get(ck0, ck0)
        if (ck, pk, dk) in exact:
            continue                                    # the standard matching places it
        g = by_c.get(ck)
        found = None
        # 1 manual decisions
        m = mdict.get((ck0, pk, dk))
        if m is not None and str(m.decision).strip().lower() in ("ok", "auto", "no_shape"):
            dec = str(m.decision).strip().lower()
            if dec == "no_shape":
                found = ("", "no shape", "certain", "manual: known gap")
            elif pd.notna(m.geo_code) and m.geo_code in code_ok:
                found = (m.geo_code, "manual", "certain" if dec == "ok" else "probable", "manual decision (%s)%s" % (dec, "" if pd.isna(m.note) else ": " + str(m.note)))
        # 2 Ethiopia woreda -> zone
        if found is None and ck == nk("ETHIOPIA") and dk in wz:
            cand = wz[dk]
            same_reg = [r for r in cand if nk(r.region) == pk] or cand
            codes = {r.zone_geo_code for r in same_reg if pd.notna(r.zone_geo_code) and r.zone_geo_code in code_ok}
            if len(codes) == 1:
                found = (codes.pop(), "woreda to zone", "certain", "woreda of zone %s" % same_reg[0].zone)
        if found is None and ck == nk("ETHIOPIA"):
            ek = eth_key(d)
            codes = eth_pool.get(ek, set())
            if len(codes) == 1:
                found = (next(iter(codes)), "woreda to zone", "certain", "woreda / zone name without SUB CITY, SC, TOWN ...")
            else:
                zm = re.fullmatch(r"\s*ZONE[ _-]?(\d+)\s*", asc(str(d)).upper())
                if zm and len(eth_zone_no.get((pk, zm.group(1)), ())) == 1:
                    found = (next(iter(eth_zone_no[(pk, zm.group(1))])), "woreda to zone", "certain", "ZONE %s of the region" % zm.group(1))
        # 3 relaxed
        if found is None and g is not None:
            pool = g[g.rk == rl(d)]
            if len(pool) > 1:
                pool = pool[pool.pk == pk]
            if len(pool) == 1:
                found = (pool.geo_code.iloc[0], "relaxed", "certain", "same name without generic words")
        # 4 old shape overlay
        if found is None and (ck0, pk, dk) in old:
            r = old[(ck0, pk, dk)]
            try:
                pv = float(r.p_best_is_right)
            except (TypeError, ValueError):
                pv = float("nan")
            if str(r.verdict) == "UNDECIDED" and pd.notna(r.spatial_best_code) and r.spatial_best_code in code_ok and pv >= P_OLD:
                found = (r.spatial_best_code, "old shape", "probable", "old shape falls inside this unit (p=%.2f, overlap %s)" % (pv, r.overlap_iou))
        # 5 name similarity (Ethiopia: against the woreda and zone names, which point to zones)
        if found is None and ck == nk("ETHIOPIA") and len(eth_key(d)) >= 6:
            ek = eth_key(d)
            keys = list(eth_pool)
            dist = np.array([lev(ek, z) / max(len(ek), len(z)) for z in keys])
            order = np.argsort(dist)
            best = float(dist[order[0]])
            best_codes = eth_pool[keys[order[0]]]
            nxt = [float(dist[i]) for i in order[1:] if eth_pool[keys[i]] != best_codes]
            second = nxt[0] if nxt else 1.0
            if best <= 0.25 and (second - best) >= MARGIN and len(best_codes) == 1:
                found = (next(iter(best_codes)), "name similarity", "probable", "closest woreda / zone name %s (distance %.2f)" % (keys[order[0]], best))
        if found is None and ck != nk("ETHIOPIA") and g is not None and dk:
            pool = g[g.pk == pk] if (g.pk == pk).any() else g
            dist = np.array([lev(dk, z) / max(len(dk), len(z)) for z in pool.dk])
            if len(dist):
                order = np.argsort(dist)
                best = float(dist[order[0]]); second = float(dist[order[1]]) if len(dist) > 1 else 1.0
                if best <= D_SIM and (second - best) >= MARGIN:
                    b = pool.iloc[order[0]]
                    found = (b.geo_code, "name similarity", "probable", "closest name %s / %s (distance %.2f)" % (b.province, b.district, best))
        if found is not None:
            out.append({"country": c, "province": p, "district": d, "geo_code": found[0], "method": found[1], "confidence": found[2], "evidence": found[3], "rows": rows})

    res = pd.DataFrame(out, columns=["country", "province", "district", "geo_code", "method", "confidence", "evidence", "rows"])
    res = res.sort_values(["country", "province", "district"]).reset_index(drop=True)
    res.drop(columns="rows").to_csv(REF / "crosswalk_extended.csv", index=False, encoding="utf-8-sig", lineterminator="\r\n")
    rep = ["Extended crosswalk: %d names placed (%d rows in the sources that carry a row count)" % (len(res), int(res.rows.sum())), ""]
    for (meth, conf), g in res.groupby(["method", "confidence"]):
        rep.append("  %-16s %-9s %5d names %7d rows" % (meth, conf, len(g), int(g.rows.sum())))
    (REF / "crosswalk_extended_report.txt").write_text("\n".join(rep) + "\n", encoding="utf-8")
    print("\n".join(rep))


if __name__ == "__main__":
    sys.exit(main())
