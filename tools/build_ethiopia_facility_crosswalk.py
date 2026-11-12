"""Build reference/ethiopia_facility_to_woreda.csv
Ethiopia LQAS form 6837: the team picks the woreda on the form (field 'facility', filtered by zone).
This file links each (zone, facility) pair as typed in the raw data to the OCHA woreda name and code,
using the majority of the GPS points recorded under that pair.
Usage: python build_xwalk.py <6837.parquet> <ocha_admin3.shp> <out.csv>
"""
import sys, re, pandas as pd, geopandas as gpd
raw, shp, out = sys.argv[1:4]
d = pd.read_parquet(raw)
num = lambda s: pd.to_numeric(s, errors='coerce')
d['lat'] = num(d['_geolocation[1]']); d['lon'] = num(d['_geolocation[2]'])
w = gpd.read_file(shp)[['admin3Name', 'admin3Pcod', 'geometry']]
ok = d.lat.notna() & d.lon.notna() & (d.lat != 0)
pts = gpd.GeoDataFrame(d.loc[ok, []], geometry=gpd.points_from_xy(d.loc[ok, 'lon'], d.loc[ok, 'lat']), crs=w.crs)
j = gpd.sjoin(pts, w, how='left', predicate='within'); j = j[~j.index.duplicated()]
d['gps_name'] = j.admin3Name; d['gps_code'] = j.admin3Pcod
sq = lambda s: re.sub(r'\s+', ' ', str(s).strip().upper())
d['zone_form'] = d.District.map(sq); d['facility_form'] = d.facility.map(sq)
rows = []
for (z, f), g in d.groupby(['zone_form', 'facility_form']):
    n = len(g); vc = g.gps_code.value_counts()
    if len(vc):
        code = vc.index[0]; share = vc.iloc[0] / n
        name = g.loc[g.gps_code == code, 'gps_name'].iloc[0]
    else:
        code, share, name = None, 0.0, None
    assign = (code is not None) and share >= 0.8 and n >= 3
    rows.append(dict(zone_form=z, facility_form=f, records=n, gps_agreement=round(share, 2),
                     woreda=name if assign else f, woreda_pcode=code if assign else '',
                     needs_review='' if assign else 'yes', gps_best_guess=name or '', gps_best_guess_pcode=code or ''))
x = pd.DataFrame(rows)
x.to_csv(out, index=False, encoding='utf-8')
print(len(x), 'pairs;', (x.needs_review == 'yes').sum(), 'to review covering', x.loc[x.needs_review == 'yes', 'records'].sum(), 'of', x.records.sum(), 'records')
