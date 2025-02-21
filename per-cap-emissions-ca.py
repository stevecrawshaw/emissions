# %%
from pathlib import Path

import altair as alt
import ibis
import ibis.selectors as s
from ibis import _
from yaml import safe_load

# %%
ibis.options.interactive = True
# %%
config_path = Path("../config.yml")
md_token = safe_load(config_path.read_text()).get("motherduck").get("token")
# %%
# print(md_token)
# %%
connect_string = f"md:mca_data?motherduck_token={md_token}"
con = ibis.duckdb.connect(connect_string)

# %%

con.list_tables()
# %%

# emissions_tbl = con.table("emissions_tbl")
# emissions_tbl.columns

emissions_tbl = ibis.read_parquet("../mca-data/data/emissions_clean.parquet")

# %%

# %%
national_tbl = emissions_tbl.filter(_.region_country == "National Total")

# %%
ca_la_tbl = con.table("ca_la_tbl").filter(_.ladnm != "North Somerset")
ca_la_tbl.info()
# ca_la_tbl.columns

# %%

ca_percap_proper_tbl = (
    emissions_tbl.join(
        ca_la_tbl,
        how="inner",
        predicates=emissions_tbl.local_authority_code == ca_la_tbl.ladcd,
    )
    .group_by(_.cauthnm, _.calendar_year)
    .agg(
        gt_sum_ca=_.grand_total.sum(),
        pop_sum_ca=_.population_000s_mid_year_estimate.sum(),
    )
    .mutate(per_cap=_.gt_sum_ca / _.pop_sum_ca)
    .rename(area="cauthnm")
    .select(_.calendar_year, _.area, _.per_cap)
)  # .order_by(_.calendar_year)
# %%
# generate the sql for an expression
ibis.to_sql(ca_percap_proper_tbl)
# %%

# %%
national_percap_proper_tbl = (
    national_tbl.mutate(area=ibis.literal("National"))
    .mutate(per_cap=_.grand_total / _.population_000s_mid_year_estimate)
    .select(_.calendar_year, _.area, _.per_cap)
    .order_by(_.calendar_year)
)

# %%
national_percap_proper_tbl.execute()

# %%
# stack the two tables
unified_percap_tbl = ca_percap_proper_tbl.union(national_percap_proper_tbl)
# %%
# use this syntax to expose the sql and create a vierw based on it
# in the motherduck db
ibis.to_sql(unified_percap_tbl)
# %%

# %%

# %%
# plotted in ggplot
chart = (
    alt.Chart(unified_percap_tbl)
    .mark_line()
    .encode(
        x=alt.X("calendar_year:N"),
        y=alt.Y("per_cap:Q"),
        color=alt.Color("area:N"),
    )
)
# .facet(row="area")

# %%
chart
# %%

# %%

unified_percap_tbl.to_csv("data/unified_percap.csv")
# %%

con.disconnect()
# %%
