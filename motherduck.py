# %%
from pathlib import Path

import ibis
import ibis.selectors as s
from ibis import _

# %%
ibis.options.interactive = True
# %%

con = ibis.duckdb.connect("md:")

# %%

con.list_tables()
# %%

con.table("ca_la_tbl").info()

# %%
con.attach("data/emissions.duckdb", "emissions")

# %%
con.list_tables(database="data/emissions.duckdb")

# %%
con = ibis.duckdb.connect("data/emissions.duckdb")

# %%

con.table("emissions_tbl").to_parquet("data/emissions_tbl.parquet")
# %%


# %%


# %%


# %%


# %%


# %%
