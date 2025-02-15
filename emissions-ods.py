# %%
from pathlib import Path

import ibis
import ibis.selectors as s
from ibis import _

# %%
ibis.options.interactive = True
# %%

ghg_path = Path("data/2005-2022-local-authority-ghg-emissions-csv-dataset.csv")

# %%

con = ibis.connect("duckdb://../mca-data/data/ca_epc.duckdb")
# %%
# need to use the read_csv method of the connection
# to read the csv file
# otherwise you get a multiple backends ibis error
ghg_data = con.read_csv(ghg_path).rename("snake_case")

# %%
ghg_data.info()

# %%
ca_la_tbl = con.table("ca_la_tbl")

# %%
ca_emissions_tbl = ghg_data.join(
    ca_la_tbl, ghg_data.local_authority_code == ca_la_tbl.ladcd, how="inner"
)

# %%
ca_emissions_tbl.columns
# %%


# %%
ca_emissions_ods = ca_emissions_tbl.drop(
    [
        "local_authority_code",
        "country",
        "country_code",
        "region",
        "second_tier_authority",
        "ladnm",
    ]
).rename(lambda x: str.replace(x, "(", "").replace(")", ""))


# %%
ca_emissions_ods.columns
# %%

(
    ca_emissions_ods.group_by(
        [
            "ladcd",
            "local_authority",
            "cauthnm",
            "cauthcd",
            "greenhouse_gas",
            "la_ghg_sector",
            "la_ghg_sub_sector",
        ]
    ).mutate(index=_.territorial_emissions_kt_co2e)
)

# %%

ca_emissions_ods.group_by(["ladcd", "local_authority_name", "cauthnm", "cauthcd"])

# %%
ca_emissions_ods.to_csv("data/ca_emissions_ods.csv")

# %%
# %%


# %%


# %%
# %%


# %%


# %%
