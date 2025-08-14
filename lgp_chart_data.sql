duckdb


-- get WECA+ data from ODS
CREATE OR REPLACE TABLE lep_transport_domestic_tbl AS 
SELECT year(calendar_year) "Year" , la_ghg_sector Sector, SUM(territorial_emissions_kt_co2e) "Emissions KTCO2e", 'WECA+' Area
FROM read_parquet('https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/ca_la_ghg_emissions_sub_sector_ods_vw/exports/parquet?lang=en&refine=cauthnm%3A%22West%20of%20England%22&refine=la_ghg_sector%3A%22Transport%22&refine=la_ghg_sector%3A%22Domestic%22&facet=facet(name%3D%22la_ghg_sector%22%2C%20disjunctive%3Dtrue)&facet=facet(name%3D%22cauthnm%22%2C%20disjunctive%3Dtrue)&qv1=(calendar_year%3A%5B2014-01-01%20TO%202023-01-01%5D)&timezone=Europe%2FLondon')
GROUP BY calendar_year, la_ghg_sector
ORDER BY calendar_year, la_ghg_sector;

DESCRIBE lep_transport_domestic_tbl;

FROM lep_transport_domestic_tbl;

-- get the UK data from the most recent CSV file published in July each year

 CREATE OR REPLACE TABLE uk_transport_domestic_tbl AS
 SELECT calendar_year "Year", la_ghg_sector Sector, sum(territorial_emissions_kt_co2e) "Emissions KTCO2e", 'UK' Area 
 FROM read_csv('data/2005-23-uk-local-authority-ghg-emissions-CSV-dataset.csv', normalize_names=true)
 WHERE calendar_year >= 2014
 AND 
 la_ghg_sector IN ('Transport', 'Domestic')
 GROUP BY calendar_year, la_ghg_sector
 ORDER BY calendar_year, la_ghg_sector;

--  union the two tables
CREATE OR REPLACE TABLE emissions_compare_base_tbl AS
FROM uk_transport_domestic_tbl
UNION BY NAME
FROM lep_transport_domestic_tbl;

FROM emissions_compare_base_tbl;

-- pivot to create columns by time
CREATE OR REPLACE TABLE pivoted_emissions_base_tbl AS
PIVOT emissions_compare_base_tbl
ON Sector, Area
USING sum("Emissions KTCO2e");


-- create an indexed dataset for the chart in the powerpoint
CREATE OR REPLACE TABLE indexed_ts_emissions_tbl AS 
WITH base_year AS (
  SELECT
    "Domestic_UK" AS base_domestic_uk,
    "Domestic_WECA+" AS base_domestic_weca,
    "Transport_UK" AS base_transport_uk,
    "Transport_WECA+" AS base_transport_weca
  FROM
    pivoted_emissions_base_tbl
  WHERE
    "Year" = 2014
)
SELECT
  d."Year",
  (
    d."Domestic_UK" / b.base_domestic_uk
  ) * 100 AS "Domestic UK Index",
  (
    d."Domestic_WECA+" / b.base_domestic_weca
  ) * 100 AS "Domestic WECA+ Index",
  (
    d."Transport_UK" / b.base_transport_uk
  ) * 100 AS "Transport UK Index",
  (
    d."Transport_WECA+" / b.base_transport_weca
  ) * 100 AS "Transport WECA+ Index"
FROM
  pivoted_emissions_base_tbl d,
  base_year b
ORDER BY
  d."Year";

-- save to csv and use in the template slides PPT

COPY indexed_ts_emissions_tbl TO
'data/lgp_lep_uk_indexed_emissions.csv';