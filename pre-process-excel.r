# read the excel file and clean the column names
# write to a table in duckdb
pacman::p_load(tidyverse,
               readxl,
               glue,
               janitor,
               duckdb,
               DBI)

emissions_xl_tbl <- read_xlsx("data/2005-22-uk-local-authority-ghg-emissions.xlsx",
                              sheet = "1_2",
                              range = "A5:AX7259") |> 
  clean_names()


con <- dbConnect(duckdb::duckdb(), "data/emissions.duckdb")
dbWriteTable(con, "emissions_tbl", emissions_xl_tbl)
dbDisconnect(con)

dbListTables(con)
dbListFields(con, "emissions_tbl")
