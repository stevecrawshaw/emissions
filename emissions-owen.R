pacman::p_load(tidyverse, janitor, glue, config, duckdb, DBI)


con <- dbConnect(duckdb::duckdb())

ghg_raw <- read_csv("data/2005-2022-local-authority-ghg-emissions-csv-dataset.csv") |> 
  clean_names()

ghg_raw |> glimpse()

md_token <- config::get(file = "../config.yml", config = "motherduck")$token

con <- duckdb::dbConnect("md://", token = md_token)


