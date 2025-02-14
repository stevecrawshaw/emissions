pacman::p_load(tidyverse,
               readxl,
               glue,
               janitor,
               ggthemes,
               openair,
               ggtext)
# https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021

raw_la_tbl <- read_csv(
  'data/2005-2022-local-authority-ghg-emissions-csv-dataset.csv') %>% 
  clean_names()

woe_las <- c("Bath and North East Somerset",
             "Bristol, City of",
             "South Gloucestershire")


percap_woe_most_recent <- raw_la_tbl %>%
  # just the most recent year
  filter(calendar_year == 2022) %>% 
  select(local_authority, mid_year_population_thousands, co2_emissions_within_the_scope_of_influence_of_l_as_kt_co2) %>% 
  group_by(local_authority) %>% 
  summarise(
    # total emissions controllable by the LA
    emissions = sum(co2_emissions_within_the_scope_of_influence_of_l_as_kt_co2, na.rm = TRUE),
    # mean pop, as pop is present for each sector in the CSV
    population = mean(mid_year_population_thousands, na.rm = TRUE)) %>% 
  # create a grouping column for whether the LA is in WECA region
  mutate(woe = if_else(local_authority %in% woe_las, "WoE", "UK")) %>% 
  group_by(woe) %>% 
  summarise(per_cap = sum(emissions, na.rm = TRUE) / sum(population, na.rm = TRUE)) %>% 
  pivot_wider(names_from = woe, values_from = per_cap) %>%
  mutate(pc_emissions_diff = (WoE - UK) * 100 / UK ) %>%
  view()

