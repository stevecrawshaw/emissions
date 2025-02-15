pacman::p_load(tidyverse,
               readxl,
               glue,
               janitor,
               ggthemes,
               openair,
               ggtext)
# https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021
raw_la_tbl <- read_xlsx(
  'data/2005-21-uk-local-authority-ghg-emissions-update-060723.xlsx',
  sheet = '1_2', range = 'A5:AX7111')

pc_tbl <- raw_la_tbl %>%
  clean_names()

per_cap_tbl <- pc_tbl %>% 
  filter(
    local_authority %in% c(
      "Bath and North East Somerset",
      "Bristol, City of",
      "South Gloucestershire",
      "North Somerset"
    ) | region_country == "National Total") %>% 
#  filter(calendar_year == 2021) %>% 
  group_by(region_country) %>% 
  summarise(mean_per_cap_emissions = mean(per_capita_emissions_t_co2e))
  # select(per_capita_emissions_t_co2e)

  per_cap_tbl
