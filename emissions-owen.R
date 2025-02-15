pacman::p_load(tidyverse, janitor, glue, timetk)


test <- read_csv(
 "data/ca_emissions_ods.csv"
) |> 
  filter(cauthnm == "West of England") |>
  glimpse()

test |> 
  write_csv("data/test_emissions.csv")

# test |> 
#   pivot_wider(id_cols = c("region_code", "cauthnm", "cauthcd", "local_authority", "ladcd", "la_ghg_sector", "la_ghg_sub_sector"),
#               names_from = "calendar_year",
#               values_from = c("territorial_emissions_kt_co2e",
#                               "co2_emissions_within_the_scope_of_influence_of_las_kt_co2"),
#               values_fn = sum
#               ) |> view()




test |> 
  group_by(calendar_year, region_code, cauthnm, cauthcd, local_authority, ladcd, la_ghg_sector, la_ghg_sub_sector) |>
  summarise(territorial_emissions_kt_co2e = sum(territorial_emissions_kt_co2e),
            co2_emissions_within_the_scope_of_influence_of_las_kt_co2  = sum(co2_emissions_within_the_scope_of_influence_of_las_kt_co2),
            mid_year_population_thousands = mean(mid_year_population_thousands),
            area_km2 = mean(area_km2)) |>
arrange(calendar_year, region_code, cauthnm, cauthcd, local_authority, ladcd, la_ghg_sector, la_ghg_sub_sector) |>
  mutate(
    across(where(is.numeric), first, .names = "base_{.col}")
    ) |> 
  mutate(index_territorial_emissions = base_territorial_emissions_kt_co2e / territorial_emissions_kt_co2e,
         index_co2_emissions_within_scope_of_influence_of_las = base_co2_emissions_within_the_scope_of_influence_of_las_kt_co2 /  na_if(co2_emissions_within_the_scope_of_influence_of_las_kt_co2, 0),
         index_mid_year_population_thousands  = base_mid_year_population_thousands / mid_year_population_thousands ) |>
  
  view()
  