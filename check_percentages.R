pacman::p_load(tidyverse, glue, janitor)

raw_2023_tbl <- read_csv("https://opendata.westofengland-ca.gov.uk/api/explore/v2.1/catalog/datasets/ca_la_ghg_emissions_sub_sector_ods_vw/exports/csv?lang=en&refine=local_authority%3A%22Bristol%2C%20City%20of%22&refine=local_authority%3A%22North%20Somerset%22&refine=local_authority%3A%22South%20Gloucestershire%22&refine=local_authority%3A%22Bath%20and%20North%20East%20Somerset%22&facet=facet(name%3D%22local_authority%22%2C%20disjunctive%3Dtrue)&qv1=(calendar_year%3D2023)&timezone=Europe%2FLondon&use_labels=false&delimiter=%2C")


raw_2023_tbl |> 
#  filter(local_authority != "North Somerset") |> 
  mutate(total_emissions = sum(territorial_emissions_kt_co2e, na.rm = TRUE)) |>
  group_by(la_ghg_sector) |>
  summarise(
    emissions_pc = sum(territorial_emissions_kt_co2e,
                          na.rm = TRUE) 
    / mean(total_emissions) * 100,
    .groups = "drop"
  )
