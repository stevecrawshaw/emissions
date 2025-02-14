pacman::p_load(tidyverse, glue, gt, gtExtras, janitor, svglite)

raw_co2 <- read_csv("https://assets.publishing.service.gov.uk/media/667ad86497ea0c79abfe4bfd/2005-2022-local-authority-ghg-emissions-csv-dataset.csv") |> clean_names()

woe_las <- c("Bristol, City of",
             "South Gloucestershire",
             "Bath and North East Somerset")

start_year <- min(raw_co2$calendar_year)
end_year <- max(raw_co2$calendar_year)

subset_co2_tbl <- raw_co2 |> 
  filter(local_authority %in% woe_las) |> 
  select(local_authority,
         calendar_year,
         territorial_emissions_kt_co2e,
         mid_year_population_thousands,
         la_ghg_sector) |>
  glimpse()

woe_sector_pop_tbl <- subset_co2_tbl |>
  group_by(calendar_year, la_ghg_sector, local_authority) |>
  summarise(total_emissions = sum(territorial_emissions_kt_co2e, na.rm = TRUE),
            total_population = mean(mid_year_population_thousands, na.rm = TRUE)) |>
  summarise(total_emissions_woe = sum(total_emissions, na.rm = TRUE),
            total_population_woe = sum(total_population, na.rm = TRUE),
            .groups = "drop") 
  
gt_source_tbl_1 <- woe_sector_pop_tbl |> 
  arrange(la_ghg_sector, calendar_year) |>
  group_by(la_ghg_sector) |>
  summarise(emissions_ts_list = list(total_emissions_woe)) |>
  glimpse()

gt_source_tbl$emissions_ts_list[[1]]

gt_source_tbl_2 <- woe_sector_pop_tbl |> 
  filter(calendar_year == end_year) |>
  mutate(sector_proportion = round(total_emissions_woe / sum(total_emissions_woe, na.rm = TRUE) * 100)) |>
  select(la_ghg_sector, sector_proportion) |>

  glimpse()

fa_tbl <- c("Agriculture" = "tractor",
            "Commercial" = "building",
            "Domestic" = "house",
            "Industry" = "industry",
            "Land Use" = "tree",
            "Public Sector" = "hospital",
            "Transport" = "car-side",
          "Waste" = "recycle") |> 
  enframe(name = "la_ghg_sector", value = "fa_icon") 

fa_tbl |> glimpse()

gt_source_tbl <- gt_source_tbl_1 |>
  inner_join(gt_source_tbl_2, by = "la_ghg_sector") |> 
  mutate(la_ghg_sector = if_else(la_ghg_sector == "LULUCF", "Land Use", la_ghg_sector)) |> 
  inner_join(fa_tbl, by = join_by("la_ghg_sector" == "la_ghg_sector")) |> 
  arrange(desc(sector_proportion))

gt_source_tbl |> 
  gt() |> 
  gt_plt_sparkline(
    column = emissions_ts_list,
    type = "shaded",
    fig_dim = c(10, 20),
    same_limit = FALSE,
    label = FALSE
    ) |>
  gt_plt_bar(
    column = sector_proportion,
    width = 30,
    keep_column = TRUE,
    color = "steelblue",
    ) |>
  fmt_icon(
    fa_icon
    ) 
    