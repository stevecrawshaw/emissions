# This script generates the chart and data for the trajectory to 
# Net zero for CO2e emissions from territorial activity
# As well as the path to net zero it also shows the trajectory for BAU using Compound Annual Growth Rate and the emissions gap in 2030

# also produced are:

# sectoral contribution and 10 year trend
# changes in per capita emissions
# specific chart for 10 year trend of commercial and industrial emissions
# territorial emissions have been used throughout and North Somerset not included

pacman::p_load(tidyverse,
               readr,
               readxl,
               glue,
               janitor,
               ggthemes,
               ggtext,
               openair,
               gt,
               config,
               gtExtras,
               svglite,
               scales
               )


weca_core_colours <- config::get(config = "colours")[c(-1)] %>% 
  unlist()

woe_las <- c("Bath and North East Somerset",
             "Bristol, City of",
             "South Gloucestershire",
             "North Somerset")

# woe_las = "all"

# data updated june \ july each year
# https://assets.publishing.service.gov.uk/media/667ad86497ea0c79abfe4bfd/2005-2022-local-authority-ghg-emissions-csv-dataset.csv
# 
source_path <- "data/2005-23-uk-local-authority-ghg-emissions-CSV-dataset.csv"

read_filter <- function(source_path, woe_las = "all"){
  
  all_las <- read_csv(source_path) %>% 
    clean_names() 
  
  if (woe_las[1] != "all") {
  # filter for just the West of England local authorities
  
   out <- all_las %>% 
      filter(local_authority %in% woe_las)
  } else {
  out <- all_las  
  }
  
  return (out)
}
  
source_data_tbl <- read_filter(source_path, woe_las = "all")

# Calculate Net Zero and BAU Trajectories ----

historic_tbl <-  source_data_tbl %>% 
  group_by(calendar_year) %>% 
  summarise(emissions_co2e_kt = sum(territorial_emissions_kt_co2e)) %>% 
  mutate(source = "Historic")

# Get the start and end values and the number of years
start_year <- min(historic_tbl$calendar_year)
end_year <- max(historic_tbl$calendar_year)

start_value <- historic_tbl$emissions_co2e_kt[historic_tbl$calendar_year == start_year]
end_value <- historic_tbl$emissions_co2e_kt[historic_tbl$calendar_year == end_year]

# Number of years between the start and end values
n <- end_year - start_year

# Calculate CAGR
CAGR <- (end_value / start_value)^(1 / n) - 1

# Project the emissions for 2030
project_year <- 2030
years_to_project <- project_year - end_year
projected_emissions_2030 <- end_value * (1 + CAGR)^years_to_project

ext_years  <-  seq.int(end_year + 1, project_year)
l <-  length(ext_years)

value  <-  end_value
nz_value <-  end_value
proj_emissions  <-  vector(mode = "numeric", length = l)
nz_traj_emission <-  vector(mode = "numeric", length = l)

nz_traj_years <- seq.int(end_year, project_year)

traj_len  <-  length(nz_traj_emission)
nz_annual_reduction <-  end_value / traj_len


# Display the results
calc_data <- list(
  start_year = start_year,
  end_year = end_year,
  start_value = start_value,
  end_value = end_value,
  CAGR = CAGR,
  project_year = project_year,
  projected_emissions_2030_BAU = projected_emissions_2030,
  annual_reduction_for_net_zero_2030 = nz_annual_reduction
) %>% 
  enframe() %>% 
  mutate(value = unlist(value)) %>% 
  glimpse()

for (i in 1:(project_year - end_year)){
  value = value + (value * CAGR)
  proj_emissions[i] = value
  nz_value = nz_value - nz_annual_reduction
  nz_traj_emission[i] = round(nz_value)
  
}

nz_traj_emission  <- c(historic_tbl$emissions_co2e_kt[historic_tbl$calendar_year == end_year], nz_traj_emission)

proj_emissions = c(historic_tbl$emissions_co2e_kt[historic_tbl$calendar_year == end_year], proj_emissions)

projections <- tibble(calendar_year = nz_traj_years,
                      emissions_co2e_kt = proj_emissions,
                      source = "Predicted BAU")

nz_traj_tbl <- tibble(calendar_year = nz_traj_years,
                      emissions_co2e_kt = nz_traj_emission,
                      source = "Net zero 2030")

plot_tbl <- bind_rows(historic_tbl, projections, nz_traj_tbl) %>% 
  mutate(calendar_year = as.integer(calendar_year)) %>% 
  glimpse()

cols <- weca_core_colours[2:4] %>% unname()

yend <- min(projections$emissions_co2e_kt)
scale_x = seq.int(start_year, project_year, 5)

# theme to apply to line charts
chart_theme <- function(){
  theme_clean() +
    theme(legend.background = element_blank(),
          panel.grid.major = element_line(linewidth = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          plot.title = element_text(size = 15),
          plot.subtitle = element_markdown(lineheight = 1.1),
          plot.background = element_rect(linewidth = NULL)) 
}

plot <- plot_tbl %>% 
  ggplot(aes(x = calendar_year, y = emissions_co2e_kt, colour = source)) +
  geom_line(linewidth = 2) +
  scale_colour_manual(values = c(Historic = cols[1],
                                 `Net zero 2030` = cols[2],
                                 `Predicted BAU` = cols[3])) +
  
  geom_segment(aes(
    x = project_year, y = 0, xend = project_year, yend = yend),
    arrow = arrow(length = unit(0.2, 'cm'), ends = "both", angle = 20),
    lwd = 1,
    data = plot_tbl %>% head(1),
    colour = weca_core_colours[1],
    lty = 5) +
  geom_text(inherit.aes = FALSE,
            mapping = aes(x = project_year, y = yend / 2),
            data = plot_tbl %>% head(1),
            nudge_x = 3,
            size = 5,
            label = glue("Gap\n{round(projected_emissions_2030)} kt")) +
  scale_x_continuous(breaks = scale_x) +
  labs(title = "West of England trajectory to net zero",
       subtitle = "All sectors: Territorial Emissions",
       colour = "Type",
       x = "Year",
       y = quickText("kt CO2e"),
       caption = "DESNZ UK LA Greenhouse gas emissions July 2025") +
  chart_theme()


plot

ggsave('plots/WoE_emissions_net_zero_2023.png',
       plot = plot,
       device = "png", height = 7, width = 10)    
plot_tbl %>% 
  write_csv('data/woe_emissions_territorial_2023.csv')

calc_data %>% 
  write_csv("data/net_zero_calculation_data_territorial_2023.csv")

# Derive Contributions by sector ----

sector_tbl <- source_data_tbl %>% 
  select(local_authority, la_ghg_sector, where(is.numeric)) %>% 
  glimpse()

(min_year <- max(sector_tbl$calendar_year) - 9)
(cur_year <- max(sector_tbl$calendar_year))
new_names <- c("la_ghg_sector", "year_start", "year_end")

sector_change_10yr_tbl <- sector_tbl %>% 
  filter(calendar_year == min_year |
         calendar_year == cur_year) %>% 
  group_by(la_ghg_sector, calendar_year) %>% 
  summarise(emissions_co2e_kt = sum(territorial_emissions_kt_co2e), .groups = "drop") %>%
  pivot_wider(id_cols = la_ghg_sector,
              names_from = calendar_year,
              values_from = emissions_co2e_kt, names_prefix = "year_") %>%
set_names(new_names) %>% 
      glimpse()


chart_tbl <- sector_change_10yr_tbl %>% 
  mutate(percentage_of_total = (year_end / sum(year_end)) * 100,
         change_10yr_perc = ((year_start - year_end) * -100 / year_start)
         ) %>% 
           arrange(change_10yr_perc) %>% 
  mutate(axis_name = glue("{la_ghg_sector} ({round(percentage_of_total)}%)") %>% 
           as_factor()) %>%
  glimpse()

sector_change_plot <- ggplot(chart_tbl,
                             aes(x = axis_name,
                      y = change_10yr_perc)) +
  geom_segment(aes(x = axis_name,
                   xend = axis_name, 
                   y = 0, 
                   yend = change_10yr_perc),
               color = "gray", lwd = 2) +
  geom_point(inherit.aes = TRUE,
             size = 4, pch = 21, bg = 4, col = 1) +
  scale_x_discrete(labels = chart_tbl$axis_name) +
  coord_flip() +
  labs(title = glue("Emissions change by sector\n{min_year} to {cur_year}"),
       y = "Percentage change",
       x = glue("Sector (contribution) in {cur_year}")) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold")
  )

sector_change_plot


ggsave("plots/sector_change_plot_inc_ns.png",
       plot = sector_change_plot,
       bg = "white", height = 7, width = 5)

# Make GT table of sector trends and contributions ----

make_subset_co2_tbl <- function(sector_tbl, woe_las){
    subset_co2_tbl <- sector_tbl |> 
  filter(calendar_year >= min_year) %>% 
  select(local_authority,
         calendar_year,
         territorial_emissions_kt_co2e,
         mid_year_population_thousands,
         la_ghg_sector)

  if (woe_las[1] != "all"){
  out <- subset_co2_tbl
  } else {
    out <- subset_co2_tbl |> 
  filter(la_ghg_sector != "LULUCF")
  
  }
return(out)  
}

subset_co2_tbl <- make_subset_co2_tbl(sector_tbl, woe_las)

sector_pop_tbl <- subset_co2_tbl |>
  group_by(calendar_year, la_ghg_sector, local_authority) |>
  summarise(total_emissions = sum(territorial_emissions_kt_co2e,
                                  na.rm = TRUE),
            total_population = mean(mid_year_population_thousands,
                                    na.rm = TRUE)) |>
  summarise(total_emissions_woe = sum(total_emissions,
                                      na.rm = TRUE),
            total_population_woe = sum(total_population,
                                       na.rm = TRUE),
            .groups = "drop") 

# calculate the ten year change  percent
gt_source_tbl_1 <- sector_pop_tbl %>% 
  filter(calendar_year == end_year | calendar_year == min_year) %>% 
  pivot_wider(id_cols = la_ghg_sector,
              names_from = calendar_year, values_from = total_emissions_woe, names_prefix = "year_") %>% 
  set_names(new_names) %>% 
  mutate(pct_change_10yr = (year_end - year_start) / year_start,
         sec_prop = year_end / sum(year_end, na.rm = TRUE)) %>% 
  glimpse()

# fontAwesome icons for gt table
fa_tbl <- c("Agriculture" = "tractor",
            "Commercial" = "building",
            "Domestic" = "house",
            "Industry" = "industry",
            "LULUCF" = "tree",
            "Public Sector" = "hospital",
            "Transport" = "car-side",
            "Waste" = "recycle") |> 
  enframe(name = "la_ghg_sector", value = "fa_icon")|> 
  glimpse()

gt_source_tbl <- gt_source_tbl_1 |>
  inner_join(fa_tbl,
             by = join_by("la_ghg_sector" == "la_ghg_sector")) |> 
  mutate(la_ghg_sector = if_else(la_ghg_sector == "LULUCF",
                                 "Land Use",
                                 la_ghg_sector)) |> 
  arrange(desc(sec_prop)) %>%
  glimpse()

sector_gt <- gt_source_tbl |> 
  select(fa_icon, la_ghg_sector, sec_prop, pct_change_10yr) %>% 
  gt() |> 
  fmt_icon(
    fa_icon
  ) %>% 
  gt_plt_bar(sec_prop,
             scale_type = "percent",
             color = weca_core_colours[1],
             width = 70) %>% 
  gt_plt_bar(pct_change_10yr,
             scale_type = "percent", 
             color = weca_core_colours[5],
             text_color = weca_core_colours[1],
             width = 70) %>% 
  cols_label(
    fa_icon = "",
    la_ghg_sector = "Sector",
    sec_prop = glue("Contribution {end_year}"),
    pct_change_10yr = "10 year change"
  ) %>% 
  cols_move_to_end(sec_prop) 
  
sector_gt

sector_gt %>% 
  gtsave("plots/gt_sector_table_territorial.png")

# Sector time series ---
woe_broad_sector_tbl <- sector_tbl |> 
  group_by(la_ghg_sector, calendar_year) |>
  summarise(total_emissions = sum(territorial_emissions_kt_co2e),
            .groups = "drop") |> 
  pivot_wider(names_from = la_ghg_sector,
              values_from = total_emissions) |>
  mutate(`Business and Public` = Agriculture + Commercial + Industry + `Public Sector` + LULUCF + Waste) |>
  select(-Agriculture, -Commercial, -Industry, -`Public Sector`, -LULUCF, -Waste) |>
  pivot_longer(cols = -calendar_year,
               names_to = "la_ghg_sector",
               values_to = "total_emissions") |>
  glimpse()

sector_3_time_series_plot <- woe_broad_sector_tbl %>%
  ggplot(aes(x = calendar_year,
             y = total_emissions,
             color = la_ghg_sector,
             group = la_ghg_sector)) +
  geom_line(lwd = 3) +
  scale_color_manual(values = cols) +
  labs(title = "Greenhouse Gases: Territorial Emissions Trends",
       subtitle = "West of England",
       caption = "Business and Public includes:\nAgriculture, Commercial, Industry,\nPublic Sector, LULUCF and Waste",
       x = "Year",
       y = quickText("KtCO2e"),
       color = "Sector") +
  expand_limits(y = c(0, max(woe_broad_sector_tbl$total_emissions))) +
  chart_theme() +
  theme(legend.text = element_text(size = 22),
        legend.title = element_text(size = 24),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        plot.title = element_text(size = 24),
        legend.position = "bottom")


sector_3_time_series_plot

ggsave("plots/sector_3_time_series_plot.png",
       plot = sector_3_time_series_plot,
       bg = "white", height = 7, width = 10)




# per capita trends ----

per_cap_emissions_tbl <- woe_sector_tbl %>% 
  group_by(calendar_year, local_authority) %>% 
  summarise(emissions_la_kt = sum(territorial_emissions_kt_co2e),
            population = mean(mid_year_population_thousands), .groups = "drop") %>% group_by(calendar_year) %>% 
  summarise(total_emissions_woe = sum(emissions_la_kt),
            total_population = sum(population), .groups = "drop") %>% 
  mutate(per_cap_emissions = total_emissions_woe / (total_population)) |> 
  glimpse()
  
  per_cap_emissions_tbl %>% 
    write_csv("data/per_cap_territorial.csv")
  
# reduction in per capita emissions (territorial)  
decline_per_cap_territorial <- ((per_cap_emissions_tbl$per_cap_emissions[per_cap_emissions_tbl$calendar_year == start_year] - per_cap_emissions_tbl$per_cap_emissions[per_cap_emissions_tbl$calendar_year == cur_year]) / per_cap_emissions_tbl$per_cap_emissions[per_cap_emissions_tbl$calendar_year == start_year]) * 100


# Sector trends

ind_com <- c("Commercial", "Industry")
cols <- weca_core_colours[2:3] %>% unname()

ind_com_chart <- woe_sector_pop_tbl %>% 
  filter(la_ghg_sector %in% ind_com) %>% 
  select(-total_population_woe) %>% 
  mutate(calendar_year = as.integer(calendar_year)) %>% 
  ggplot(aes(x = calendar_year,
             y = total_emissions_woe,
             color = la_ghg_sector,
             group = la_ghg_sector)) +
  scale_x_continuous(labels = unique(woe_sector_pop_tbl$calendar_year),
                   breaks = unique(woe_sector_pop_tbl$calendar_year)) +
  scale_color_manual(values = c("Industry" = "#ed749d",
                                "Commercial" = "#6bd4f2"))+
  geom_line(lwd = 2) +
  labs(title = "Greenhouse Gases: Emissions Trends",
       subtitle = "Commercial and Industrial Sectors",
       caption = "Territorial emissions, West of England",
       alt = "Chart showing change in emissions from commerce and industry in the West of England.",
       x = "Year",
       y = quickText("KtCO2e"),
       color = "Sector") +
  chart_theme()


ind_com_chart

ggsave("plots/commercial_and_industrial_trends_woe.png",
       plot = ind_com_chart, bg = "white", height = 7, width = 10)

scales::show_col(weca_core_colours)


# Emissions by local authority ----
# 
la_breakdown_tbl <- woe_sector_tbl |> 
  group_by(local_authority, calendar_year) |> 
  summarise(total_emissions = sum(territorial_emissions_kt_co2e),
            total_emissions_la_control = sum(emissions_within_the_scope_of_influence_of_l_as_kt_co2)) |>
  pivot_longer(cols = c(total_emissions, total_emissions_la_control),
               names_to = "emission_type",
               values_to = "KtCO2e") |>
  mutate(emission_type = if_else(emission_type == "total_emissions",
                                 "Total Emissions",
                               "Emissions within LA control"),
         calendar_year = as.integer(calendar_year)) |>
  filter(calendar_year >= min_year) |>
  glimpse()

colours = weca_core_colours[1:3] |> 
  set_names(woe_las)

plot_la_bar_10yr <- la_breakdown_tbl %>%
  filter(emission_type == "Total Emissions") %>%
  ggplot(aes(x = calendar_year,
             y = KtCO2e,
             color = local_authority,
             group = local_authority,
             fill = local_authority)) +
  geom_col(position = "dodge" ) +
  scale_x_continuous(breaks = function(x) round(seq(min(x), max(x), by = 1))) +
  scale_fill_manual(values = colours) +
  scale_color_manual(values = colours) +
  chart_theme() +
  labs(title = "Territorial Emissions by Local Authority",
       subtitle = "West of England",
       x = "Year",
       y = quickText("KtCO2e"),
       color = "Local Authority",
       fill = "Local Authority") 

plot_la_bar_10yr

ggsave("plots/la_plot_10yr_total_territorial_emissions.png",
       plot_la_bar_10yr,
       bg = "white",
       height = 7,
       width = 10)
  

la_breakdown_tbl |> 
  filter(calendar_year == cur_year | calendar_year == min_year,
         emission_type == "Total Emissions") |>
  pivot_wider(id_cols = local_authority,
              names_from = calendar_year,
              values_from = KtCO2e, names_prefix = "Territorial_Emissions_KtCO2e_") |>
  mutate(pct_change_10yr = round(((`Territorial_Emissions_KtCO2e_2022` - `Territorial_Emissions_KtCO2e_2013`) / `Territorial_Emissions_KtCO2e_2013`) * 100)) |>
  write_csv("data/la_breakdown_territorial_10yrs.csv")

# q: how to display x axis values in ggplot as integers?
# a: use scale_x_continuous(labels = unique(woe_sector_pop_tbl$calendar_year),
# 
# 
# 
# 
#  SECTOR AND CONTRIBUTION ANALYSIS UK and MCA ----



source_data_tbl |> 
  filter(calendar_year == end_year) |>
  group_by(la_ghg_sector) |>
  summarise(total_emissions = sum(territorial_emissions_kt_co2e)) |> 
  mutate(percentage_of_total = total_emissions * 100 / sum(total_emissions, na.rm = TRUE))

