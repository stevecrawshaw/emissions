pacman::p_load(tidyverse,
               readxl,
               glue,
               janitor,
               ggthemes,
               openair,
               ggtext)
# https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021

# Territorial emissions ----
raw_la_tbl <- read_xlsx(
  'data/2005-21-uk-local-authority-ghg-emissions-update-060723.xlsx',
  sheet = '1_1', range = 'A5:AX7111') %>% 
  clean_names()

raw_la_tbl %>% glimpse()


weca_core_colours <- c(
  "Dark Blue" = "#354753",
  "Blue" = "#6bd4f2",
  "Pink" = "#ed749d",
  "Yellow" = "#ffd900",
  "Green" = "#79decc")

# filter for WoE

lep_tbl <- raw_la_tbl %>%
  filter(
    local_authority %in% c(
      "Bath and North East Somerset",
      "Bristol, City of",
      "South Gloucestershire",
      "North Somerset"
    ))

woe_tbl <- lep_tbl %>%
  filter(
    !local_authority %in% c(
      "North Somerset"
    ))
# Domestic emissions ----
woe_tbl %>%
  filter(calendar_year == 2021) %>% 
  select(local_authority, domestic_total, grand_total) %>% 
  summarise(prop = (sum(domestic_total) * 100) / sum(grand_total))


woe_tbl %>%
  filter(calendar_year >= 2011) %>% 
  select(calendar_year, 
         local_authority,
         domestic_total,
         grand_total) %>% 
  mutate(pc_dom = domestic_total * 100 / grand_total) %>% 
  # ggplot(aes(x = calendar_year,
  #            y = pc_dom, 
  #            group = local_authority, 
  #            colour = local_authority)) +
  # geom_line() +
  # expand_limits(y = c(0, 40))
  # 
  pivot_wider(id_cols = calendar_year,
              names_from = local_authority, 
              values_from = pc_dom) %>% 
  rename(Bristol = "Bristol, City of") %>% 
  rowwise() %>% 
  mutate(`West of England` = mean(c(Bristol,
                                    `South Gloucestershire`, 
                                    `Bath and North East Somerset`))) %>% 
  write_csv("data/re_emissions_domestic.csv")
  

pivot_longer(cols = ends_with("_total"))
  
# Business emissions ----

woe_tbl %>% glimpse()

comm_ind_emissions_tbl <- woe_tbl %>% 
  select(calendar_year,
         contains(c("indust", "commercial"))
         ) %>% 
  select(Year = calendar_year, ends_with("total")) %>% 
  rename(`Industry total` = industry_total,
         `Commercial total` = commercial_total) %>% 
  pivot_longer(!Year, names_to = "Sector", values_to = "KTCO2e") %>% 
  group_by(Year, Sector) %>% 
  summarise(KTCO2e = sum(KTCO2e, na.rm = TRUE))

comm_ind_emissions_tbl %>% 
  ggplot() +
  geom_line(aes(x = Year, y= KTCO2e, colour = Sector))


comm_ind_emissions_tbl %>%
  pivot_wider(id_cols = Year,
              names_from = Sector,
              values_from = KTCO2e) %>% 
  write_csv("data/commercial_industrial_sheet_1_1.csv")

# Transport emissions ----

make_total_props_tbl <- function(tbl) {
  # make a table of total emissions and transport emissions
  # grouped by year
  if(any(unique(tbl$local_authority) %in% "North Somerset")){
    boundary = "West of England (including North Somerset)"
  } else {
    boundary = "West of England Combined Authority"
  }
  
  tbl %>% 
  group_by(calendar_year) %>%
  summarise(
    total_emissions = sum(grand_total),
    total_transport = sum(transport_total),
    total_local_road_transport = sum(road_transport_minor_roads + road_transport_a_roads)) %>%
      mutate(prop_transport = total_transport * 100 / total_emissions,
             prop_local_transport_of_total_transport = total_local_road_transport * 100 / total_transport,
             units = "KTCO2e (territorial GHG)",
             boundary = boundary) 
}


lep_tbl %>% 
  make_total_props_tbl() %>% 
  write_csv("data/transport_emissions_and_props_lep.csv")

woe_tbl %>% 
  make_total_props_tbl() %>% 
  write_csv("data/transport_emissions_and_props_weca.csv")


plot_transport_ts <- function(tbl){

  boundary = unique(tbl$boundary)
  
  core_colours <- c(
    "total_emissions" = "#354753",
    "total_transport" = "#6bd4f2",
    "total_local_road_transport" = "#ed749d",
    "prop_transport" = "#ffd900",
    "prop_local_transport_of_total_transport" = "#79decc")
  
tbl %>% 
  select(-units, -boundary) %>% 
  pivot_longer(cols = !calendar_year, names_to = "Sector", values_to = "value") %>%
  mutate(metric = if_else(str_starts(Sector, "total"), "(GHG) KTCO2e", "Percent")) %>% 
  ggplot(aes(x = calendar_year, y = value,
             colour = Sector,
             group = Sector)) +
  geom_line(lwd = 2) +
  scale_colour_manual(values = core_colours, 
                      labels = c(
    "total_transport" = "Total Transport Emissions",
    "total_emissions" = "Total Emissions",
    "total_local_road_transport" = "Total Local Road Transport Emissions",
    "prop_transport" = "Transport Emissions proportion of Total",
    "prop_local_transport_of_total_transport" = "Local Road Transport Emissions\nproportion of Total Transport")
    ) +
  labs(title = "Greenhouse Gases Territorial Emissions",
       subtitle = boundary,
       caption = "Source: https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021",
       x = "Year",
       y = "Value") +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  theme_clean() +
  theme(plot.caption = element_text(size = 8, hjust = 0.2),
        legend.background = element_rect(colour = "white"),)
  }

woe_tbl %>% 
  make_total_props_tbl() %>% 
  plot_transport_ts() 

lep_tbl %>% 
  make_total_props_tbl() %>% 
  plot_transport_ts() 


lep_tbl %>% 
  mutate()


# Per capita emissions ----
make_per_cap <- function(tbl){
  tbl %>% 
  group_by(calendar_year) %>% 
  summarise(grand_total = sum(grand_total),
            population = sum(population_000s_mid_year_estimate)) %>%
  mutate(per_capita_emissions = grand_total / population) 
}


lep_tbl %>% 
  make_per_cap() %>% 
  inner_join(woe_tbl %>% make_per_cap(),
             by = "calendar_year",
             suffix = c("_lep", "_woe")) %>%
  write_csv("data/per_capita_emissions.csv")











