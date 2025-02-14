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

weca_core_colours <- c(
  "Dark Blue" = "#354753",
  "Blue" = "#6bd4f2",
  "Pink" = "#ed749d",
  "Yellow" = "#ffd900",
  "Green" = "#79decc")

# filter for WoE
woe_tbl <- raw_la_tbl %>%
  clean_names() %>% 
  filter(
         local_authority %in% c(
           "Bath and North East Somerset",
           "Bristol, City of",
           "South Gloucestershire",
           "North Somerset"
           ))

woe_las <- c("Bath and North East Somerset",
             "Bristol, City of",
             "South Gloucestershire")

percap_woe_most_recent <- raw_la_tbl %>%
  clean_names() %>% 
  filter(calendar_year == 2021) %>% 
  mutate(woe = local_authority %in% woe_las) %>% 
  select(local_authority, grand_total, woe, population_000s_mid_year_estimate) %>%
  group_by(woe) %>% 

  summarise(per_cap = sum(grand_total) / sum(population_000s_mid_year_estimate)) %>% 
  pivot_wider(names_from = woe, values_from = per_cap) %>%
  mutate(pc_emissions_diff = (`TRUE` - `FALSE`) * 100 / `FALSE` ) %>%
  view()



# tbl for historic emissions
totals_tbl <- woe_tbl %>% 
  group_by(calendar_year) %>% 
  summarise(total_emissions = sum(grand_total)) %>% 
  mutate(source = "Historic")

# exclude the pandemic year from the model source
model_source_tbl <- totals_tbl %>% 
  filter(calendar_year != 2020)

# use 2031 as this is what was used in orig spreadsheet - reporting year
target_year <- 2031
scale_x = seq.int(2005, 2031, 5) # scales for ggplot
last_year <- max(totals_tbl$calendar_year) # last year for historic data
years_vector <- c(last_year, target_year) # vector for future years

last_value <- totals_tbl$total_emissions[which(totals_tbl$calendar_year == last_year)]
# lm for predicting BAU
emissions_model <- lm(total_emissions ~calendar_year, data = model_source_tbl)
# predict emissions at 2030 for BAU
predicted_emissions <- predict(emissions_model,
                               newdata = data.frame(calendar_year = 2030))

emissions_vector = c(last_value, predicted_emissions) # predicted business as usual 2030
# for appending for BAU plot line
xtend_tbl <- tibble(calendar_year = years_vector,
                    total_emissions = emissions_vector, 
                    source = "Predicted BAU")

num_years <- target_year - last_year

years_traj <- last_year:target_year

traj_tbl <- tibble(calendar_year = c(last_year, target_year),
                   total_emissions = c(last_value, 0),
                   source = 'Net zero 2030')

full_base_tbl <- rbind(totals_tbl, xtend_tbl, traj_tbl)

cols <- weca_core_colours[2:4] %>% unname()

yend <- min(xtend_tbl$total_emissions)

plot <- full_base_tbl %>% 
  ggplot(aes(x = calendar_year, y = total_emissions, colour = source)) +
  geom_line(linewidth = 2) +
  scale_colour_manual(values = c(Historic = cols[1],
                                 `Net zero 2030` = cols[2],
                                 `Predicted BAU` = cols[3])) +
  theme_clean() +
  geom_segment(aes(
    x = target_year, y = 0, xend = target_year, yend = yend),
    arrow = arrow(length = unit(0.2, 'cm'), ends = "both", angle = 20),
    lwd = 1,
    colour = weca_core_colours[1],
    lty = 5) +
  geom_text(inherit.aes = FALSE,
    mapping = aes(x = target_year, y = yend / 2),
            nudge_x = 3,
            label = glue("Gap\n{round(yend, 0)} kt")) +
  scale_x_continuous(breaks = scale_x) +
  labs(title = "West of England (including North Somerset) trajectory to net zero",
       subtitle = "All sectors: territorial emissions",
       colour = "Type",
       x = "Year",
       y = quickText("kt CO2e"),
       caption = "DESNZ UK LA Greenhouse gas emissions July 2023") +
  theme(legend.background = element_blank(),
        panel.grid.major = element_line(linewidth = 1),
        axis.text = element_text(size = "18px"),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold", size = "24px"),
        plot.subtitle = element_markdown(lineheight = 1.1)) 

plot

ggsave('plots/WoE_emissions_net_zero.png', plot = plot, device = "png")    
woe_tbl %>% 
  write_csv('data/woe_emissions.csv')
full_base_tbl %>% 
  write_csv('data/full_base_tbl_chart_data.csv')

