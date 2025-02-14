pacman::p_load(tidyverse,
               readxl,
               glue,
               janitor,
               ggthemes,
               openair,
               scales)
# https://www.gov.uk/government/statistics/uk-local-authority-and-regional-greenhouse-gas-emissions-national-statistics-2005-to-2021
raw_la_tbl <- read_xlsx(
  'data/2005-21-uk-local-authority-ghg-emissions-update-060723.xlsx',
  sheet = '1_2', range = 'A5:AX7111')

raw_wages_tbl <- read_xlsx(
  'data/Real Wages-Earnings vs Inflation.xlsx',
  sheet = 'Chart - CA + LEP', range = 'A25:B40') %>% 
  clean_names()

weca_core_colours <- c(
  "Dark Blue" = "#354753",
  "Blue" = "#6bd4f2",
  "Pink" = "#ed749d",
  "Yellow" = "#ffd900",
  "Green" = "#79decc")

cols <- weca_core_colours[2:4] %>% unname()
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

# tbl for historic emissions
totals_tbl <- woe_tbl %>% 
  group_by(calendar_year) %>% 
  summarise(total_emissions = sum(grand_total)) %>% 
  mutate(source = "Historic")

scale_x = seq.int(2005, 2050, 5) # scales for ggplot
save_gg_a3 <- function(ggplot, path){
  
  ggsave(path,
         plot = ggplot,
         device = "png",
         height = 29.7, #A3
         width = 42,
         units = 'cm',
         bg = "white")    
}


nicetheme <- function(){
  
    theme(legend.background = element_blank(),
          panel.grid.major.x = element_line(linewidth = 1),
          panel.grid.major.y = element_line(linewidth = 1),
          axis.text = element_text(size = "25", face = 'bold'),
          axis.title = element_text(face = "bold", size = "25"),
          axis.title.y = element_text(face = "bold", size = "25"),
          plot.subtitle = element_text(size = 20),
          plot.title = element_text(size = 38))
}

plot <- totals_tbl %>% 
  ggplot(aes(x = calendar_year, y = total_emissions)) +
  geom_line(linewidth = 4, colour = cols[1]) +
  theme_minimal() +
  scale_x_continuous(breaks = scale_x) +
  labs(title = quickText("When will the West of England meet net zero (CO2 emissions)?"),
       subtitle = "Place your sticker on the year!",
       colour = NULL,
       x = "Year",
       y = quickText("CO2e (kt)")) +
  nicetheme() +
  expand_limits(y = c(0, 8000),
                x = c(2005, 2050))

plot

wages_scale_x <- seq.int(2010, 2040, 5)

wages_scale_x
plot_wages <- raw_wages_tbl %>% 
  ggplot(aes(x = date, y = west_of_england_ca)) +
  geom_line(linewidth = 4, colour = cols[1]) +
  theme_minimal() +
  scale_x_continuous(breaks = wages_scale_x) +
  scale_y_continuous(labels = label_number(scale = 1e-3, prefix = "£", suffix = "K")) +
  labs(title = "What can we expect real wages in our region to be in 2040, in 2022 prices?",
       subtitle = "Place your sticker on the wage in 2040!",
       colour = NULL,
       x = "Year",
       y = "Wages in equivalent 2022 prices") +
  nicetheme() +
  theme(plot.title = element_text(size = 30))+
  expand_limits(y = c(0, 50000), x = wages_scale_x)

plot_wages



plot_wages %>% 
  save_gg_a3("plots/wages_chart_0-50k.png")




