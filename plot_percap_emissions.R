pacman::p_load(tidyverse, ggtext, ggthemes, glue, janitor, openair)

# data created in motherduck.py

percap_tbl <- read_csv("data/unified_percap.csv")

percap_tbl %>% 
  mutate(Year = as.integer(calendar_year)) |> 
  ggplot(aes(x = Year, y = per_cap)) +
  geom_col() +
  facet_wrap(~ area) + 
  theme_minimal() +
  labs(title = "Per capita emissions for Combined Authority areas",
       subtitle = quickText("Emissions in tonnes of CO2 equivalent per person"),
       x = "Year",
       y = quickText("tCO2e pp"))
