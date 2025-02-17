pacman::p_load(tidyverse, ggtext, ggthemes, glue, janitor, openair, duckdb, config, gghighlight)

# doesn't work in windows!
# 
# (md_token = config::get(file = "../config.yml",
#                         config = "motherduck",
#                         value = "token"))
# 
# (attach_str <- glue("ATTACH 'md:mca_data:motherduck_token={md_token}' AS mca_data"))

# con = dbConnect(duckdb::duckdb())
# dbExecute(con, "INSTALL 'motherduck'")
# dbExecute(con, "LOAD 'motherduck'")
# dbExecute(con, attach_str)
# dbExecute(con, "USE mca_data")


# data created in per-cap-emissions-ca.py

percap_tbl <- read_csv("data/unified_percap.csv")

percap_bar_plot_facet <- percap_tbl %>% 
  mutate(Year = as.integer(calendar_year)) |> 
  ggplot(aes(x = Year, y = per_cap)) +
  geom_col() +
  facet_wrap(~ area, ncol = 3) + 
  labs(title = "Per Capita Emissions for Combined Authority Areas",
       subtitle = quickText("Emissions in tonnes of CO2 equivalent per person"),
       x = "Year",
       y = quickText("tCO2e\nPer cap")) +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))


percap_bar_plot_facet


percap_tbl %>%
  filter(area == "West of England" | area == "National") %>% 
  filter(calendar_year == min(calendar_year) | calendar_year == max(calendar_year)) %>%
  pivot_wider(id_cols = area, 
              names_from = calendar_year, 
              values_from = per_cap, names_prefix = "year_") |>
  mutate(change = -round((year_2005 - year_2022) * 100 / year_2005)) |> 
  glimpse() 


percap_tbl |> 
  filter(area != "Tees Valley") |> 
  group_by(area) |> 
  ggplot(aes(x = calendar_year, y = per_cap, colour = area)) +
  geom_line(linewidth = 1) +
  gghighlight(area == "West of England" | area == "National",
              # keep_scales = TRUE,
              # use_direct_label = TRUE,
              unhighlighted_params = list(colour = NULL, alpha = 0.2)
              ) +
  labs(title = "Per Capita Emissions for Combined Authorities",
       subtitle = quickText("Emissions in tonnes of CO2 equivalent per person"),
       x = "Year",
       y = quickText("tCO2e\nPer cap"),
       caption = "Tees Valley omitted for visibility") +
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

gghighlight

  