pacman::p_load(tidyverse,
               openair)

bris_data <- importAURN(site = c("BRS8", "BR11"),
                        pollutant = c("pm10", "pm2.5", "nox"),
                        year = 2023
                        )

bris_data %>% 
  select(date, site, pm10, pm2.5) %>% 
  pivot_longer(cols = c("pm10", "pm2.5"),
               names_to = "Pollutant",
               values_to = "Concentration") %>% 
  filter(date >= as.POSIXct("2023-10-01"),
         !is.na(Concentration)) %>% 
  ggplot(aes(x = date, y = Concentration, colour = Pollutant, group = Pollutant)) +
  geom_line() +
  facet_wrap(~ site)

