### #30DayMapChallenge 2021 ### 
## Day 1, Points 

# packages 
library(tidyverse)
library(sf)
library(WDI)
library(countrycode)
library(rnaturalearth)
library(ggfx)
library(paletteer)

# get data
protected_areas <- WDI(indicator = "ER.LND.PTLD.ZS", 
                       start = 2018, 
                       end = 2018) %>% 
  mutate(ISO3 = countrycode(iso2c, 
                            origin = "iso2c", 
                            destination = "iso3c")) %>% 
  rename("Area" = "ER.LND.PTLD.ZS") %>%
  drop_na() %>% 
  mutate(Bins = cut(Area, 
                    breaks = c(-Inf, 10, 20, 30, 40, 50, Inf)),
         Area_cat = case_when(Bins == "(-Inf,10]" ~ "A",
                              Bins == "(10,20]" ~ "B",
                              Bins == "(20,30]" ~ "C",
                              Bins == "(30,40]" ~ "D",
                              Bins == "(40,50]" ~ "E",
                              Bins == "(50, Inf]" ~ "F"))

ocean <- ne_download(scale = 10, 
                     category = "physical",
                     type = "ocean",
                     returnclass = "sf") %>% 
  st_transform(crs = "+proj=robin")

countries <- ne_download(scale = 10,
                         type = "countries",
                         returnclass = "sf") %>% 
  dplyr::select("ISO_A3", "NAME_EN", "CONTINENT") %>% 
  st_transform(crs = "+proj=robin") 

# join data and country points
world_points <- countries %>% 
  st_point_on_surface() %>% 
  left_join(protected_areas, by = c("ISO_A3" = "ISO3")) %>% 
  drop_na(Area) %>% 
  mutate(CONTINENT = case_when(ISO_A3 == "SYC" ~ "Africa",
                               ISO_A3 == "MUS" ~ "Africa",
                               ISO_A3 == "MDV" ~ "Asia",
                               ISO_A3 == "RUS" ~ "Asia",
                               TRUE ~ CONTINENT))

# plot 
world_plot <- 
  ggplot() +
  geom_sf(data = ocean,
          fill = "#F1F5F5",
          colour = NA) +
  with_shadow(geom_sf(data = countries,
                      fill = "#FAF7E2",
                      colour = "#EDEAD5",
                      size = 0.2),
              x_offset = 0,
              y_offset = 0,
              sigma = 20,
              colour = "#689BC4") +
  geom_sf(data = world_points,
          aes(size = Area_cat,
              colour = CONTINENT),
          alpha = 0.9) +
  scale_size_manual(values = c(1,3,5,7,9,12),
                    labels = c("Less than 10%", "10-20%", "20-30%", "30-40%", "40-50%", "More than 50%")) +
  scale_colour_paletteer_d(`"awtools::spalette"`,
                           guide = "none") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf() + 
  labs(title = "Earth's Protected Areas",
       subtitle = "Terrestrial protected areas as a percentage of total land area, per country",
       caption = "#30DayMapChallenge | Day 1, Points | Visualisation: Joshua Copping | Data: Protected Planet") +
  guides(size = guide_legend(override.aes = list(colour = "grey25"),
                             title = NULL,
                             nrow = 1),
         colour = "none") +
  theme_void() +
  theme(legend.position = "top",
        legend.text = element_text(size = 14,
                                   colour = "grey25",
                                   family = "Chapaza"),
        legend.margin = margin(10, 0, 0, 0),
        plot.background = element_rect(fill = "white", 
                                       colour = NA),
        panel.background = element_rect(fill = "white", 
                                        colour = NA),
        plot.title = element_text(hjust = 0.5,
                                  family = "Chapaza",
                                  size = 58,
                                  colour = "grey25",
                                  margin = margin(15, 0, 0, 0)),
        plot.subtitle = element_text(hjust = 0.5, 
                                     family = "Chapaza",
                                     size = 18, 
                                     colour = "grey25",
                                     margin = margin(0, 0, 0, 0)),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    size = 11,
                                    colour = "grey25",
                                    margin = margin(25, 0, 0, 0)),
        plot.margin = margin(0, 0, 0, 0)) 

# save
ggsave(plot = world_plot,
       here::here("maps", "Day_01.png"),
       width = 18,
       height = 11,
       dpi = 300)
