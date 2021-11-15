### #30DayMapChallenge 2021 ### 
## Day 13, Natural Earth Data

# packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggfx)

# get Natural Earth country data, calculate area per country and round into the number of points to sample
countries <- ne_download(scale = 10,
                         type = "countries",
                         returnclass = "sf") 

countries_tidy <- countries %>% 
  select(NAME) %>% 
  st_transform(3857) %>% 
  mutate(area = st_area(countries$geometry),
         num_points = as.numeric(ceiling(area/10^10)))

# create random point in each country based - the number of point is based on country size
sampled_points <- st_sample(countries_tidy,
                            size = countries_tidy$num_points,
                            type = "random") %>%
  st_as_sf()

# give the points attribute data - only really need country name
joined_points <- st_join(sampled_points, countries_tidy, left = FALSE) %>% 
  mutate(NAME = case_when(NAME == "United States of America" ~ "USA",
                          TRUE ~ NAME),
         angle = sample(0:360, nrow(sampled_points), rep = TRUE))

# bounding box and graticules
box <- ne_download(scale = 10,
                   category = "physical",
                   type = "wgs84_bounding_box",
                   returnclass = "sf") 

grat <- st_graticule()

# plot
name_plot <- 
  ggplot() +
  geom_sf(data = box,
          fill = NA,
          colour = "grey85",
          size = 0.25) +
  geom_sf(data = grat,
          fill = NA,
          colour = "grey85",
          size = 0.25) +
  with_outer_glow(geom_text(data = joined_points,
                            aes(label = NAME,
                                colour = NAME,
                                angle = angle,
                                geometry = geometry),
                            stat = "sf_coordinates",
                            size = 1,
                            family = "Roboto",
                            hjust = 0),
                  expand = 0, 
                  sigma = 2.5,
                  colour = "grey30") +
  coord_sf(crs = "+proj=robin") +
  scale_size_continuous(range = c(0.25, 1)) +
  scale_colour_viridis_d(option = "turbo") +
  labs(title = "Names Filling the Map",
       subtitle = "An experimental map, using country names placed randomly inside the country's boundaries to shape the map",
       caption = "#30DayMapChallenge | Day 13, Natural Earth Data | Visualisation: Joshua Copping | Data: Natural Earth") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey95",
                                       colour = NA),
        panel.background = element_rect(fill = "grey95",
                                        colour = NA),
        plot.title = element_text(hjust = 0.5,
                                  family = "Poppins Light",
                                  size = 32,
                                  colour = "black",
                                  margin = margin(30, 0, 5, 0)),
        plot.subtitle = element_text(hjust = 0.5,
                                     family = "Poppins Light",
                                     size = 15,
                                     colour = "grey10",
                                     margin = margin(0, 0, 10, 0)),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Poppins",
                                    size = 8,
                                    colour = "grey70",
                                    margin = margin(0, 0, 5, 0)),
        plot.margin = margin(0, 10, 0, 20))

ggsave(plot = name_plot,
       here::here("maps", "Day13.png"),
       width = 18,
       height = 10,
       dpi = 600)