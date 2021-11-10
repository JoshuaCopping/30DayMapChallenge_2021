### #30DayMapChallenge 2021 ### 
## Day 10, Raster 

# packages 
library(tidyverse)
library(sf)

# National Parks data available from:
# https://geoportal.statistics.gov.uk/datasets/national-parks-december-2019-gb-buc/explore

# load lake district data  
ld_boundary <- st_read(here::here("data", "National_Parks__December_2019__GB_BUC-shp", "National_Parks__December_2019__GB_BUC.shp")) %>% 
  filter(NPARK19NM == "Lake District National Park")

ld_lakes <- st_read(here::here("data", "LD water", "LakeDistrict_lakes.shp")) %>%
  select(wb_name) %>% 
  st_union()

# make grid
grid <- st_make_grid(ld_boundary, 
                     cellsize = 100,
                     what = "centers") %>% 
  st_intersection(ld_boundary)

#calculate distance to lakes
dist_df <- data.frame(distance = as.vector(st_distance(ld_lakes, grid))/1000,
                      st_coordinates(grid))

# plot
lakes_map <- 
  ggplot() +
  geom_raster(data = dist_df,
              aes(x = X, y = Y,
                  fill = distance),
              alpha = 0.9)  +
  geom_sf(data = ld_boundary,
          fill = NA,
          colour = "white",
          size = 1.5) +
  geom_sf(data = ld_lakes,
          fill = "white",
          colour = "grey45",
          size = 0.1)  +
  scale_fill_gradient2(low = "#D2FBD4",
                       mid = "#559C9E",
                       high = "#123F5A",
                       midpoint = 6,
                       breaks = seq(0, 16, by = 4),
                       labels = c("0", "4", "8", "12", "16 km")) +
  labs(title = "Lake District Lakes",
       subtitle = "How far from a lake are you, when in the lake district? This map shows the\ndistance to The Lake District's lakes, meres, reservoirs, tarns & waters",
       caption = "#30DayMapChallenge | Day 10, Raster | Visualisation: Joshua Copping | Data: Office for National Statistics & Ordnance Survey") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey90", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey90", 
                                        colour = NA),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key.width = unit(2.5, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family = "Champagne & Limousines",
                                   size = 20,
                                   colour = "#123F5A"),
        plot.title = element_text(hjust = 0.5,
                                  family = "Champagne & Limousines",
                                  size = 68,
                                  colour = "#123F5A",
                                  margin = margin(30, 0, 0, 0)),
        plot.subtitle = element_text(hjust = 0.5,
                                     family = "Champagne & Limousines",
                                     size = 24,
                                     colour = "#123F5A",
                                     margin = margin(10, 0, 30, 0)), 
        plot.caption = element_text(hjust = 0.5,
                                    size = 11,
                                    colour = "#123F5A",
                                    margin = margin(10, 0, 5, 0)),
        plot.margin = margin(0, 20, 0, 20)) 

ggsave(plot = lakes_map,
       here::here("maps", "Day10.png"),
       width = 12,
       height = 15,
       dpi = 300)
