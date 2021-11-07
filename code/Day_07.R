### #30DayMapChallenge 2021 ### 
## Day 7, Green

# packages
library(tidyverse)
library(sf)
library(osmdata)
library(rmapshaper)
library(ggtext)

# load tree data, available from:
# https://www.london.gov.uk/what-we-do/environment/parks-green-spaces-and-biodiversity/trees-and-woodlands/london-tree-map

trees <- read_csv(here::here("data", "LondonTrees.csv")) %>% 
  st_as_sf(coords = c("lon", "lat"))%>% 
  st_set_crs(4326) %>% 
  st_transform(27700)

# load london outline
london_outline <- st_read(here::here("LondonBoundaries", "ESRI", "London_Ward_CityMerged.shp")) %>%
  st_union() %>% 
  st_transform(27700)

# london bbox
x <- c(-0.510375, 0.334015)
y <- c(51.28676, 51.691874)

london_bbox <- rbind(x, y) 
colnames(london_bbox) <- c("min", "max")

# get OSM data
natural_water <- london_bbox %>%
  opq() %>%
  add_osm_feature(key = "natural", 
                  value = "water") %>%
  osmdata_sf() 

rivers <- london_bbox %>%
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "riverbank")) %>%
  osmdata_sf() 

big_roads <- london_bbox %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary","secondary", "tertiary","trunk")) %>%
  osmdata_sf()

medium_roads <- london_bbox %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("secondary_link", "tertiary_link")) %>%
  osmdata_sf()

railways <- london_bbox %>%
  opq() %>%
  add_osm_feature(key = "railway", value = "rail") %>%
  osmdata_sf()

# select and crop layers
rivers_p <- rivers$osm_polygons %>%
  select(osm_id)

natural_water_p <- natural_water$osm_polygons %>%
  select(osm_id)

natural_water_mp <- natural_water$osm_multipolygons %>%
  select(osm_id)

water <- rbind(rivers_p, natural_water_p, natural_water_mp) %>% 
  st_transform(27700) %>%
  ms_filter_islands(min_area = 10000) %>% 
  st_intersection(london_outline)

big_roads_crop <- big_roads$osm_lines %>%
  select(osm_id) %>% 
  st_transform(27700) %>%
  st_intersection(london_outline)

medium_roads_crop <- medium_roads$osm_lines %>%
  select(osm_id) %>% 
  st_transform(27700) %>%
  st_intersection(london_outline)

railways_crop <- railways$osm_lines %>%
  select(osm_id) %>% 
  st_transform(27700) %>%
  st_intersection(london_outline)

# plot
title <- "<span style = 'font-size:108px'>London,</span><br><span style = 'font-size: 64px'>Urban Forest?</span>"
text <- "The United Nations definition of a forest<br>is any area with at least 10 percent tree<br>canopy cover. London has roughly 8.4<br>million trees, almost one per resident.<br>This equates to around 21 percent<br>canopy cover, and there is the goal to<br>increase this number further, to 23<br>percent by 2050. This officially makes <br>London an urban forest.<br><br>The London Tree Map dataset provides<br>species and location information for<br>over 880,000 of London's trees. The<br>most common are cherry trees, with<br>over 120,000 recorded, covering around<br>15 percent of the canopy. Trees in the<br>dataset are primarily street trees, not<br>those in London’s woodlands, parks, or<br>private gardens, and have been<br>recorded in only 26 of the 33 boroughs.<br><br>The map on the right, using the London<br>Tree Map data, shows just how green<br>the capital appears, even with just<br>10 percent of all trees in the city<br>currently mapped."

tree_plot <- ggplot() +
  geom_sf(data = water,
          colour = "grey80",
          fill = "grey80") +
  geom_sf(data = railways_crop,
          colour = "grey30",
          size = 0.1,
          alpha = 0.9) +
  geom_sf(data = medium_roads_crop,
          colour = "grey25",
          size = 0.2,
          alpha = 0.9) +
  geom_sf(data = big_roads_crop,
          colour = "grey25",
          size = 0.4,
          alpha = 0.9) +
  geom_sf(data = trees,
          shape = 16,
          size = 0.001,
          alpha = 0.5,
          colour = "#2C8225") +
  geom_sf(data = london_outline,
          size = 1, 
          colour = "#2C8225",
          fill = NA) +
  annotate("richtext",
           x = 480000, y = 200500,
           label = title,
           hjust = 0,
           vjust = 1,
           family = "Mermaid",
           colour = "#2C8225",
           label.colour = NA,
           fill = NA) +
  annotate("richtext",
           x = 480000, y = 191000,
           label = text,
           hjust = 0,
           vjust = 1,
           family = "Roboto Medium",
           colour = "grey40",
           size = 5.5,
           label.colour = NA,
           fill = NA) +
  coord_sf() +
  scale_x_continuous(limits = c(475000, 568000),
                     expand = c(0, 0)) +
  labs(caption = "#30DayMapChallenge | Day 7, Green | Visualisation: Joshua Copping | Data: Greater London Authority & OpenStreetMap") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey95", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey95", 
                                        colour = NA),
        plot.caption = element_text(family = "Roboto",
                                    size = 11,
                                    colour = "grey40",
                                    hjust = 0.5,
                                    margin = margin(15, 0, 0, 0))) 

ggsave(plot = tree_plot,
       here::here("maps", "Day07.png"),
       width = 18,
       height = 10.2,
       dpi = 300)
