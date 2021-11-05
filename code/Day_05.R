### #30DayMapChallenge 2021 ### 
## Day 5, Data Challenge 1: OpenStreetMap

# packages
library(tidyverse)
library(sf)
library(osmdata)
library(ggtext)

# create bounding box 
x <- c(-1.123524, -1.039341)
y <- c(53.93383, 53.982739)

york_bbox <- rbind(x,y) 
colnames(york_bbox) <- c("min", "max")

# get osm data 
historic <- york_bbox %>%
  opq() %>%
  add_osm_feature(key = "historic", 
                  value = c("citywalls", "city_gate", "church",  "building", "castle", "ruins")) %>%
  osmdata_sf()

religious <- york_bbox %>%
  opq() %>%
  add_osm_feature(key = "building", 
                  value = c("cathedral", "chapel", "church")) %>%
  osmdata_sf()

buildings <- york_bbox %>%
  opq() %>%
  add_osm_feature(key = "building", 
                  value = c("apartments", "bungalow", "detached", "dormitory", "hotel", "house", "residential", "semidetached_house", "terrace", "commercial", "industrial", "kiosk", "office", "retail",  "supermarket", "warehouse", "mosque", "religious", "synagogue", "temple", "bakehouse", "civic", "fire_station", "government", "hospital", "public", "train_station", "toilets", "transportation", "school", "university", "college", "pavilion", "sports_hall", "garage", "garages", "parking", "service", "gatehouse")) %>%
  osmdata_sf()

natural_water <- york_bbox %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() 

big_roads <- york_bbox %>%
  opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("primary", "secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()

small_roads <- york_bbox %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>%
  osmdata_sf()

railways <- york_bbox %>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf() 

barriers <- york_bbox %>%
  opq()%>%
  add_osm_feature(key = "barrier", 
                  value = c("fence", "wall")) %>%
  osmdata_sf()

# circle for cropping
circle <- tibble(lat = 53.958332, 
                 long = -1.080278) %>% 
  st_as_sf(coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(crs = 27700) %>% 
  st_buffer(dist = 2000) 

# crop data
historic_line_crop <- historic$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

historic_poly_crop <- historic$osm_polygons %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

religious_crop <- religious$osm_polygons %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

buildings_crop <- buildings$osm_polygons %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

natural_water_crop <- natural_water$osm_polygons %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

big_roads_crop <- big_roads$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

small_roads_crop <- small_roads$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

railways_crop <- railways$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

barriers_crop <- barriers$osm_lines %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(circle)

# plot 
york_plot <- 
  ggplot() +
  geom_sf(data = natural_water_crop,
          colour = "grey90",
          fill = "grey90") +
  geom_sf(data = railways_crop,
          colour = "grey60",
          size = 0.4) +
  geom_sf(data = small_roads_crop,
          colour = "grey60",
          size = 0.4) +
  geom_sf(data = big_roads_crop,
          colour = "grey60",
          size = 0.9) +
  geom_sf(data = barriers_crop,
          colour = "grey85",
          fill = "grey85",
          size = 0.05) +
  geom_sf(data = buildings_crop,
          colour = "grey80",
          fill = "grey80",
          size = 0.1) +
  geom_sf(data = religious_crop,
          colour = "#AB2330",
          fill = "#AB2330",
          size = 0.1) +
  geom_sf(data = historic_line_crop,
          colour = "#AB2330",
          size = 2) +
  geom_sf(data = historic_poly_crop,
          colour = "#AB2330",
          fill = "#AB2330",
          size = 0.1) +
  geom_sf(data = circle,
          colour = "grey60",
          fill = NA,
          size = 0.5) + 
  coord_sf(clip = "off") +
  annotate("richtext",
           x = 460446, y = 449550, 
           label = paste0("<span style = 'font-size: 84px'>The Historic City of York</span><br><span style = 'font-size: 60px'>53° 57' 29",'"'," N / 01° 04' 49",'"'," W</span>"),
           family = "Fogtwono5",
           colour = "#AB2330",
           lineheight = 4,
           hjust = 0.5,
           vjust = 1,
           label.colour = NA,
           fill = NA) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(448000, 453800),
                     expand = c(0, 0)) +
  labs(caption = "#30DayMapChallenge | Day 5, OpenStreetMap Data | Visualisation: Joshua Copping | Data: OpenStreetMap") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", 
                                       colour = NA),
        panel.background = element_rect(fill = "white", 
                                        colour = NA),
        plot.margin = margin(80, 30, 0, 30),
        plot.caption = element_text(hjust = 0.5,
                                    size = 14,
                                    colour = "grey60",
                                    family = "Roboto",
                                    margin = margin(0, 0, 5, 0))) 

ggsave(plot = york_plot,
       here::here("maps", "Day05.png"),
       width = 12,
       height = 15,
       dpi = 300)
