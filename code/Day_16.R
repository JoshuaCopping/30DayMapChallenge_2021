### #30DayMapChallenge 2021 ### 
## Day 16, Urban/Rural

# load packages
library(tidyverse)
library(sf)
library(raster)
library(hddtools)
library(paletteer)

# loads london building
buildings <- st_read(here::here("data", "LondonBuildingHeights", "LondonBuildingHeights.shp")) 

# get extent shapefile
ldn_bbox <- extent(-0.291951, 0.11538, 51.387402, 51.588277)
ldn_sp <- bboxSpatialPolygon(ldn_bbox, 
                             proj4stringFrom = NULL,
                             proj4stringTo = NULL)

ldn_sf <- st_as_sf(ldn_sp) %>% 
  st_transform(27700)

# crop whole london dataset to central area
buildings_crop <- st_intersection(st_make_valid(buildings), ldn_sf)

# plot 
ldn_height <- 
  ggplot() +
  geom_sf(data = buildings_crop,
          aes(fill = MAX_max),
          colour = NA) +
  annotate("text",
           x = 531167.5, y = 173021,
           label = "#30DayMapChallenge | Day 16, Urban | Visualisation: Joshua Copping | Data: Emu Analytics & Ordnance Survey",
           hjust = 0.5, 
           vjust = 0,
           colour = "grey90",
           family = "Louis George Cafe",
           size = 3.6) +
  scale_fill_paletteer_c(`"pals::cubicyf"`,
                         name = "Height (m) of Buildings in London",
                         limits = c(0, 300),
                         breaks = seq(0, 300, by = 50)) +
  scale_x_continuous(limits = c(520918, 541687),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(172851, 185922),
                     expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey10", 
                                       colour = NA,),
        panel.background = element_rect(fill = "grey10", 
                                        colour = NA),
        legend.position = c(0.5, 0.095),
        legend.key.width = unit(2.6, "cm"),
        legend.title = element_text(family  = "Louis George Cafe",
                                    face = "bold",
                                    colour = "grey90",
                                    size = 24,
                                    hjust = 0.5),
        legend.text = element_text(family  = "Louis George Cafe",
                                   colour = "grey90",
                                   size = 14)) +
  guides(fill = guide_colourbar(direction = "horizontal",
                                title.position = "top",
                                label.position = "bottom"))

ggsave(plot = ldn_height,
       here::here("maps", "Day16.png"),
       width = 15.88937,
       height = 10,
       dpi = 300)
