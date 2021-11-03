### #30DayMapChallenge 2021 ### 
## Day 3, Polygons 

# packages
library(tidyverse)
library(sf)
library(rgdal)
library(terra)
library(ggtext)

# load and tidy data
# National Parks data available from:
# https://geoportal.statistics.gov.uk/datasets/national-parks-december-2019-gb-buc/explore

national_parks <- st_read("data", "National_Parks__December_2019__GB_BUC-shp", "National_Parks__December_2019__GB_BUC.shp")
uk <- st_read(here::here("data", "gadm36_GBR_shp", "gadm36_GBR_1.shp"))

np_centres <- st_point_on_surface(national_parks) %>% 
  dplyr::select(NPARK19NM) 

gbr <- uk %>% 
  st_transform(27700) %>%
  filter(NAME_1 != "Northern Ireland") %>% 
  dplyr::select(NAME_1)

# convert to SpatialPoints then Terra Vect to use terra::voronoi
npc_sp <- as(np_centres, "Spatial")
gbr_sp <- as(gbr, "Spatial")

npc_v <- vect(npc_sp)
gbr_v <- vect(gbr_sp)

vor <- voronoi(npc_v, gbr_v)
vor_gbr <- crop(vor, gbr_v)

# randomly assing numbers to change plotting colour/order of polygons
set.seed(1)
vor_nps <- st_as_sf(vor_gbr) %>% 
  mutate(number = as.factor(sample(15)))

# plot 
poly_plot <-
  ggplot() +
  geom_sf(data = vor_nps, 
          aes(fill = number),
          colour = NA) +
  geom_sf(data = national_parks,
          colour = "grey90",
          fill = "white",
          alpha = 0.2,
          size = 0.3) +
  geom_sf(data = np_centres,
          colour = "grey95") +
  coord_sf(xlim = c(0, 650000),
           ylim = c(0, 1200000),
           clip = "off") +
  scale_fill_viridis_d(begin = 0.14, end = 0.72, direction = -1) +
  annotate("richtext", 
           x = 440000, y =  720000, 
           label = "<b><span style = 'font-size: 24px;'>BRITAIN'S NATIONAL PARKS</span></b><br><br><span style = 'font-size: 14.5px'>Voronoi polygons showing the areas for which<br>National Park is closest throughout Great Britain,<br>based on the centre point of each park.", 
           family = "Roboto",
           colour = "grey25",
           lineheight = 1,
           hjust = 0,
           label.colour = NA,
           fill = NA) +
  labs(caption = "#30DayMapChallenge | Day 3, Polygons | Visualisation: Joshua Copping | Data: Office for National Statistics") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "grey90", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey90", 
                                        colour = NA),
        plot.caption = element_text(family = "Roboto",
                                    size = 8,
                                    colour = "grey25",
                                    hjust = -20,
                                    margin = margin(0, 0, 5, 0)),
        plot.margin = margin(10, 100, 0, 0)) 

# save
ggsave(plot = poly_plot,
       here::here("maps", "Day03.png"),
       width = 8,
       height = 10,
       dpi = 300)
