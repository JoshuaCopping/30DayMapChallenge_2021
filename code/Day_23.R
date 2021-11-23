### #30DayMapChallenge 2021 ### 
## Day 23, GHSL data

# packages 
library(raster)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggfx)

# load population raster
population <- raster(here("data", "GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0", "GHS_POP_E2015_GLOBE_R2019A_54009_1K_V1_0.tif"))

# bounding box
bbox <- extent(-40.3, 45.2, 19, 71.8)
bbox_sp <- hddtools::bboxSpatialPolygon(bbox)
bbox_sf <- st_as_sf(bbox_sp)

# get countries shapefile for clipping
countries <- ne_download(scale = 50,
                         type = "countries",
                         returnclass = "sf") %>% 
  st_intersection(bbox_sf) %>% 
  filter(!ADMIN %in% c("Greenland")) %>%
  st_transform(crs = "+proj=moll") 

# clip/mask raster
pop_tidy <- crop(population, countries)
pop_tidy <- mask(pop_tidy, countries)
pop_tidy <- projectRaster(pop_tidy, crs = 3857)

# transform countries to mercator 
countries_prj <- st_transform(countries, 3857)

# make grid 
pop_grid <- st_make_grid(countries_prj,
                         cellsize = 20000,
                         what = 'polygons',
                         square = FALSE,
                         flat_topped = TRUE) %>% 
  st_as_sf() 

# extract raster values 
pop_grid2 <- pop_grid %>% 
  mutate(population = raster::extract(pop_tidy, pop_grid, fun = sum, na.rm = TRUE)) 

# clip to europe
pop_grid3 <- pop_grid2 %>%
  st_intersection(countries_prj) %>% 
  dplyr::select(population) %>% 
  mutate(id = row_number())

# calculate number of dots per polygon
num_dots <- pop_grid3 %>% 
  mutate(number = round(population/10000)) %>%
  drop_na(number)

# creates points randomly in polygons
sf_dots <- st_sample(num_dots, 
                     size = num_dots$number, 
                     type = "random") 

# plot 
night_map <- 
  ggplot() +
  geom_sf(data = countries_prj,
          colour = NA,
          fill = "#11001F") +
  geom_sf(data = sf_dots,
          colour = "#FFF563",
          shape = 16,
          size = 0.01,
          alpha = 0.75) +
  geom_text(aes(x = -6400000, y = 3450000,
                label = "#30DayMapChallenge | Day 23, GHSL Data | Visualisation: Joshua Copping | Data: European Commission"),
            hjust = 0,
            family = "Roboto",
            size = 7.2,
            colour = "grey30") +
  with_outer_glow(geom_text(aes(x = -5800000, y = 8200000, 
                                label = "Europe At Night"),
                            family =  "Banks Miles Single Line",
                            size = 34,
                            colour = "#FFF563",
                            hjust = 0),
                  expand = 2,
                  sigma = 30,
                  colour = "#FFF563") +
  with_outer_glow(geom_text(aes(x = -5800000, y = 7800000, 
                                label = "1 dot represents 10,000 people"),
                            family =  "Banks Miles Single Line",
                            size = 17,
                            colour = "#FFF569",
                            hjust = 0), 
                  expand = 1,
                  sigma = 20,
                  colour = "#FFF569") +
  coord_sf(expand = FALSE,
           xlim = c(-6500000, 4800000),
           ylim = c(3350000, 11700000)) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "black", 
                                       colour = NA),
        panel.background = element_rect(fill = "black", 
                                        colour = NA),
        plot.margin = margin(0, 0, 0, 0))

ggsave(plot = night_map,
       here("maps", "Day23.png"),
       width = 30,
       height = 22.16815,
       dpi = 300)

