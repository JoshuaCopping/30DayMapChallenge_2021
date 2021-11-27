### #30DayMapChallenge 2021 ### 
## Day 27, Heatmap

# packages
library(raster)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(paletteer)
library(ggfx)
library(patchwork)

# load lightning data 
lightning <- brick(here("data", "WGLC", "05m", "wglc_climatology_05m.nc"), varname = "density") # source: https://zenodo.org/record/4882792#.YXJyZ9nMI-Q

# create 1 layer from raster brick
lightning_total <- calc(lightning, fun = sum)

# assign NA to areas with no lightning strikes
lightning_update <- lightning_total
lightning_update[lightning_total[] == 0] <- NA

# project data
lightning_update <- projectRaster(lightning_update, crs = 3857)

# download countries
countries <- ne_download(scale = 10,
                         type = "countries",
                         returnclass = "sf") %>% 
  select("ISO_A3", "NAME_EN", "CONTINENT") %>% 
  filter(CONTINENT != "Antarctica") %>% 
  st_transform(crs = 3857) 

countries_union <- st_union(countries)

# create hexagon grid
world_hex <- st_make_grid(countries_union,
                          cellsize = 250000,
                          what = 'polygons',
                          square = FALSE,
                          flat_topped = FALSE) %>% 
  st_as_sf() 

# extract raster values to hex cells - using mean value
world_hex_lightning <- world_hex %>% 
  mutate(lightning_density = raster::extract(lightning_update, world_hex, fun = mean, na.rm = TRUE))

# create centre points in hexagons with lightning data
world_points <- world_hex_lightning %>% 
  st_centroid()

# mask to remove sea and, bot hexagons and points
world_hex_lightning_mask <- st_intersection(world_hex_lightning, countries_union)
world_points_mask <- st_intersection(world_points, countries_union)

# plot
lightning_map <- 
  ggplot() +
  geom_sf(data = world_hex_lightning_mask,
          fill = "grey8",
          size = 0.01,
          colour = "grey45") +
  with_outer_glow(geom_sf(data = world_points_mask,
                          shape = 16,
                          aes(size = lightning_density,
                              colour = lightning_density,
                              alpha = lightning_density)),
                  sigma = 20,
                  expand = 7.5,
                  colour = "#01EFFF") +
  scale_size_continuous(range = c(0.25, 3.2)) +
  scale_colour_paletteer_c(`"pals::ocean.ice"`,
                           trans = "log") +
  scale_alpha_continuous(range = c(0.25, 1),
                         trans = "log") +
  coord_sf(expand = FALSE,
           xlim = c(-18750000, 20040000),
           ylim = c(-8800000, 15450000)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "black", 
                                       colour = NA),
        panel.background = element_rect(fill = "black", 
                                        colour = NA),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Adam Bold",
                                    colour = "grey30",
                                    size = 12,
                                    margin = margin(20, 0, 5, 0))) +
  annotate("text",
           x = 645000, y = -8400000,
           label = "#30DayMapChallenge | Day 27, Heatmap | Visualisation: Joshua Copping | Data: Kaplan & Lau, 2021",
           hjust = 0.5,
           family = "Adam Bold",
           colour = "grey30",
           size = 4.5) 

title_plot <- 
  ggplot() +
  labs(title = "Lightning Strikes",
       subtitle = "Heatmap showing mean global\nlightning strike density on land,\nbetween 2010 & 2020. Bigger,\nbrighter points indicate greater\ndensity of lightning strikes") +
  theme(plot.background = element_rect(fill = "black", 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_text(family =  "Adam Bold",
                                  size = 32,
                                  colour = "grey90",
                                  margin = margin(0, 0, 8, 0)),
        plot.subtitle = element_text(family = "Adam Medium",
                                     colour = "grey90",
                                     size = 16.6,
                                     lineheight = 0.9))

final_lightning_map <- lightning_map +
  inset_element(title_plot, 0.04, 0.2, 0.25, 0.3) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) 

ggsave(plot = final_lightning_map,
       here("maps", "Day27.png"),
       width = 19.19506,
       height = 12,
       dpi = 300)
