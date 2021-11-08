### #30DayMapChallenge 2021 ### 
## Day 8, Blue

# packages
library(tidyverse)
library(raster)
library(terra)
library(sf)
library(paletteer)
library(ggfx)
library(patchwork)

# download data 
bio_tile1 <- getData("worldclim", var = "bio", res = 0.5, lon = -5, lat = 70, path = here::here("data"))
bio_tile2 <- getData("worldclim", var = "bio", res = 0.5, lon = -5, lat = 60, path = here::here("data"))
bio_tile3 <- getData("worldclim", var = "bio", res = 0.5, lon = 0, lat = 60, path = here::here("data"))

uk_outline <- st_read(here::here("data", "gadm36_GBR_shp", "gadm36_GBR_0.shp"))
irl_outline <- st_read(here::here("data", "gadm36_IRL_shp", "gadm36_IRL_0.shp")) %>%
  st_transform(27700)

# select precipitation data
precipitation_tile1 <- bio_tile1[[12]]
precipitation_tile2 <- bio_tile2[[12]]
precipitation_tile3 <- bio_tile3[[12]]

# merge raster data
precipitation_merge <- mosaic(precipitation_tile1, precipitation_tile2, precipitation_tile3, fun = mean)

rm(bio_tile1, bio_tile2, bio_tile3, precipitation_tile1, precipitation_tile2, precipitation_tile3)

# crop, increase resolution, mask & project precipitation data
precipitation_uk <- terra::crop(precipitation_merge, uk_outline)
precipitation_uk <- terra::mask(precipitation_uk, uk_outline)
precipitation_uk <- disaggregate(precipitation_uk, fact = 4, method = "bilinear")
precipitation_uk <- projectRaster(precipitation_uk, crs = 27700)

precipitation_uk_df <- as.data.frame(precipitation_uk, xy = TRUE) %>% 
  rename("precip" = "layer") %>%
  drop_na(precip) 

uk_outline_sf <- uk_outline %>%
  st_as_sf() %>%
  st_transform(27700)

# plot
blue_plot <- 
  ggplot() +
  with_shadow(geom_sf(data = uk_outline_sf,
                      size = 0.1,
                      colour = NA,
                      fill = "grey85"),
              x_offset = 0,
              y_offset = 0,
              sigma = 12,
              colour = "#14142C") +
  with_shadow(geom_sf(data = irl_outline,
                      size = 0.1,
                      colour = NA,
                      fill = "grey85"),
              x_offset = 0,
              y_offset = 0,
              sigma = 12,
              colour = "#14142C") +
  geom_tile(data = precipitation_uk_df,
            aes(x = x, y = y,
                fill = precip)) +
  coord_sf(clip = "off") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_paletteer_c(`"pals::ocean.ice"`, direction = -1,
                         breaks = c(1000, 1500, 2000),
                         labels = scales::comma) +
  labs(caption = "#30DayMapChallenge | Day 8, Blue | Visualisation: Joshua Copping | Data: WorldClim",
       fill = "Average Annual\nPrecipitation (mm)") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#D2D8DC", 
                                       colour = NA),
        panel.background = element_rect(fill = "#D2D8DC", 
                                        colour = NA),
        plot.caption = element_text(family = "Roboto",
                                    size = 8,
                                    colour = "grey25",
                                    hjust = 0.65,
                                    margin = margin(20, 0, 5, 0)),
        plot.caption.position = "plot",
        plot.margin = margin(30, 70, 0, 0),
        legend.position = c(0.95, 0.62),
        legend.text = element_text(family = "Poppins",
                                   colour = "grey25",
                                   size = 12),
        legend.title = element_text(family = "Poppins",
                                    colour = "grey25",
                                    size = 12)) 

title_plot <- ggplot() +
  labs(title = "Do You Need an Umbrella?",
       subtitle = "Average annual precipitation in the United Kingdom,\nranges from 552 mm in East Anglia, to 2,293 mm in\nthe northwest of Scotland. If you live in the latter,\nyou’ll need a coat, umbrella, and wellies a lot\nmore than those living in southeast England.") +
  theme(plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_text(family = "Poppins SemiBold",
                                  size = 26,
                                  colour = "#333D8F"),
        plot.subtitle = element_text(family = "Poppins",
                                     size = 12.5,
                                     colour = "grey25"))

blue_final <- blue_plot +
  inset_element(title_plot, 0.05, 0.88, 0.2, 0.95) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) 

ggsave(plot = blue_final,
       here::here("maps", "Day08.png"),
       width = 8,
       height = 9.5,
       dpi = 300)
