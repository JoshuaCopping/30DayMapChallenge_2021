### #30DayMapChallenge 2021 ### 
## Day 17, Land

# packages
library(raster)
library(sp)
library(sf)
library(tidyverse)
library(sfheaders)
library(patchwork)

# load corine raster and british isles vectors 
corine_r <- raster(here::here("data", "u2018_clc2018_v2020_20u1_raster100m", "DATA", "U2018_CLC2018_V2020_20u1.tif"))

uk_sf <- st_read(here::here("data", "gadm36_GBR_shp", "gadm36_GBR_0.shp")) %>% 
  st_transform(3035)

irl_sf <- st_read(here::here("data", "gadm36_IRL_shp", "gadm36_IRL_0.shp")) %>% 
  st_transform(3035)

imn_sf <- st_read(here::here("data", "gadm36_IMN_0", "gadm36_IMN_0.shp")) %>% 
  st_transform(3035)

bi_sf <- rbind(uk_sf, irl_sf, imn_sf)
bi_sp <- as(bi_sf, "Spatial")

# crop, mask and project corine data
corine_bi_r <- crop(corine_r, bi_sp)
corine_bi_r <- mask(corine_bi_r, bi_sp)

corine_bi_r <- projectRaster(corine_bi_r, crs = 3857)

# update raster with classes
natural_r <- corine_bi_r
natural_r[natural_r <23 | natural_r == 128] <- NA
natural_r[natural_r >= 23] <- 1

# aggregate cells then to dataframe
natural_agg_r <- aggregate(natural_r, 
                           fact = 145, 
                           fun = sum,
                           na.rm = TRUE)

natural_df <- as.data.frame(natural_agg_r, xy = TRUE) %>% 
  rename("land" = "layer") %>%
  drop_na(land) 

# quantify natural land
side_data <- natural_df %>% 
  group_by(y) %>% 
  summarise(total = mean(land))

# plotting
natural_map <- 
  ggplot() +
  geom_point(data = natural_df,
             aes(x = x, y = y,
                 size = land),
             colour = "#809D24",
             alpha = 0.8) +
  scale_size_continuous(range = c(0.01, 4)) +
  coord_equal() +
  theme_void() +
  theme(legend.position = "none") 

side_plot <- 
  ggplot() +
  geom_bar(data = side_data,
           aes(x = y, y = total),
           stat = "identity",
           colour = NA,
           fill = "#809D24",
           alpha = 0.8) +
  scale_y_continuous(limits = c(0, 20000),
                     expand = c(0, 0)) +
  coord_flip() +
  theme_void()

final <- 
  natural_map + side_plot +
  plot_layout(ncol = 2, 
              widths = c (4, 1)) +
  plot_annotation(title = "Natural Land in The British Isles",
                  subtitle = "the latitudinal gradient of natural land cover in the british isles,\nusing aggregated corine 2018 land cover classes",
                  caption = "#30DayMapChallenge | Day 17, Land | Visualisation: Joshua Copping | Data: Copernicus Programme",
                  theme = theme(plot.background = element_rect(fill = "grey95", 
                                                               colour = NA,),
                                panel.background = element_rect(fill = "grey95", 
                                                                colour = NA),
                                plot.title = element_text(hjust = 0.5,
                                                          family = "The Light Font",
                                                          size = 50,
                                                          colour = "#809D24",
                                                          margin = margin(30, 0, 15, 0)), 
                                plot.subtitle = element_text(hjust = 0.5, 
                                                             family = "The Light Font",
                                                             size = 26,
                                                             colour = "grey40"),
                                plot.caption = element_text(hjust = 0.5,
                                                            family = "Roboto",
                                                            size = 12,
                                                            color = "grey40"))) 

ggsave(plot = final,
       here::here("maps", "Day17.png"),
       width = 12,
       height = 15,
       dpi = 300)

