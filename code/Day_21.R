### #30DayMapChallenge 2021 ### 
## Day 21, Elevation

# packages
library(tidyverse)
library(rnaturalearth)
library(elevatr)
library(raster)
library(ggridges)
library(ggtext)

# get British Isles shapefile
british_isles <- ne_download(scale = 10,
                             type = "countries",
                             returnclass = "sf") %>% 
  dplyr::select("ISO_A3", "NAME_EN", "CONTINENT") %>% 
  filter(ISO_A3 %in% c("GBR", "IRL", "IMN"))

# get elevation data
dem <- get_elev_raster(british_isles, 
                       z = 7, 
                       clip = "locations", 
                       neg_to_na = FALSE)

dem_bng <- projectRaster(dem, crs = 27700)

dem_df <- as_tibble(sampleRegular(dem_bng, 
                                  size = 50000, 
                                  xy = TRUE)) %>% 
  rename(elevation = 3)

# plot 
elev_plot <- 
  ggplot() +
  geom_density_ridges(data = dem_df,
                      aes(x, y, 
                          group = y,
                          height = elevation),
                      stat = "identity",
                      scale = 4,
                      size = 0.4,
                      fill ="grey90",
                      color = "black") +
  coord_cartesian() +
  scale_x_continuous(limits = c(-250000, 750000),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(-50000, 1275000), 
                     expand = c(0, 0)) +
  annotate("richtext", 
           x = -220000, y = 1200000, 
           label = "<b><span style ='font-size: 32px; color: #004473'>Elevation of the British Isles</span></b><br><span style ='font-size: 14.2px; color: #404040'>Ridgeline/joy plot showing elevation throughout the British Isles</span>", 
           hjust = 0,
           family = "Poppins",
           label.colour = NA,
           fill = NA) +
  annotate("text", 
           x = 250000, y = -15000, 
           label = "#30DayMapChallenge | Day 21, Elevation | Visualisation: Joshua Copping | Data: SRTM & GMTED",
           size = 3,
           hjust = 0.5,
           family = "Roboto",
           colour = "#404040") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", 
                                       colour = NA,),
        panel.background = element_rect(fill = "white", 
                                        colour = "#004473",
                                        size = 10))

ggsave(plot = elev_plot,
       here::here("maps", "Day21.png"),
       width = 8,
       height = 10,
       dpi = 300)

