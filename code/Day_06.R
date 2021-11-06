### #30DayMapChallenge 2021 ### 
## Day 6, Red

# packages
library(tidyverse)
library(sf)
library(GADMTools)
library(spatialEco)
library(raster)
library(patchwork)
library(ggtext)
library(ggimage)

# load data
# data from GBIF: https://doi.org/10.15468/dl.kfqmkg

species <- read_delim(here::here("data", "Scottish_RedSpp_GBIF.csv"), delim = "\t") %>% 
  filter(occurrenceStatus == "PRESENT",
         basisOfRecord == "HUMAN_OBSERVATION",
         year %in% 2011:2020,
         coordinateUncertaintyInMeters < 80000) %>% 
  dplyr::select(species, decimalLongitude, decimalLatitude) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326) %>% 
  st_transform(27700) %>% 
  mutate(name = case_when(species == "Vulpes vulpes" ~ "Red Fox",
                          species == "Sciurus vulgaris" ~ "Red Squirrel",
                          species == "Cervus elaphus" ~ "Red Deer",
                          species == "Milvus milvus" ~ "Red Kite",
                          species == "Lagopus lagopus" ~ "Red Grouse",
                          species == "Vanessa atalanta" ~ "Red Admiral"))

# scotland outline
scotland <- st_read(here::here("data", "gadm36_GBR_shp", "gadm36_GBR_1.shp")) %>% 
  filter(NAME_1 == "Scotland") %>% 
  st_transform(27700)

# point/kernel density calculations
species_spatial <- as_Spatial(species)
species_density <- sp.kde(species_spatial, 
                          nr = 2000, nc = 2000,
                          standardize = TRUE,
                          bw = 75000)

species_density_masked <- terra::mask(species_density, scotland)

species_density_df <- as.data.frame(species_density_masked, xy = TRUE) %>% 
  drop_na(kde) 

# text for plot
species_text <- tribble(
  ~x, ~y, ~name, 
  90000, 1150000, "Red Fox", 
  90000, 1150000, "Red Admiral",
  90000, 1150000, "Red Grouse",
  90000, 1150000, "Red Deer",
  90000, 1150000, "Red Squirrel",
  90000, 1150000, "Red Kite"
)

legend_text <- tribble(
  ~x, ~y, ~label,
  35000, 1030000, "Few occurrences",
  35000, 1062000, "Some occurrences",
  35000, 1094000, "Most occurrences" 
)

silhouettes <- tribble(
  ~x, ~y, ~name, ~image,
  30000, 1150000, "Red Fox", here::here("data", "images", "fox.png"),
  30000, 1150000, "Red Admiral", here::here("data", "images", "butterfly.png"),
  30000, 1150000, "Red Grouse", here::here("data", "images", "grouse.png"),
  30000, 1150000, "Red Deer", here::here("data", "images", "deer.png"),
  30000, 1150000, "Red Squirrel", here::here("data", "images", "squirrel.png"),
  30000, 1150000, "Red Kite", here::here("data", "images", "kite.png"),
)

# plot
# raster map
red_rast <- ggplot() +
  geom_tile(data = species_density_df, 
            aes(x = x, y = y,
                fill = kde)) +
  annotate("richtext", 
           x = 9300, y = 1180000, 
           label = "<b><span style = 'font-size: 46px; color: #D73D39'>Scotland's Red Species</span></b><br><br><span style = 'font-size: 18px; color: #CCCCCC'>Occurrences of six red coloured and named species in Scotland<br>between 2011 and 2020, and a density map showing hotspots<br>of where occurrences were recorded</span>",
           hjust = 0,
           lineheight = 1.2,
           label.colour = NA,
           fill = NA) +
  geom_text(data = legend_text,
            aes(x = x, y = y, label = label),
            size = 4.55,
            hjust = 0,
            colour = "grey80") +
  coord_equal() + 
  scale_fill_stepsn(n.breaks = 6,
                    colours = c("#E0675D", "#AD0600"),
                    guide = "coloursteps") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey15", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey15", 
                                        colour = NA),
        plot.margin = margin(0, 0, 0, 20),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = c(0.07, 0.75)) 

# points map
red_points <- ggplot() +
  geom_sf(data = scotland,
          fill = "grey20",
          colour = "grey70",
          size = 0.2) +
  geom_sf(data = species,
          colour = "#D73D39",
          shape = 16,
          size = 0.15,
          alpha = 0.7) +
  geom_text(data = species_text, 
            aes(x = x, y = y, 
                label = name),
            hjust = 0,
            colour = "grey80",
            size = 6) +
  geom_image(data = silhouettes, 
             aes(x = x, y = y,
                 image = image),
             colour = "#CCCCCC",
             size = 0.2) +
  coord_sf() +
  scale_x_continuous(limits = c(-10000, 470000)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_wrap(~name, 
             ncol = 2) +
  labs(caption = "#30DayMapChallenge | Day 6, Red | Visualisation: Joshua Copping | Data: Global Biodiversity Information Facility") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey15", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey15", 
                                        colour = NA),
        plot.caption = element_text(hjust = 4.75,
                                    size = 12,
                                    colour = "grey80",
                                    family = "Roboto",
                                    margin = margin(30, 0, 5, 0)),
        strip.text = element_blank(),
        plot.margin = margin(20, 0, 0, 0),
        panel.spacing.x = unit(4, "lines"),
        panel.spacing.y = unit(1, "lines")) 

red_final <- (red_rast | red_points) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) 

ggsave(plot = red_final,
       here::here("maps", "Day06.png"),
       width = 15,
       height = 12,
       dpi = 300)
