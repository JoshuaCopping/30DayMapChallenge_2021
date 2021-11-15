### #30DayMapChallenge 2021 ### 
## Day 14, New Tool

# not a new tool, but a new tool for me - neighbourhood analysis

# packages
library(tidyverse)
library(sf)
library(spdep)
library(ggtext)
library(patchwork)

# load UK administrative areas - counties & local authorities
uk_c <- st_read(here::here("data", "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC", "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"))

# convert to sp
ukc_sp <- as(uk_c, 'Spatial')

# neighbour analysis
ukc_neighbours <- poly2nb(ukc_sp)

# convert to lines, as sf and set crs
ukc_neighbours_sf <- as(nb2lines(ukc_neighbours, coords = coordinates(ukc_sp)), 'sf')
ukc_neighbours_sf <- st_set_crs(ukc_neighbours_sf, st_crs(uk_c))

# load Ireland shapefile
irl <- st_read(here::here("data", "gadm36_IRL_shp", "gadm36_IRL_0.shp")

# plot
neighbour_map <- 
  ggplot() + 
  geom_sf(data = irl,
          colour = "grey88",
          fill = "grey85",
          size = 0.1) +
  geom_sf(data = uk_c,
          colour = "white",
          fill = "grey88",
          size = 0.1) +
  geom_sf(data = ukc_neighbours_sf,
          size = 0.5,
          colour = "#FF3D61",
          alpha = 0.8) +
  labs(caption = "#30DayMapChallenge | Day 14, New Tools | Visualisation: Joshua Copping | Data: GADM & Office for National Statistics") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey82", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey82", 
                                        colour = NA),
        plot.caption = element_text(family = "Louis George Cafe",
                                    colour = "grey25",
                                    size = 8,
                                    hjust = 0.5,
                                    margin = margin(0, 0, 5, 0)),
        plot.margin = margin(20, 50, 0, 50)) 

title_plot <- 
  ggplot() +
  labs(title = "<b>Areas of The UK<br>Sharing Borders</b>",
       subtitle = "<b>Neighbour analysis on UK counties<br>& unitary authorities, with lines<br>showing which <span style ='color: #FF3D61'>areas share borders</span></b>") +
  theme(plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_markdown(family =  "Louis George Cafe",
                                      colour = "grey25",
                                      size = 31,
                                      margin = margin(10, 0, 10, 0)),
        plot.subtitle = element_markdown(family = "Louis George Cafe",
                                         colour = "grey40",
                                         size = 14))

final_plot <- neighbour_map +
  inset_element(title_plot, -0.05, 0.87, 0.4, 0.94) +
  plot_annotation(theme = theme(plot.margin = margin(20, 20, 20, 20))) 

ggsave(plot = final_plot,
       here::here("maps", "Day14"),
       width = 8,
       height = 10,
       dpi = 300)

