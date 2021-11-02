### #30DayMapChallenge 2021 ### 
## Day 2, Lines

# packages
library(tidyverse)
library(sf)
library(ggtext)
library(patchwork)

# load AIS tracking data downloaded from:
# https://data.gov.uk/dataset/963c1a7b-5b72-4cce-93f5-3f1e223fd575/anonymised-ais-derived-track-lines-2015

tracks <- st_read(here::here("data", "Anonymised_AIS_Derived_Track_Lines_2015_MMO", "Anonymised_AIS_Derived_Track_Lines_2015_MMO.shp")) %>% 
  st_transform(3857)

# plot
track_plot <- 
  ggplot() +
  geom_sf(data = tracks,
          alpha = 0.02,
          size = 0.1,
          colour = "#FCFDBF") +
  coord_sf() +
  scale_x_continuous(limits = c(-1100000, 784500),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(6000000, 8620000),
                     expand = c(0, 0)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#0C2330", 
                                       colour = NA),
        panel.background = element_rect(fill = "#0C2330", 
                                        colour = NA),
        plot.margin = margin(0, 0, 0, 0))

title_plot <- 
  ggplot() +
  labs(title = "Britain's Shipping Routes",
       subtitle = "<span style = 'font-size: 27.1px'>AIS derived vessel track lines from 2015</span><br><br><br><span style = 'font-size: 13.4px'>#30DayMapChallenge | Day 2, Lines | Visualisation: Joshua Copping | Data: MMO</span>") +
  theme(plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_text(family =  "Roboto Black",
                                  size = 32,
                                  colour = "grey90"),
        plot.subtitle = element_markdown(family = "Roboto",
                                         colour = "grey90"))

lines_plot <- track_plot +
  inset_element(title_plot, 0.45, 0, 0.95, 0.12) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) 

# save
ggsave(plot = lines_plot,
       here::here("maps", "Day02.png"),
       width = 10,
       height = 13.90289,
       dpi = 300)
