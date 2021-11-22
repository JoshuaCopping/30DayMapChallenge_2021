### #30DayMapChallenge 2021 ### 
## Day 22, Boundaries

# packages
library(tidyverse)
library(sf)

# load and wrangle data 
country <- st_read(here("data", "gadm36_GBR_shp", "gadm36_GBR_1.shp")) %>% 
  filter(NAME_1 == "England") %>%
  st_transform(27700) %>% 
  select(NAME_1) %>% 
  rename("NAME" = "NAME_1") %>% 
  mutate(type = "country",
         colour = "#1F83B4",
         size = 0.5,
         order  = 2)

regions <- st_read(here("data", "bdline_essh_gb", "Data", "GB", "english_region_region.shp")) %>% 
  st_intersection(country) %>% 
  select(NAME) %>% 
  mutate(type = "regions",
         colour = "#2CA030",
         size = 0.3,
         order  = 3)

ceremonial <- st_read(here("data", "bdline_essh_gb", "Data", "Supplementary_Ceremonial", "Boundary-line-ceremonial-counties_region.shp")) %>% 
  st_intersection(country) %>% 
  select(NAME) %>% 
  mutate(type = "ceremonial",
         colour = "#BCBD22",
         size = 0.2,
         order  = 4)

counties <- st_read(here("data", "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC", "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp")) %>%   
  st_intersection(country) %>% 
  select(ctyua19nm) %>% 
  rename("NAME" = "ctyua19nm") %>%
  mutate(type = "counties",
         colour = "#FFBF50",
         size = 0.2,
         order  = 5)

local <- st_read(here("data", "bdline_essh_gb", "Data", "GB", "district_borough_unitary_region.shp")) %>% 
  st_intersection(country) %>% 
  select(NAME) %>% 
  mutate(type = "local",
         colour = "#FF7F0E",
         size = 0.1,
         order  = 6)

constit <- st_read(here("data", "bdline_essh_gb", "Data", "GB", "westminster_const_region.shp")) %>% 
  st_intersection(country) %>% 
  select(NAME) %>% 
  mutate(type = "constit",
         colour = "#C7519C",
         size = 0.1,
         order  = 7)

parish <- st_read(here("data", "bdline_essh_gb", "Data", "GB", "parish_region.shp")) %>%  
  st_intersection(country) %>% 
  select(NAME) %>% 
  mutate(type = "parish",
         colour = "#8A60B0",
         size = 0.1,
         order  = 8)

# merge for one map
all <- rbind(parish, constit, local, counties, ceremonial, regions, country) %>% 
  mutate(type = "all",
         order  = 9)

# create blank plot for title 
blank <- country %>% 
  mutate(type = "blank",
         colour = NA,
         size = NA,
         order = 1)

# bind individual and all, into one dataframe 
all_boundaries <- rbind(country, regions, ceremonial, counties, local, constit, parish, all, blank)

# names for plot facets
labels <- c("", "National Boundary", "Regions", "Ceremonial Counties", "Counties & Unitary Authorities", "Local Authority Districts", "Parliamentary Constituencies", "Parishes", "All Boundaries")
names(labels) <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

# can't seem to change plotting order for all plot, add country and regions separately on top

country_all <- country %>% 
  mutate(order = 9)

regions_all <- regions %>% 
  mutate(order = 9)

# title text 
title <- tribble(
  ~x, ~y, ~order, ~text, 
  369157.8, 500000, 1, "The Many Boundaries\nShaping England"
)

# plot
boundaries_plot <- 
  ggplot() +
  geom_sf(data = all_boundaries,
          aes(colour = colour,
              size = size),
          fill = NA) +
  geom_sf(data = regions_all,
          aes(colour = colour,
              size = size),
          fill = NA) +
  geom_sf(data = country_all,
          aes(colour = colour,
              size = size),
          fill = NA) +
  scale_colour_identity() +
  scale_size_identity() +
  facet_wrap(~order,
             ncol = 3, 
             labeller = labeller(order = labels)) +
  labs(caption = "#30DayMapChallenge | Day 22, Boundaries | Visualisation: Joshua Copping | Data: Office for National Statistics & Ordnance Survey") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey10", 
                                       colour = NA,),
        panel.background = element_rect(fill = "grey10", 
                                        colour = NA),
        strip.text = element_text(hjust = 0.5,
                                  family = "Tall Films",
                                  size = 30,
                                  colour = "white",
                                  margin = margin(9, 0, 3, 0)), 
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    size = 12,
                                    colour = "grey50",
                                    margin = margin(20, 0, 5, 0)),
        plot.margin = margin(40, 10, 0, 10)) +
  geom_text(data = title,
            aes(x = x, y = y,
                label = text),
            family = "Tall Films",
            size = 20,
            colour = "white",
            hjust = 0.5,
            vjust = 1,
            lineheight = 1)

ggsave(plot = boundaries_plot,
       here("maps", "Day22.png"),
       width = 12,
       height = 15,
       dpi = 300)
