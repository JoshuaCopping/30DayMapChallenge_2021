### #30DayMapChallenge 2021 ### 
## Day 18, Water

# packages
library(tidyverse)
library(sf)
library(rmapshaper)
library(ggfx)
library(ggspatial)
library(shadowtext)

# load and prep data
# Environment Agency Thames basin data: https://environment.data.gov.uk/catchment-planning/RiverBasinDistrict/6
# Ordnance Survey Open Rivers: https://www.ordnancesurvey.co.uk/business-government/products/open-map-rivers

coastal <- st_read(here::here("data", "RBD_Thames_shapefile", "WFD_Coastal_Water_Bodies_Cycle_2.shp")) %>% 
  st_transform(27700) 

basin <- st_read(here::here("data", "RBD_Thames_shapefile", "WFD_River_Basin_Districts_Cycle_2.shp")) %>% 
  st_transform(27700) %>% 
  st_difference(coastal) %>%
  ms_filter_islands(min_area = 100000)

estuary <- st_read(here::here("data", "RBD_Thames_shapefile", "WFD_Transitional_Water_Bodies_Cycle_2.shp")) %>% 
  st_transform(27700) %>% 
  st_union()

rivers <- st_read(here::here("data", "RBD_Thames_shapefile", "WFD_River_Water_Bodies_Cycle_2.shp")) %>% 
  st_transform(27700)

os_rivers <- st_read(here::here("data", "OS Open Rivers", "data", "WatercourseLink.shp")) %>%
  st_intersection(basin)

# get river thames
thames <- os_rivers %>% 
  filter(name1 %in% c("River Thames"))

# plot text 
text <- tribble(
  ~x, ~y, ~label, ~angle, ~size,
  419742, 216337, "The\nCotswolds", 0, 10,
  485928, 199468, "The Chilterns", 40, 14,
  435163, 178038, "North Wessex\nDowns", 5, 8,
  517632, 155074, "Surrey Hills", 20, 8,
  562256, 164832, "Kent Downs", 0, 10,
  526987, 182117, "London", 20, 5
)

# plot 
thames_plot <- 
  ggplot() +
  with_inner_glow(geom_sf(data = basin,
                          fill = "white",
                          colour = "#08519C",
                          size = 0.25),
                  colour = "#08519C",
                  expand = 0.5,
                  sigma = 20) +
  geom_sf(data = os_rivers, 
          size = 0.25,
          colour = "#2171B5") +
  geom_sf(data = rivers, 
          size = 0.5,
          colour = "#2171B5") +
  geom_sf(data = thames, 
          size = 1,
          colour = "#2171B5") +
  geom_sf(data = estuary, 
          size = NA,
          fill = "#2171B5") +
  geom_shadowtext(data = text,
                  aes(x = x, y = y,
                      label = label,
                      angle = angle,
                      size = size),
                  family = "Afterglow",
                  colour = "grey60",
                  bg.colour = "white",
                  lineheight = 0.75) +
  annotation_scale(aes(style = "ticks",
                       width_hint = 0.2,
                       line_col = "grey20",
                       text_col = "grey20"),
                   location = "bl",
                   pad_x = unit(3, "cm"),
                   pad_y = unit(2.5, "cm"),
                   height = unit(0.3, "cm"),
                   line_width = 1.5,
                   text_family = "Afterglow",
                   text_cex = 1.2,
                   tick_height = 0.6) +
  annotate("text",
           x = 465730, y = 256514,
           label = "River Thames Catchment",
           hjust = 0,
           family = "Afterglow",
           size = 24,
           colour = "grey25") + 
  scale_size_identity() +
  labs(caption = "#30DayMapChallenge | Day 18, Water | Visualisation: Joshua Copping | Data: Ordnance Survey & Environment Agency") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", 
                                       colour = NA,),
        panel.background = element_rect(fill = "white", 
                                        colour = NA),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    size = 12,
                                    colour = "grey60",
                                    margin = margin(10, 0, 0, 0)),
        plot.margin = margin(10, 20, 10, 20))

ggsave(plot = thames_plot,
       here::here("maps", "Day18.png"),
       width = 18,
       height = 11,
       dpi = 300)
