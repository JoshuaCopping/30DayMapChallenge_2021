### #30DayMapChallenge 2021 ### 
## Day 4, Hexagons

# packages
library(tidyverse)
library(jsonlite)
library(sf)
library(ggfx)
library(paletteer)
library(ggtext)
library(ggrepel)

# load london outline data 
ldn_outline <- st_read(here::here("LondonBoundaries", "ESRI", "London_Ward_CityMerged.shp")) %>%
  st_union() %>%
  st_transform(27700)

ldn_boroughs <- st_read(here::here("LondonBoundaries", "ESRI", "London_Borough_Excluding_MHW.shp")) %>%
  st_union() %>%
  st_transform(27700) 

ldn_borough_names <- st_read(here::here("LondonBoundaries", "ESRI", "London_Borough_Excluding_MHW.shp")) %>%
  st_transform(27700) 

# get charge point data 
charge_points <- fromJSON("https://chargepoints.dft.gov.uk/api/retrieve/registry/format/json")

# convert to sf spatial object
ldn_charge_points <- tibble(charge_points$ChargeDevice$ChargeDeviceLocation) %>% 
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  select(Longitude:Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>% 
  st_set_crs(4326) %>% 
  st_transform(27700) %>% 
  st_intersection(ldn_outline) %>% 
  mutate(charge_point = 1)

# create hex grid 
ldn_hex <- st_make_grid(ldn_outline,
                        cellsize = 1000,
                        what = 'polygons',
                        square = FALSE,
                        flat_topped = FALSE) %>%
  st_as_sf()

ldn_hex <- ldn_hex %>% 
  mutate(hex_id = 1:nrow(ldn_hex))

# join charge points and hex grid, then sum points per hexagon
ldn_hex_charge <- st_join(ldn_hex, 
                          ldn_charge_points, 
                          join = st_contains) %>% 
  group_by(hex_id) %>% 
  summarise(total_points = sum(charge_point, 
                               na.rm = FALSE)) %>% 
  st_intersection(ldn_boroughs)

# plot 
charge_plot <- 
  ggplot() +
  with_shadow(geom_sf(data = ldn_outline,
                      colour = "#7DF9FF",
                      fill = "grey25",
                      size = 1.5),
              x_offset = 0,
              y_offset = 0,
              sigma = 20,
              colour = "#7DF9FF") +
  geom_sf(data = ldn_hex_charge,
          aes(fill = total_points),
          colour = "grey80",
          size = 0.3) +
  geom_sf(data = ldn_outline,
          colour = "#7DF9FF",
          fill = NA,
          size = 1.5) +
  geom_text_repel(data = ldn_borough_names,
                  aes(label = NAME,
                      geometry = geometry),
                  stat = "sf_coordinates",
                  family = "Roboto Black",
                  colour = "white",
                  size = 5,
                  alpha = 0.9) +
  annotate("segment", 
           x = 553200, xend = 575000,
           y = 160500, yend = 160500,
           colour = "white",
           size = 1.5) +
  annotate("text",
           x = 553200, y = 159500,
           label = "Number of charge points per square kilometre",
           hjust = 0,
           colour = "grey80",
           family = "Roboto",
           size = 6.1) +
  annotate("richtext",
           x = 552750, y = 167000,
           label = "<span style = 'font-size: 149px'>London's</span><br><span style = 'font-size: 106.5px'>electric car</span><br><span style = 'font-size: 82.4px'>charging points</span>",
           family = "Bebas",
           hjust = 0,
           color = "#4FF9FF",
           label.colour = NA,
           fill = NA,
           alpha = 0.9) +
  coord_sf(xlim = c(502500, 575000)) +
  scale_fill_steps2(low = "#09606E", 
                    mid = "#418DD4", 
                    high = "#4FF9FF", 
                    midpoint = 50, 
                    na.value = "grey30") +
  labs(caption = "#30DayMapChallenge | Day 4, Hexagons | Visualisation: Joshua Copping | Data: UK Department for Transport") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey25", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey25", 
                                        colour = NA),
        legend.position = c(0.815, 0.08),
        legend.key.width = unit(2.5, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(family  = "Roboto",
                                   face = "bold",
                                   colour = "grey80",
                                   size = 15),
        plot.caption = element_text(hjust = 0.5,
                                    size = 11,
                                    colour = "grey80",
                                    family = "Roboto",
                                    margin = margin(20, 0, 0, 0)),
        plot.caption.position = "plot",
        plot.margin = margin(0, 0, 0, 0)) +
  guides(fill = guide_colourbar(override.aes = list(colour = NA),
                                label.position = "bottom",
                                direction = "horizontal",
                                ticks = FALSE)) 

ggsave(plot = charge_plot,
       here::here("maps", "Day04.png"),
       width = 18,
       height = 12,
       dpi = 300)
