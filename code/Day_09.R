### #30DayMapChallenge 2021 ### 
## Day 9, Monochrome

# packages
library(tidyverse)
library(raster)
library(elevatr)
library(hddtools)
library(sf)
library(osmdata)
library(ggtext)
library(metR)

# get DEM data
arrochar_bbox <- extent(-4.868313, -4.729084, 56.202191, 56.305111)
arrochar_sp <- bboxSpatialPolygon(arrochar_bbox, 
                                  proj4stringFrom = NULL, 
                                  proj4stringTo = NULL)

dem <- get_elev_raster(arrochar_sp, 
                       z = 11,
                       clip = "locations",
                       neg_to_na = TRUE)

dem_df <- as.data.frame(dem, xy = TRUE) %>% 
  rename("height" = !!names(.[3])) %>%
  drop_na(height) 

# munro locations 
munros <- tribble(
  ~name, ~label, ~lon, ~lat, 
  "Beinn Ime", paste0("1,011 m / 56.236°N / 4.816°W") , -4.8160, 56.2368,
  "Ben Vorlich", paste0("943 m / 56.274°N / 4.755°W"), -4.7540, 56.2730,
  "Beinn Narnain", paste0("926 m / 56.221°N / 4.789°W"), -4.7891, 56.2216,
  "Ben Vane", paste0("915 m / 56.249°N / 4.781°W"), -4.7824, 56.2498
)  

munros_sf <- st_as_sf(munros, 
                      coords = c("lon", "lat"), 
                      crs = 4326)

# walking trails from OSM
arrochar_bbox_sf <- st_as_sf(arrochar_sp) %>% 
  st_bbox()

trails <- arrochar_bbox_sf %>%
  opq() %>%
  add_osm_feature(key = "highway", value = c("path", "track")) %>%
  osmdata_sf() 

trails_crop <- trails$osm_lines %>% 
  st_intersection(st_as_sf(arrochar_sp))

# plot
arrochar_map <- 
  ggplot() +
  geom_contour(data = dem_df, 
               aes(x = x, y = y, 
                   z = height),
               binwidth = 25, 
               colour = "grey65", 
               size = 0.2)  +
  geom_contour(data = dem_df, 
               aes(x = x, y = y, 
                   z = height),
               binwidth = 100, 
               colour = "grey65", 
               size = 0.4) +
  geom_sf(data = trails_crop,
          colour = "#333333",
          size = 0.4,
          linetype = "dashed") +
  geom_text_contour(data = dem_df,
                    aes(x = x, y = y,
                        z = height),
                    label.placer = label_placer_n(2),
                    check_overlap = TRUE,
                    colour = "grey66",
                    family = "Berlin Email 2",
                    stroke = 0.2,
                    stroke.colour = "white") +
  geom_sf_text(data = munros_sf,
               label = "x",
               family = "Fake Serif",
               colour = "#000000",
               size = 16,
               alpha = 0.8) +
  geom_sf_text(data = munros_sf, 
               aes(label = name),
               hjust = 1,
               vjust = 0.1,
               nudge_x = -0.005,
               family = "Rostheroid",
               size = 8,
               alpha = 0.75,
               colour = "#000000") +
  geom_sf_text(data = munros_sf, 
               aes(label = label),
               hjust = 1,
               vjust = 1,
               nudge_x = -0.005,
               nudge_y = -0.0015,
               family = "Berlin Email Bold",
               size = 4.5,
               alpha = 0.75,
               colour = "#000000") +
  annotate("richtext",
           x = -4.798699, y = 56.303,
           hjust = 0.5,
           vjust = 1,
           label = "<span style = 'font-size: 96px; color: #000000'>Arrochar Alps</span><br><span style = 'font-size: 38px; color: #333333'>Argyll & Bute / Scotland</span>",
           family = "Rostheroid",
           label.colour = NA,
           fill = NA) +
  coord_sf(label_graticule = "NESW",
           expand = FALSE) +
  scale_x_continuous(breaks = c(-4.86, -4.82, -4.78, -4.74)) +
  labs(caption = "#30DayMapChallenge | Day 9, Monochrome | Visualisation: Joshua Copping | Data: OpenStreetMap & SRTM") +
  theme(plot.background = element_rect(fill = "white", 
                                       colour = NA),
        panel.background = element_rect(fill = "white", 
                                        colour = NA),
        axis.line = element_line(size = 1, 
                                 colour = "grey25"),
        axis.title = element_blank(),
        axis.text = element_text(family = "Berlin Email 2",
                                 colour = "grey25",
                                 size = 18),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid = element_line(size = 0.5,
                                  colour = "grey92"),
        plot.margin = margin(25, 25, 0, 25),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    colour = "grey50",
                                    size = 11,
                                    margin = margin(20, 0, 5, 0)))

ggsave(plot = arrochar_map,
       here::here("maps", "Day09.png"),
       width = 9.8,
       height = 12,
       dpi = 300)
