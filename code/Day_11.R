### #30DayMapChallenge 2021 ### 
## Day 11, 3D

# load packages
library(tidyverse)
library(elevatr)
library(raster)
library(sf)
library(osmdata)
library(ggtext)
library(ggfx)
library(ggspatial)

# National Parks data available from:
# https://geoportal.statistics.gov.uk/datasets/national-parks-december-2019-gb-buc/explore

# load Cairngorms shapefile
cairngorms <- st_read(here::here("data", "National_Parks__December_2019__GB_BUC-shp", "National_Parks__December_2019__GB_BUC.shp")) %>% 
  filter(NPARK19NM == "The Cairngorms National Park") 

# get dem & project to Brtish National Grid
dem <- get_elev_raster(cairngorms, 
                       z = 11,
                       clip = "locations",
                       neg_to_na = TRUE)

dem_bng <- projectRaster(dem, crs = 27700)

# multiply height by 2 to increase hillshade constrast
dem_bng2 <- dem_bng
dem_bng2[] <- dem_bng2[]*2

# create hillshade data
slope_r <- terrain(dem_bng2, opt = 'slope')
aspect_r <- terrain(dem_bng2, opt = 'aspect')
hillshade_r <- hillShade(slope_r, aspect_r, 30, 120)

# mask and convert rasters to data frame
cairngorms_shrunk <- cairngorms %>% 
  st_buffer(-50)

dem_crop <- terra::mask(dem_bng2, cairngorms_shrunk)
hillshade_crop <- terra::mask(hillshade_r, cairngorms_shrunk)

dem_df <- as.data.frame(dem_crop, xy = TRUE) %>% 
  rename("height" = !!names(.[3])) %>%
  drop_na(height) 

hillshade_df <- as.data.frame(hillshade_crop, xy = TRUE) %>% 
  rename("hillshade" = "layer") %>%
  drop_na(hillshade)

# get OSM water data 
x <- c(-4.3972, -2.6544)
y <- c(56.7094, 57.4669)

cairngorms_bbox <- rbind(x,y) 
colnames(cairngorms_bbox) <- c("min", "max")

natural_water <- cairngorms_bbox %>%
  opq() %>%
  add_osm_feature(key = "natural", value = "water") %>%
  osmdata_sf() 

water <- cairngorms_bbox %>%
  opq() %>%
  add_osm_feature(key = "water", 
                  value = c("river", "oxbow", "lake", "stream_pool")) %>%
  osmdata_sf() 

waterway <- cairngorms_bbox %>%
  opq() %>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "riverbank", "stream")) %>%
  osmdata_sf() 

# select and mask water 
nat_wat_p <- natural_water$osm_polygons %>% 
  dplyr::select(osm_id)

nat_wat_mp <- natural_water$osm_multipolygons %>% 
  dplyr::select(osm_id)

wat_p <- water$osm_polygons %>% 
  dplyr::select(osm_id)

wat_mp <- water$osm_multipolygons %>% 
  dplyr::select(osm_id)

wat_way_p <- waterway$osm_polygons %>% 
  dplyr::select(osm_id)

cairngorms_shrunk_water <- cairngorms %>% 
  st_buffer(-200)

all_water <- rbind(nat_wat_p, nat_wat_mp, wat_p, wat_mp, wat_way_p) %>% 
  st_transform(27700) %>% 
  st_intersection(cairngorms_shrunk_water) 

# plot 
# inspired by the Ordnance Survey National Parks visualisations:
# https://www.ordnancesurvey.co.uk/newsroom/blog/new-poster-to-celebrate-70-years-of-britains-national-parks

cairngorms_map <- 
  ggplot() +
  with_shadow(geom_raster(data = dem_df,
                          aes(x = x, y = y,
                              fill = height)),
              x_offset = 10,
              y_offset = 25,
              sigma = 25,
              colour = "grey20") +
  geom_sf(data = all_water,
          colour = "#A2F2DE",
          fill = "#A2F2DE")  +
  geom_raster(data = hillshade_df, 
              aes(x = x, y = y, 
                  alpha = hillshade),
              fill = "black") + 
  annotate("richtext",
           x = 240000, y = 838000,
           label = "<span style = 'font-size: 80px'>Cairngorms</span><br><span style = 'font-size: 69px'>National Park</span>",
           hjust = 0,
           vjust = 1,
           family = "Altone Trial",
           colour = "grey20",
           lineheight = 0,
           label.colour = NA,
           fill = NA) +
  annotate("richtext",
           x = 240100, y = 826500,
           label = "The Cairngorms is one of two National Parks in<br>Scotland and is the largest in the UK, covering<br>an area of 4,528 sq km. Around 50% of the<br>land in the park is considered ‘wild land’, acting<br>as an important stronghold for numerous rare<br>and threatened, species and habitats,<br>including capercaillie, osprey, pine marten,<br>wildcat, and Caledonian pine forest.",
           hjust = 0,
           vjust = 1,
           family = "Altone Trial",
           colour = "grey30",
           size = 5.3,
           lineheight = 1,
           label.colour = NA,
           fill = NA) +
  annotate("text",
           x = 351458, y = 757000,
           label = "#30DayMapChallenge | Day 11, 3D | Visualisation: Joshua Copping | Data: SRTM & OpenStreetMap",
           hjust = 1,
           vjust = 0,
           family = "Altone Trial",
           size = 4.5,
           colour = "grey40") +
  annotation_scale(aes(style = "ticks",
                       width_hint = 0.3,
                       line_col = "grey40",
                       text_col = "grey40"),
                   pad_x = unit(0.4, "cm"),
                   pad_y = unit(0, "cm"),
                   height = unit(0.3, "cm"),
                   line_width = 1.2,
                   text_family = "Altone Trial",
                   text_cex = 1.2) +
  scale_fill_gradient2(low = "#BAE0C0", 
                       mid = "#F2E8C2", 
                       high = "#FFFFFF", 
                       midpoint = 800) +
  scale_alpha_continuous(range = c(0, 0.9)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits =c(757000, 843000),
                     expand = c(0, 0)) +
  coord_sf(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#F4F6F4", 
                                       colour = NA,),
        panel.background = element_rect(fill = "#F4F6F4", 
                                        colour = NA),
        legend.position = "none",
        plot.margin = margin(10, 0, 20, 0))

ggsave(plot = cairngorms_map,
       here::here("maps", "Day11.png"),
       width = 17,
       height = 12,
       dpi = 300)
