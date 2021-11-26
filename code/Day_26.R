### #30DayMapChallenge 2021 ### 
## Day 26, Choropleth

# packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(WDI)
library(countrycode)
library(cartogram)
library(paletteer)
library(patchwork)

# get countries  
countries <- ne_download(scale = 110,
                         type = "countries",
                         returnclass = "sf") %>% 
  select("ADM0_A3", "ISO_A3") %>% 
  replace_na(list(ISO_A3 = "000")) %>% 
  mutate(ISO_A3 = case_when(ISO_A3 == "000" ~ ADM0_A3,
                            TRUE ~ ISO_A3)) 

# get WDI data
# Mortality rate attributed to household and ambient air pollution, age-standardized (per 100,000 population)
wdi_data <- tibble(WDI(indicator = "SH.STA.AIRP.P5")) %>% 
  rename("value" = "SH.STA.AIRP.P5") %>%
  group_by(country) %>% 
  drop_na(value) %>% 
  top_n(1, year) %>% 
  mutate(ISO3 = countrycode(iso2c, 
                            origin = "iso2c", 
                            destination = "iso3c")) %>% 
  drop_na(ISO3)

# join WDI and countries data
world_pollution <- countries %>% 
  left_join(wdi_data, by = c("ISO_A3" = "ISO3")) %>%
  drop_na(value) %>% 
  st_transform(crs = "+proj=robin") 

# cartogram
world_pollution_carto <- cartogram_cont(world_pollution,
                                        weight = "value")

# reference plot
ref_pollution <- world_pollution %>% 
  ms_filter_islands(min_area = 10000000000)

ref_outline <- ref_pollution %>% 
  st_union() 

# plot 
carto <- 
  ggplot(world_pollution_carto) +
  geom_sf(aes(fill = value),
          colour = NA) +
  coord_sf(xlim = c(-12000000, 16000000),
           ylim = c(-7000000, 7500000),
           expand = FALSE) +
  scale_fill_paletteer_c(`"pals::ocean.matter"`,
                         name = "Mortality per 100,000 population",
                         limits = c(0, 350),
                         breaks = seq(50, 300, by = 50)) +
  labs(title = "The Invisible Killer",
       subtitle = "Cartogram showing global mortality rates attributed to\nair pollution, with regions resized according to their value", 
       caption = "#30DayMapChallenge | Day 26, Choropleth | Visualisation: Joshua Copping | Data: Natural Earth & World Health Organisation") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey20", 
                                       colour = NA),
        panel.background = element_rect(fill = "grey20", 
                                        colour = NA),
        plot.margin = margin(0, 20, 0, 20),
        plot.title = element_text(hjust = 0.5,
                                  family = "Sansation Bold",
                                  size = 46,
                                  colour = "grey95",
                                  margin = margin(30, 0, 5, 0)),
        plot.subtitle = element_text(hjust = 0.5,
                                     family = "Sansation Light",
                                     size = 20,
                                     colour = "grey95",
                                     margin = margin(0, 0, 50, 0)),
        plot.caption = element_text(hjust = 0.5,
                                    family = "Roboto",
                                    size = 12,
                                    colour = "grey50",
                                    margin = margin(80, 0, 5, 0)),
        legend.position = c(0.5, 0),
        legend.key.width = unit(3, "cm"),
        legend.margin = margin(10, 0, 0, 0),
        
        legend.title = element_text(family  = "Sansation",
                                    colour = "grey80",
                                    size = 22,
                                    hjust = 0.5),
        legend.text = element_text(family  = "Sansation",
                                   colour = "grey80",
                                   size = 16)) +
  guides(fill = guide_colourbar(direction = "horizontal",
                                title.position = "top",
                                label.position = "bottom"))

reference_plot <- 
  ggplot() +
  geom_sf(data = ref_pollution,
          aes(fill = value),
          colour = NA) +
  geom_sf(data = ref_outline,
          colour = "white",
          fill = NA,
          size = 0.25) +
  scale_fill_paletteer_c(`"pals::ocean.matter"`) +
  labs(title = "Original map for reference") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_text(hjust = 0.5,
                                  family = "Sansation",
                                  size = 14,
                                  colour = "grey95",
                                  margin = margin(0, 0, 5, 0)))

final_map <- carto + 
  inset_element(reference_plot, 0.7, 0.02, 0.95, 0.22,
                align_to = "full",
                clip = FALSE) +
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0))) 

ggsave(plot = final_map,
       here("maps", "Day26.png"),
       width = 16.6,
       height = 12,
       dpi = 300)
