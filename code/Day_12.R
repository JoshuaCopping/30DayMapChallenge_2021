### #30DayMapChallenge 2021 ### 
## Day 12, Population

# packages
library(tidyverse)
library(eurostat)
library(rnaturalearth)
library(sf)
library(patchwork)
library(magrittr)

# get demographics data - population density and median age
pop <- get_eurostat("demo_r_d3dens", 
                    time_format = "num") %>%
  filter(time == 2018) %>%
  rename("population" = "values") %>%
  select(geo, population) 

age <- get_eurostat("demo_r_pjanind2", 
                    time_format = "num") %>% 
  filter(indic_de == "MEDAGEPOP",
         unit == "YR",
         time == 2018) %>%
  rename("med_age" = "values") %>%
  select(geo, med_age)

# join demographics data with nuts shapefiles
nuts2_sf <- st_read(file.choose()) %>%
  filter(!NUTS_ID %in% c("FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "ES70", "PT20", "PT30")) %>%
  left_join(pop, by = c("NUTS_ID" = "geo")) %>% 
  left_join(age, by = c("NUTS_ID" = "geo")) %>% 
  drop_na(population, med_age)

# calculate quantiles for population and age
nuts2_sf$population %>%
  quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_pop

nuts2_sf$med_age %>%
  quantile(probs = seq(0, 1, length.out = 4)) -> quantiles_age

# create colour scale
bivariate_color_scale <- tibble(
  "3 - 3" = "#3B4994", # high inequality, high income
  "2 - 3" = "#8C62AA",
  "1 - 3" = "#BE64AC", # low inequality, high income
  "3 - 2" = "#5698B9",
  "2 - 2" = "#A5ADD3", # medium inequality, medium income
  "1 - 2" = "#DFB0D6",
  "3 - 1" = "#5AC8C8", # high inequality, low income
  "2 - 1" = "#ACE4E4",
  "1 - 1" = "#E8E8E8" # low inequality, low income
) %>%
  gather("group", "fill")

# join colour to mapping data
nuts2_sf %<>%
  mutate(age_quantiles = cut(med_age,
                             breaks = quantiles_age,
                             include.lowest = TRUE),
         pop_quantiles = cut(population,
                             breaks = quantiles_pop,
                             include.lowest = TRUE),
         group = paste(as.numeric(age_quantiles), "-",
                       as.numeric(pop_quantiles))) %>%
  left_join(bivariate_color_scale, by = "group")

# load NUTS level 0 data
nuts2_mask <- nuts2_sf %>% 
  st_union()

nuts0_sf <- st_read(file.choose()) %>% 
  st_intersection(nuts2_mask)

# get world data to fill in missing countries
countries <- ne_download(scale = 10,
                         type = "countries",
                         returnclass = "sf") %>% 
  select("ADM0_A3","ISO_A3", "NAME_EN", "CONTINENT") %>% 
  filter(CONTINENT == "Europe",
         ADM0_A3 %in% c("ALB", "AND", "BIH", "KOS")) %>% 
  st_transform(3857)

# data for legend
bivariate_color_scale %<>%
  separate(group, into = c("Population", "Age"), sep = " - ") %>%
  mutate(Population = as.integer(Population),
         Age = as.integer(Age))

text <- tribble(
  ~x, ~y, ~label, ~angle, ~size,
  2, 0, "Median Age", 0, 10,
  3.25, 0.25, "+", 0, 16,
  0.75, 0.25, "-", 0, 16,
  0, 2, "Pop. Density", 270, 10,
  0.45, 3.25, "+", 90, 16,
  0.45, 0.75, "-", 90, 16,
)

# plot 
population_map<- 
  ggplot() +
  geom_sf(data = countries,
          colour = NA,
          fill = "grey40") +
  geom_sf(data = nuts2_sf, 
          aes(fill = fill),
          colour = NA) +
  geom_sf(data = nuts0_sf,
          colour = "white",
          fill = NA,
          size = 0.3) +
  scale_fill_identity()+
  coord_sf(expand = FALSE) +
  labs(caption = "#30DayMapChallenge | Day 12, Population | Visualisation: Joshua Copping | Data: EuroStat") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey20", 
                                       colour = NA,),
        panel.background = element_rect(fill = "grey20", 
                                        colour = NA),
        plot.caption = element_text(colour = "grey50",
                                    family = "Roboto",
                                    size = 14,
                                    hjust = 0.5,
                                    margin = margin(40, 0, 5, 0)),
        plot.margin = margin(50, 50, 0, 50))

legend <- 
  ggplot() +
  geom_tile(data = bivariate_color_scale,
            aes(x = Population, y = Age,
                fill = fill)) +
  geom_text(data = text,
            aes(x = x, y = y,
                label = label,
                angle = angle,
                size = size),
            family =  "Berlin Email 2",
            colour = "grey90",
            hjust = 0.5,
            vjust = 0.5) +
  annotate("segment", 
           x = 1, xend = 3, 
           y = 0.35, yend = 0.35,
           arrow = arrow(type = "open", 
                         length = unit(0.25,"cm")),
           colour = "grey90") +
  annotate("segment", 
           x = 0.35, xend = 0.35, 
           y = 1, yend = 3,
           arrow = arrow(type = "open", 
                         length = unit(0.25,"cm")),
           colour = "grey90") +
  coord_equal(clip = "off") +
  scale_fill_identity() +
  scale_size_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA))

title_plot <- ggplot() +
  labs(title = "Population Demographics of Europe",
       subtitle = "Median age & population density in NUTS 2 regions") +
  theme(plot.background = element_rect(fill = NA, 
                                       colour = NA),
        panel.background = element_rect(fill = NA, 
                                        colour = NA),
        plot.title = element_text(family =  "Berlin Email 2",
                                  size = 56,
                                  colour = "white",
                                  margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(family =  "Berlin Email 2",
                                     size = 32,
                                     colour = "grey90"))

final_map <- population_map +
  inset_element(title_plot, 0, 0.85, 0.4, 1) +
  inset_element(legend, 0.78, 0.28, 1, 0.5) +
  plot_annotation(theme = theme(plot.margin = margin(20, 20, 20, 20),
                                plot.background = element_rect(colour = "grey95",
                                                               size = 20)))

ggsave(plot = final_map,
       here::here("maps", "Day12.png"),
       width = 15,
       height = 14.5,
       dpi = 300)
