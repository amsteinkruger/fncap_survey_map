# Get a map of Oregon's state forests.

# Include state borders, a handful of cities, and state forests with reasonably clear unit definitions. 

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(showtext)

# Data

# Check whether ODF boundaries work. (Nope.)

# dat_odf = "data/ODF_Boundaries.lyrx" %>% vect

#  Get cities, then subset to Astoria, Portland, Salem, Eugene, Bend, and Medford.

dat_cities = 
  "data/Cities_OR.gdb" %>% 
  vect %>% 
  filter(CITY %in% c("ASTORIA", "PORTLAND", "SALEM", "EUGENE", "BEND", "MEDFORD")) %>% 
  mutate(City = CITY %>% str_to_title)

#  Get state boundaries, then subset to 121W, then subset to 121W and (?)N.

dat_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") # %>% 
  # project("epsg:2992")

dat_or_west = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  crop(ext(-125, -121, 40, 48))

dat_or_northwest = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  crop(ext(-125.0, -120.5, 43.5, 48.0))

#  State Forests, but pulling from more detailed code for earlier bar visualization

dat_odf =
  "data/ODF.gdb" %>%
  vect %>%
  select(DISTRICT) %>%
  mutate(FOREST = case_when(DISTRICT %in% c("Tillamook", "Forest Grove") ~ "Tillamook State Forest",
                            DISTRICT == "Astoria" ~ "Clatsop State Forest",
                            DISTRICT == "North Cascade" ~ "Santiam State Forest",
                            DISTRICT == "Coos" ~ "Elliott State Forest",
                            DISTRICT == "Klamath-Lake" ~ "Gilchrist State Forest",
                            TRUE ~ NA)) %>%
  filter(!is.na(FOREST)) %>% 
  filter(FOREST %in% c("Clatsop State Forest", "Tillamook State Forest", "Santiam State Forest")) %>% # 2025/05/20
  group_by(FOREST) %>%
  summarize() %>%
  ungroup

# Check state forest names.

dat_odf %>% 
  ggplot() +
  geom_spatvector(aes(fill = FOREST), color = NA)

# Visualize

#  Use Oregon State's "Pine Stand" color.

pine = "#4A773C"

#  Use Calibri. 

font_add(family = "Calibri", regular = "C:/Windows/Fonts/calibri.ttf")

#  Plot:
#   All OR
#   W OR
#   NW OR

# All OR

vis = 
  ggplot() +
  geom_spatvector(data = dat_or,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities,
                  color = "grey50") +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri") +
  geom_spatvector(data = dat_odf,
                  color = NA,
                  fill = pine) +
  geom_spatvector_text(data = dat_odf, 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

# W OR

vis = 
  ggplot() +
  geom_spatvector(data = dat_or_west,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities,
                  color = "grey50") +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri") +
  geom_spatvector(data = dat_odf,
                  color = NA,
                  fill = pine) +
  geom_spatvector_text(data = dat_odf, 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

# NW OR

vis = 
  ggplot() +
  geom_spatvector(data = dat_or_northwest,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities %>% filter(City != "Medford"),
                  color = "grey50") +
  geom_spatvector_text(data = dat_cities %>% filter(City != "Medford"),
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri") +
  geom_spatvector(data = dat_odf,
                  color = NA,
                  fill = pine) +
  geom_spatvector_text(data = dat_odf, 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

# Export

ggsave("out/vis_boundaries.png",
       vis,
       dpi = 300,
       width = 3.75,
       height = 3.75,
       bg = NULL)
