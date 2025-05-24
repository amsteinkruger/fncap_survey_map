# Get a map of Oregon's state forests.

# Include state borders, a handful of cities, and state forests with reasonably clear unit definitions. 

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(showtext)

showtext_auto(enable = TRUE)

# Data

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

dat_or_northwester = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  crop(ext(-125.0, -121.5, 44.5, 48.0))

#  Get forest boundaries, etc.

dat_odf_boundaries_management = "data/geodata_odf.gdb" %>% vect(layer = "Ownership_ManagedLands") %>% mutate(legend = "Management") %>% project(crs(dat_or))
dat_odf_boundaries_forests = "data/geodata_odf.gdb" %>% vect(layer = "State_Forests___Forest_Boundaries") %>% mutate(legend = "State Forests") %>% project(crs(dat_or))
dat_odf_boundaries_plan = "data/geodata_odf.gdb" %>% vect(layer = "Draft_HCP___Plan_Area_Boundary") %>% mutate(legend = "Plan Areas") %>% project(crs(dat_or))
dat_odf_boundaries_conservation = "data/geodata_odf.gdb" %>% vect(layer = "Draft_HCP___Habitat_Conservation_Areas") %>% mutate(legend = "Habitat Conservation Areas") %>% project(crs(dat_or))

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

vis_conservation_all = 
  ggplot() +
  geom_spatvector(data = dat_or,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities,
                  color = "grey50",
                  size = 1) +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       size = 10) +
  geom_spatvector(data = dat_odf_boundaries_conservation,
                  color = NA,
                  fill = pine) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

ggsave("out/vis_conservation_all.png",
       vis_conservation_all,
       dpi = 300,
       width = 3.25,
       height = 3.25,
       bg = NULL)

# W OR

vis_conservation_west = 
  ggplot() +
  geom_spatvector(data = dat_or_west,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities,
                  color = "grey50",
                  size = 1) +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       size = 10) +
  geom_spatvector(data = dat_odf_boundaries_conservation,
                  color = NA,
                  fill = pine) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

ggsave("out/vis_conservation_west.png",
       vis_conservation_west,
       dpi = 300,
       width = 3.25,
       height = 3.25,
       bg = NULL)

# NW OR

vis_conservation_northwest = 
  ggplot() +
  geom_spatvector(data = dat_or_northwest,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities %>% filter(City != "Medford" & City != "Astoria"),
                  color = "grey50",
                  size = 1) +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       size = 10) +
  geom_spatvector(data = dat_odf_boundaries_conservation %>% crop(dat_or_northwest),
                  color = NA,
                  fill = pine) +
  coord_sf(ylim = c(43.5, 46.25)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

ggsave("out/vis_conservation_northwest.png",
       vis_conservation_northwest,
       dpi = 300,
       width = 3.25,
       height = 3.25,
       bg = NULL)

# NW(er)

vis_conservation_northwester = 
  ggplot() +
  geom_spatvector(data = dat_or_northwester,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities %>% filter(City == "Portland" | City == "Salem"),
                  color = "grey50",
                  size = 1) +
  geom_spatvector_text(data = dat_cities %>% filter(City == "Portland" | City == "Salem"),
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       size = 10) +
  geom_spatvector(data = dat_odf_boundaries_conservation %>% crop(dat_or_northwester),
                  color = NA,
                  fill = pine) +
  coord_sf(ylim = c(44.5, 46.25)) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

ggsave("out/vis_conservation_northwester.png",
       vis_conservation_northwester,
       dpi = 300,
       width = 3.25,
       height = 3.25,
       bg = NULL)
