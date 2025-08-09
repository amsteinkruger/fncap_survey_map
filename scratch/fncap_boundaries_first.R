# Get a map of Oregon's state forests.

# Include state borders, a handful of cities, and state forests with reasonably clear unit definitions. 

# Packages

library(tidyverse)
library(terra)
library(tidyterra)
library(showtext)

showtext_auto(enable = TRUE)

# Data

# Check whether ODF boundaries work. (Nope.)

# dat_odf = "data/ODF_Boundaries.lyrx" %>% vect

dat_odf_boundaries_management = "geodata_odf.gdb" %>% vect(layer = "Ownership_ManagedLands") %>% mutate(legend = "Management")
dat_odf_boundaries_forests = "geodata_odf.gdb" %>% vect(layer = "State_Forests___Forest_Boundaries") %>% mutate(legend = "State Forests")
dat_odf_boundaries_plan = "geodata_odf.gdb" %>% vect(layer = "Draft_HCP___Plan_Area_Boundary") %>% mutate(legend = "Plan Areas")
dat_odf_boundaries_conservation = "geodata_odf.gdb" %>% vect(layer = "Draft_HCP___Habitat_Conservation_Areas") %>% mutate(legend = "Habitat Conservation Areas")

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

#  State Forests, but pulling from more detailed code for earlier bar visualization

# dat_odf =
#   "data/ODF.gdb" %>%
#   vect %>%
#   select(DISTRICT) %>%
#   mutate(FOREST = case_when(DISTRICT %in% c("Tillamook", "Forest Grove") ~ "Tillamook State Forest",
#                             DISTRICT == "Astoria" ~ "Clatsop State Forest",
#                             DISTRICT == "North Cascade" ~ "Santiam State Forest",
#                             DISTRICT == "Coos" ~ "Elliott State Forest",
#                             DISTRICT == "Klamath-Lake" ~ "Gilchrist State Forest",
#                             TRUE ~ NA)) %>%
#   filter(!is.na(FOREST)) %>% 
#   filter(FOREST %in% c("Clatsop State Forest", "Tillamook State Forest", "Santiam State Forest")) %>% # 2025/05/20
#   group_by(FOREST) %>%
#   summarize() %>%
#   ungroup %>% 
#   mutate(legend = "Habitat Conservation Areas")

# Check state forest names.

# dat_odf %>% 
#   ggplot() +
#   geom_spatvector(aes(fill = FOREST), color = NA)

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
  # Cities
  #  Points
  geom_spatvector(data = dat_cities %>% filter(City != "Medford" & City != "Astoria"),
                  color = "grey50") +
  #  Text
  #   Left-Align
  geom_spatvector_text(data = dat_cities %>% filter(City != "Medford" & City != "Astoria" & City != "Salem"),
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 0,
                       vjust = 1,
                       nudge_x = 0.03,
                       nudge_y = -0.03,
                       size = 13) +
  #   Right-Align
  geom_spatvector_text(data = dat_cities %>% filter(City == "Salem"),
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       nudge_x = -0.03,
                       nudge_y = -0.03,
                       size = 13) +
  # Forests
  #  Points
  geom_spatvector(data = dat_odf,
                  color = NA,
                  fill = pine) +
  #  Text
  #   Clatsop
  geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Clatsop State Forest"), 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri",
                       hjust = 0.50,
                       vjust = 0.00,
                       nudge_x = 0.05,
                       nudge_y = 0.25,
                       size = 14) +
  #   Tillamook
  geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Tillamook State Forest"), 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri",
                       hjust = 0.00,
                       vjust = 0.50,
                       nudge_x = 0.10,
                       nudge_y = -0.40,
                       size = 14) +
  #   Santiam
  geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Santiam State Forest"), 
                       aes(label = FOREST),
                       color = "black",
                       family = "Calibri",
                       hjust = 0.50,
                       vjust = 1.00,
                       nudge_x = 0.10,
                       nudge_y = -0.10,
                       size = 14) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

# NW(er)

vis = 
  ggplot() +
  geom_spatvector(data = dat_or_northwester,
                  fill = "white",
                  color = "black") +
  # Cities
  #  Points
  geom_spatvector(data = dat_cities %>% filter(City == "Portland" | City == "Salem"),
                  color = "grey50") +
  #  Text
  geom_spatvector_text(data = dat_cities %>% filter(City == "Portland" | City == "Salem"),
                       aes(label = City),
                       color = "grey50",
                       family = "Calibri",
                       size = 13) +
  #   Left-Align
  # geom_spatvector_text(data = dat_cities %>% filter(City != "Medford" & City != "Astoria" & City != "Salem"),
  #                      aes(label = City),
  #                      color = "grey50",
  #                      family = "Calibri",
  #                      hjust = 0,
  #                      vjust = 1,
  #                      nudge_x = 0.03,
  #                      nudge_y = -0.03,
  #                      size = 13) +
  #   Right-Align
  # geom_spatvector_text(data = dat_cities %>% filter(City == "Salem"),
  #                      aes(label = City),
  #                      color = "grey50",
  #                      family = "Calibri",
  #                      hjust = 1,
  #                      vjust = 1,
  #                      nudge_x = -0.03,
  #                      nudge_y = -0.03,
  #                      size = 13) +
  # Forests
  #  Points
  geom_spatvector(data = dat_odf,
                  aes(fill = legend),
                  color = NA) +
  #  Text
  # geom_spatvector_text(data = dat_odf,
  #                      aes(label = FOREST),
  #                      color = "black",
  #                      family = "Calibri",
  #                      size = 15) +
  #   Clatsop
  # geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Clatsop State Forest"), 
  #                      aes(label = FOREST),
  #                      color = "black",
  #                      family = "Calibri",
  #                      hjust = 0.50,
  #                      vjust = 0.00,
  #                      nudge_x = 0.05,
  #                      nudge_y = 0.25,
  #                      size = 14) +
  #   Tillamook
  # geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Tillamook State Forest"), 
  #                      aes(label = FOREST),
  #                      color = "black",
  #                      family = "Calibri",
  #                      hjust = 0.00,
  #                      vjust = 0.50,
  #                      nudge_x = 0.10,
  #                      nudge_y = -0.40,
  #                      size = 14) +
  #   Santiam
  # geom_spatvector_text(data = dat_odf %>% filter(FOREST == "Santiam State Forest"), 
  #                      aes(label = FOREST),
  #                      color = "black",
  #                      family = "Calibri",
  #                      hjust = 0.50,
  #                      vjust = 1.00,
  #                      nudge_x = 0.10,
  #                      nudge_y = -0.10,
  #                      size = 14) +
  scale_fill_manual(values = pine) +
  coord_sf(ylim = c(44.5, 46.25)) +
  theme_void() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 32),
        legend.title = element_blank(),
        panel.background = element_rect(colour = "black", fill = NA))

# Export

ggsave("out/vis_boundaries.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = 3.25,
       bg = NULL)
