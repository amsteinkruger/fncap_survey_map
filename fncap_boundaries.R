# Get a map of Oregon's state forests.

# Include state borders, a handful of cities, and state forests with reasonably clear unit definitions. Worry about details later.

# Packages

library(tidyverse)
library(terra)
library(tidyterra)

# Data

#  Get cities, then subset to Astoria, Portland, Salem, Eugene, Bend, and Medford.

dat_cities = 
  "data/Cities_OR.gdb" %>% 
  vect %>% 
  filter(CITY %in% c("ASTORIA", "PORTLAND", "SALEM", "EUGENE", "BEND", "MEDFORD")) %>% 
  mutate(City = CITY %>% str_to_title)

#  Get state boundaries, then subset to 121W.

dat_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") %>% 
  crop(ext(-125, -121, 40, 48)) %>% 
  project("epsg:2992")

#  State Forests

dat_odf = "data/ODF.gdb" %>% vect %>% aggregate

#  State Forests, but pulling from more detailed code for earlier bar visualization

dat_odf_disagg =
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
  group_by(FOREST) %>%
  summarize() %>%
  ungroup

# visualize names of districts to map state forest names onto districts or parts of districts

# dat_odf_disagg %>% 
#   ggplot() +
#   geom_spatvector(aes(fill = DISTRICT), color = NA)


dat_odf_disagg %>% 
  ggplot() +
  geom_spatvector(aes(fill = FOREST), color = NA)

# Wrangle

# Visualize
#  use colorbrewer greens for multiple state forests w/ outside borders in black and labels to left?

vis = 
  ggplot() +
  geom_spatvector(data = dat_or,
                  fill = "white",
                  color = "black") +
  geom_spatvector(data = dat_cities,
                  color = "grey50") +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       color = "grey50") +
  geom_spatvector(data = dat_odf_disagg,
                  aes(fill = FOREST),
                  color = NA) +
  geom_spatvector_text(data = dat_odf_disagg, 
                       aes(label = FOREST),
                       color = "black") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(colour = "black", fill = NA))

# Export

ggsave("out/vis_boundaries.png",
       vis,
       dpi = 300,
       width = 3.25,
       height = )


