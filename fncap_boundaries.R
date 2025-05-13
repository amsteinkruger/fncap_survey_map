# Get a map of Oregon's state forests.

# Include state borders, a handful of cities, and state forests with reasonably clear unit definitions. Worry about details later.

# Packages

library(tidyverse)
library(terra)
library(tidyterra)

# Data

#  State Boundaries

dat_or = "data/cb_2023_us_state_500k" %>% vect %>% filter(STUSPS == "OR") %>% project("epsg:2992")

#  State Forests

dat_odf = "data/ODF.gdb" %>% vect %>% aggregate

#  State Forests, but pulling from more detailed code for earlier bar visualization

# dat_bars_3_odf = 
#   "data/ODF.gdb" %>% 
#   vect %>% 
#   select(DISTRICT, ACRES) %>% 
#   filter(DISTRICT %in% c("Tillamook", "Astoria", "Forest Grove", "North Cascade")) %>% 
#   mutate(FOREST = case_when(DISTRICT %in% c("Tillamook", "Forest Grove") ~ "Tillamook State Forest",
#                             DISTRICT == "Astoria" ~ "Clatsop State Forest",
#                             DISTRICT == "North Cascade" ~ "Santiam State Forest")) %>% 
#   group_by(FOREST) %>% 
#   summarize(ACRES = sum(ACRES, na.rm = TRUE)) %>% 
#   ungroup

# Wrangle

# Visualize

vis = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf)

# Export