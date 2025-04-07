# Problem: mean of GCM estimates returns lower maximum, mean, etc. than baseline. Could be (1) spatial processing issue or (2) oddball GCMs.

# Get a map of Oregon's state forests with wildfire probabilities for two periods and two policies.

# Options

options(scipen = 999)

# Packages

library(tidyverse)
library(ggpubr)
library(magrittr)
library(terra)
library(tidyterra)

# Data

#  State Boundaries

dat_or = "data/cb_2023_us_state_500k" %>% vect %>% filter(STUSPS == "OR") %>% project("epsg:2992")

#  State Forests

dat_odf = "data/ODF.gdb" %>% vect %>% aggregate

#  Wildfire Probabilities

#   Baseline (1)

dat_base_coast = "data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/BP_baseline.tif" %>% rast

dat_base_cascades = 
  "data/2023_Dyeetal_Fire/Data/Data/OR West Cascades/BP_baseline.tif" %>% 
  rast %>% 
  resample(dat_base_coast)

dat_base_lowlands = 
  "data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands/BP_baseline.tif" %>% 
  rast %>% 
  resample(dat_base_coast)

dat_baseline = 
  dat_base_coast %>% 
  merge(dat_base_cascades) %>% 
  merge(dat_base_lowlands) %>% 
  filter(BP_Baseline > 0) %>% 
  project("epsg:2992") 

#   Rasterize boundaries.

dat_or_raster = dat_or %>% rasterize(dat_baseline)

dat_odf_raster = dat_odf %>% rasterize(dat_baseline)

#   Baseline (2)

dat_baseline = 
  dat_baseline %>% 
  `*` (dat_or_raster) %>% 
  trim %>% 
  `*` (100)

rm(dat_base_coast, dat_base_cascades, dat_base_lowlands)

#   GCM Mean

dat_gcm_coast = 
  "data/2023_Dyeetal_Fire/Data/Data/OR Coast Range" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  sapp(fun = function(x){ x[x <= 0] <- NA; return(x) }) %>% 
  mean(na.rm = TRUE)

dat_gcm_cascades = 
  "data/2023_Dyeetal_Fire/Data/Data/OR West Cascades" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/OR West Cascades/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  sapp(fun = function(x){ x[x <= 0] <- NA; return(x) }) %>% 
  mean(na.rm = TRUE) %>% 
  resample(dat_gcm_coast)

dat_gcm_lowlands = 
  "data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  sapp(fun = function(x){ x[x <= 0] <- NA; return(x) }) %>% 
  mean(na.rm = TRUE) %>% 
  resample(dat_gcm_coast)

dat_gcm = 
  dat_gcm_coast %>% 
  merge(dat_gcm_cascades) %>% 
  merge(dat_gcm_lowlands) %>% 
  project("epsg:2992") %>% 
  `*` (dat_or_raster) %>% 
  trim %>% 
  `*` (100)

rm(dat_gcm_coast, dat_gcm_cascades, dat_gcm_lowlands)

gc()

#  Keep cells with values in both wrangled rasters.

dat_baseline_dummy = dat_baseline / dat_baseline

dat_gcm_dummy = dat_gcm / dat_gcm

dat_gcm = dat_gcm %>% resample(dat_baseline) %>% `*` (dat_baseline_dummy)

dat_baseline = dat_baseline * dat_gcm_dummy

#  Keep cells in state forests.

dat_baseline_odf = dat_baseline * resample(dat_odf_raster, dat_baseline)
dat_gcm_odf = dat_gcm * resample(dat_odf_raster, dat_baseline)

#  Plot.

#   Maps

#    Handle scale limits and breaks.

scale_max_baseline = dat_baseline %>% as.matrix %>% max(na.rm = TRUE) %>% ceiling
scale_min_baseline = dat_baseline %>% as.matrix %>% min(na.rm = TRUE) %>% floor

scale_max_gcm = dat_gcm %>% as.matrix %>% max(na.rm = TRUE) %>% ceiling
scale_min_gcm = dat_gcm %>% as.matrix %>% min(na.rm = TRUE) %>% floor

scale_max = max(scale_max_baseline, scale_max_gcm)
scale_min = min(scale_min_baseline, scale_min_gcm)

#    First, plot both sets of probabilities without limiting the extent to state forests.

vis_baseline = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_baseline) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, scale_max),
                       breaks = c(0, scale_max / 2, scale_max),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

vis_gcm = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_gcm) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, scale_max),
                       breaks = c(0, scale_max / 2, scale_max),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

vis_both = ggarrange(vis_baseline, vis_gcm, common.legend = TRUE) + theme(legend.position = "bottom")

#    Second, plot both sets of probabilities only within state forest boundaries.

vis_baseline_odf = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_baseline_odf) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, scale_max),
                       breaks = c(0, scale_max / 2, scale_max),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

vis_gcm_odf = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_gcm_odf) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, scale_max),
                       breaks = c(0, scale_max / 2, scale_max),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

vis_both_odf = ggarrange(vis_baseline_odf, vis_gcm_odf, common.legend = TRUE) + theme(legend.position = "bottom")

#   Bars
#    (1) Statewide (w/in Dye et al. study area)
#    (2) State Forests
#    (3) State Forest

# (1)

dat_bars_1 = 
  tibble(which = c("Baseline", "GCM Mean"), 
         value = c(pull(global(dat_baseline, mean, na.rm = TRUE)), pull(global(dat_gcm, mean, na.rm = TRUE)))) %>% 
  mutate(which = which %>% factor %>% fct_rev)

vis_bars_1 = 
  dat_bars_1 %>% 
  ggplot() +
  geom_col(aes(x = which,
               y = value)) +
  coord_flip() +
  labs(x = NULL, y = "Mean Annual Burn Probabilities (%), All OR Study Area")

# (2)

dat_bars_2 = 
  tibble(which = c("Baseline", "GCM Mean"), 
         value = c(pull(global(dat_baseline_odf, mean, na.rm = TRUE)), pull(global(dat_gcm_odf, mean, na.rm = TRUE)))) %>% 
  mutate(which = which %>% factor %>% fct_rev)

vis_bars_2 = 
  dat_bars_2 %>% 
  ggplot() +
  geom_col(aes(x = which,
               y = value)) +
  coord_flip() +
  labs(x = NULL, y = "Mean Annual Burn Probabilities (%), OR State Forests")

# (3)

# Get state forests.

# Note that this code is pretty fragile.

dat_bars_3_odf = 
  "data/ODF.gdb" %>% 
  vect %>% 
  select(DISTRICT, ACRES) %>% 
  filter(DISTRICT %in% c("Tillamook", "Astoria", "Forest Grove", "North Cascade")) %>% 
  mutate(FOREST = case_when(DISTRICT %in% c("Tillamook", "Forest Grove") ~ "Tillamook State Forest",
                            DISTRICT == "Astoria" ~ "Clatsop State Forest",
                            DISTRICT == "North Cascade" ~ "Santiam State Forest")) %>% 
  group_by(FOREST) %>% 
  summarize(ACRES = sum(ACRES, na.rm = TRUE)) %>% 
  ungroup

dat_bars_3_baseline = dat_bars_2_odf %>% extract(dat_baseline, ., mean, na.rm = TRUE)
dat_bars_3_gcm = dat_bars_2_odf %>% extract(dat_gcm, ., mean, na.rm = TRUE)

dat_bars_3 = 
  left_join(dat_bars_3_baseline, dat_bars_3_gcm) %>% 
  mutate(Forest = case_when(ID == 1 ~ "Clatsop",
                            ID == 2 ~ "Santiam",
                            ID == 3 ~ "Tillamook")) %>% 
  rename(Baseline = BP_Baseline,
         'GCM Mean' = mean) %>% 
  pivot_longer(cols = c(Baseline, 'GCM Mean'))

vis_bars_3 = 
  dat_bars_3 %>% 
  ggplot() +
  geom_col(aes(x = Forest,
               y = value,
               fill = name),
           position = "dodge2") +
  coord_flip()

