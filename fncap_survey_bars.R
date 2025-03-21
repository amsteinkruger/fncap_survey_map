# Get a map of Oregon's state forests with wildfire probabilities for two periods and two policies.

# Options

options(scipen = 999)

# Packages

library(tidyverse)
library(magrittr)
library(terra)
library(tidyterra)

# Data

#  State Boundaries

dat_or = "data/cb_2023_us_state_500k" %>% vect %>% filter(STUSPS == "OR") %>% project("epsg:2992")

#  State Forests

dat_odf = "data/ODF.gdb" %>% vect %>% aggregate

#  Wildfire Probabilities

#   Baseline

dat_base_coast = "data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/BP_baseline.tif" %>% rast
dat_base_cascades = "data/2023_Dyeetal_Fire/Data/Data/OR West Cascades/BP_baseline.tif" %>% rast
dat_base_lowlands = "data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands/BP_baseline.tif" %>% rast

#   GCM Mean

dat_gcm_mean = 
  "data/2023_Dyeetal_Fire/Data/Data/OR Coast Range" %>% 
  list.files %>% 
  tibble(files = .) %>% 
  mutate(files_long = 
           files %>% 
           map(~ paste0("data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/", .)),
         data = 
           files_long %>% 
           map(rast) %>% 
           map(magrittr::extract)) %>% 
  unnest(data) %>% 
  select(data) %>% 
  as.data.frame %>% # This is not a clever approach to the single-column-matrix problem.
  filter(!is.na(data) & data > 0) %>% 
  as_tibble %>% 
  summarize(mean = mean(data)) %>% 
  pull(mean)

# GCM Mean Raster

# Experiment: read as stacked raster, take mean, then combine in tidy format

dat_test = 
"data/2023_Dyeetal_Fire/Data/Data/OR Coast Range" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  rename(BP = 1) # almost, but look at terra docs to rename var across all layers in a stack?
  filter() # sentinel values to NA
  mean(na.rm = TRUE) # this works if sentinel values are fixed


# This works, but it's fragile.

dat_gcm_raster = 
  # Get names.
  tibble(coast = "data/2023_Dyeetal_Fire/Data/Data/OR Coast Range" %>% list.files %>% paste0("data/2023_Dyeetal_Fire/Data/Data/OR Coast Range/", .),
         cascades = "data/2023_Dyeetal_Fire/Data/Data/OR West Cascades" %>% list.files %>% paste0("data/2023_Dyeetal_Fire/Data/Data/OR West Cascades/", .),
         lowlands = "data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands" %>% list.files %>% paste0("data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands/", .)) %>% 
  # Clean out baseline estimates.
  filter(str_sub(coast, -15, -1) != "BP_Baseline.tif") %>% 
  # Get files into compatible spatial definitions.
  mutate(coast = 
           coast %>% 
           map(rast) %>% 
           map(~ rename(., var = 1)) %>% 
           map(~ filter(., var > 0)),
         cascades = 
           cascades %>% 
           map(rast) %>% 
           map(~ rename(., var = 1)) %>% 
           map(~ filter(., var > 0)) %>% 
           map2(.x = .,
                .y = coast,
                .f = ~ resample(.x, .y)),
         lowlands = 
           lowlands %>% 
           map(rast) %>% 
           map(~ rename(., var = 1)) %>% 
           map(~ filter(., var > 0)) %>% 
           map2(.x = .,
                .y = coast,
                .f = ~ resample(.x, .y))) %>% 
  # Get a single set of rasters.
  mutate(merge_1 = 
           map2(.x = coast,
                .y = cascades,
                .f = ~ merge(.x, .y)),
         merge_2 = 
           map2(.x = merge_1,
                .y = lowlands,
                .f = ~ merge(.x, .y))) %>% 
  # Set up for aggregation.
  select(out = merge_2)

  # then pull
  # then merge/mean
  # then project
  
  
  pull(data)

dat_gcm_raster = 
  mean(dat_gcm_raster_list[[1]], 
       dat_gcm_raster_list[[2]], 
       dat_gcm_raster_list[[3]], 
       dat_gcm_raster_list[[4]], 
       dat_gcm_raster_list[[5]], 
       dat_gcm_raster_list[[6]], 
       dat_gcm_raster_list[[7]], 
       dat_gcm_raster_list[[8]], 
       dat_gcm_raster_list[[9]], 
       dat_gcm_raster_list[[10]], 
       dat_gcm_raster_list[[11]], 
       dat_gcm_raster_list[[12]],
       na.rm = TRUE)

# Wrangling

# Note that resolutions differ over rasters. Resampling helps.

# Rasters (Wildfire Probabilities)

dat_dye_cascades = resample(dat_dye_cascades, dat_dye_coast)
dat_dye_lowlands = resample(dat_dye_lowlands, dat_dye_coast)

dat_dye_1 = merge(dat_dye_coast, dat_dye_cascades)
dat_dye_2 = merge(dat_dye_1, dat_dye_lowlands)

dat_dye = dat_dye_2 %>% filter(BP_Baseline > 0) %>% project("epsg:2992")

rm(dat_dye_coast, dat_dye_cascades, dat_dye_lowlands, dat_dye_1, dat_dye_2)

# Vectors (State Boundaries, State Forest Boundaries)

dat_odf_raster = dat_odf %>% rasterize(dat_dye)

dat_or_raster = dat_or %>% rasterize(dat_dye)

# Rasters, Part 2

dat_dye_less = (dat_dye * dat_odf_raster * 100) %>% trim
dat_dye_more = (dat_dye * dat_or_raster * 100) %>% trim

#  Glance at a plot with both datasets before worrying about wrangling.

#   Just state forests.

scale_max_less = dat_dye_less %>% as.matrix %>% max(na.rm = TRUE)
scale_min_less = dat_dye_less %>% as.matrix %>% min(na.rm = TRUE)

vis_check_less = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_dye_less) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, round(scale_max_less, 3)),
                       breaks = c(0, round(scale_max_less, 3) / 2, round(scale_max_less, 3)),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

scale_max_more = dat_dye_more %>% as.matrix %>% max(na.rm = TRUE)
scale_min_more = dat_dye_more %>% as.matrix %>% min(na.rm = TRUE)

vis_check_more = 
  ggplot() +
  geom_spatvector(data = dat_or) +
  geom_spatvector(data = dat_odf) +
  geom_spatraster(data = dat_dye_more) +
  scale_fill_viridis_c(na.value = NA,
                       limits = c(0, round(scale_max_more, 3)),
                       breaks = c(0, round(scale_max_more, 3) / 2, round(scale_max_more, 3)),
                       option = "turbo") +
  labs(fill = "Burn Probability (%)") +
  theme_void() +
  theme(legend.ticks = element_blank())

ggsave("out/vis_fncap_burn_less_20250307.png",
       vis_check_less,
       dpi = 300,
       width = 6.5)

ggsave("out/vis_fncap_burn_more_20250307.png",
       vis_check_more,
       dpi = 300,
       width = 6.5)
