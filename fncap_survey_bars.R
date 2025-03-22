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

#   Set boundaries to raster definition.

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
  sapp(fun = function(x){ x[x < 0] <- NA; return(x) }) %>% 
  mean(na.rm = TRUE)

dat_gcm_cascades = 
  "data/2023_Dyeetal_Fire/Data/Data/OR West Cascades" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/OR West Cascades/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  sapp(fun = function(x){ x[x < 0] <- NA; return(x) }) %>% 
  mean(na.rm = TRUE) %>% 
  resample(dat_gcm_coast)

dat_gcm_lowlands = 
  "data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands" %>% 
  list.files %>% 
  paste0("data/2023_Dyeetal_Fire/Data/Data/Olympic and Puget Lowlands/", .) %>% 
  rast %>% 
  subset("BP_Baseline", negate = TRUE) %>% 
  sapp(fun = function(x){ x[x < 0] <- NA; return(x) }) %>% 
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

dat_gcm = dat_gcm %>% resample(dat_baseline) %>% `*` (dat_baseline)

dat_baseline = dat_baseline * dat_gcm

#  Aggregate to (1) "statewide" estimate (2) state forests estimate (3) state forest estimates.

