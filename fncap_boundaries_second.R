# Get maps of Oregon's state forests and habitat conservation areas.

# Packages

library(tidyverse)
library(ggpubr)
library(patchwork)
library(terra)
library(tidyterra)
library(showtext)

showtext_auto(enable = TRUE)

# Data

#  Get state boundaries, then subset.

dat_or = 
  "data/cb_2023_us_state_500k" %>% 
  vect %>% 
  filter(STUSPS == "OR") # %>% 
# project("epsg:2992")

dat_or_west = 
  dat_or %>% 
  crop(ext(-125, -121, 40, 48))

dat_or_northwest = 
  dat_or %>% 
  crop(ext(-125.0, -120.5, 43.5, 48.0))

dat_or_northwester = 
  dat_or %>% 
  crop(ext(-125.0, -121.5, 44.5, 48.0))

#  Get the EPA Ecoregion (3) for the Cascades.

dat_cascades = 
  "data/EPA Ecoregions/us_eco_l3/us_eco_l3.shp" %>% 
  vect %>% 
  filter(US_L3NAME == "Cascades") %>% 
  project(crs(dat_or)) %>% 
  crop(dat_or)

# Set up a specific inset for later.

dat_inset_background = dat_or
dat_inset_ext = vect(ext(dat_or_northwester), crs = crs(dat_or_northwester))

vis_inset = 
  ggplot() + 
  geom_spatvector(data = dat_or,
                  fill = "white",
                  color = "grey50") +
  geom_spatvector(data = dat_or %>% crop(dat_or_northwester),
                  fill = "grey75",
                  color = "grey50") +
  geom_spatvector(data = dat_inset_ext,
                  fill = NA,
                  color = "black") +
  geom_spatvector_text(data = dat_or,
                       aes(label = NAME),
                       color = "black",
                       size = 10,
                       family = "Calibri",
                       nudge_y = -0.25) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "black"))

#  Get cities, then subset to Astoria, Portland, Salem, Eugene, Bend, and Medford.

dat_cities = 
  "data/Cities_OR.gdb" %>% 
  vect %>% 
  filter(CITY %in% c("ASTORIA", "PORTLAND", "SALEM", "EUGENE", "BEND", "MEDFORD")) %>% 
  mutate(City = CITY %>% str_to_title) %>% 
  project(crs(dat_or))

#  Get forest boundaries, etc.

dat_odf_boundaries_management = 
  "data/geodata_odf.gdb" %>% 
  vect(layer = "Ownership_ManagedLands") %>% 
  project(crs(dat_or)) %>% 
  mutate(legend = 
           "Management" %>% 
           factor(levels = c("Management", "Forests", "Plan", "Conservation"), 
                  labels = c("Managed Areas", "State Forests", "Plan Areas", "Conservation Areas")))

dat_odf_boundaries_forests = 
  "data/geodata_odf.gdb" %>% 
  vect(layer = "State_Forests___Forest_Boundaries") %>% 
  project(crs(dat_or)) %>% 
  mutate(legend = 
           "Forests" %>% 
           factor(levels = c("Management", "Forests", "Plan", "Conservation"), 
                  labels = c("Managed Areas", "State Forests", "Plan Areas", "Conservation Areas")))

dat_odf_boundaries_plan = 
  "data/geodata_odf.gdb" %>% 
  vect(layer = "Draft_HCP___Plan_Area_Boundary") %>% 
  project(crs(dat_or)) %>% 
  mutate(legend = 
           "Plan" %>% 
           factor(levels = c("Management", "Forests", "Plan", "Conservation"), 
                  labels = c("Managed Areas", "State Forests", "Plan Areas", "Conservation Areas")))

dat_odf_boundaries_conservation = 
  "data/geodata_odf.gdb" %>% 
  vect(layer = "Draft_HCP___Habitat_Conservation_Areas") %>% 
  project(crs(dat_or)) %>% 
  mutate(legend = 
           "Conservation" %>% 
           factor(levels = c("Management", "Forests", "Plan", "Conservation"), 
                  labels = c("Managed Areas", "State Forests", "Plan Areas", "Conservation Areas")))

# Visualize

#  Use Oregon State's secondary colors.

pine = "#4A773C"
moss = "#C4D6A4"
bine = "#AA9D2E"
tide = "#00859B"
luminance = "#FFB500"
stratosphere = "#006A8E"

palette = c("Managed Areas" = pine, "State Forests" = tide, "Plan Areas" = luminance, "Conservation Areas" = stratosphere)

#  Use Calibri. 

font_add(family = "Calibri", regular = "C:/Windows/Fonts/calibri.ttf")

#  Set up elements.

vis_theme = 
  theme_void() +
  theme(text = element_text(family = "Calibri", size = 15),
        legend.text = element_text(size = 30),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.background = element_rect(colour = "black", fill = NA))

vis_base_all = 
  ggplot() +
  geom_spatvector(data = dat_or,
                  fill = "white") +
  geom_spatvector(data = dat_cities,
                  size = 1) +
  geom_spatvector_text(data = dat_cities,
                       aes(label = City),
                       family = "Calibri",
                       hjust = 0,
                       vjust = 1,
                       size = 10) +
  scale_fill_manual(values = palette)

vis_base_west = 
  ggplot() +
  geom_spatvector(data = dat_or_west,
                  fill = "white") +
  geom_spatvector(data = dat_cities %>% crop(dat_or_west),
                  size = 1) +
  geom_spatvector_text(data = dat_cities %>% crop(dat_or_west),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 0,
                       vjust = 1,
                       size = 10) +
  scale_fill_manual(values = palette)

vis_base_northwest = 
  ggplot() +
  geom_spatvector(data = dat_or_northwest,
                  fill = "white") +
  geom_spatvector(data = dat_cities %>% crop(dat_or_northwest),
                  size = 1) +
  geom_spatvector_text(data = dat_cities %>% crop(dat_or_northwest),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 0,
                       vjust = 1,
                       size = 10) +
  scale_fill_manual(values = palette)

#  3x4 of all boundaries at first three scales (all, west, northwest).

#   Management

vis_management_all = 
  vis_base_all + 
  geom_spatvector(data = dat_odf_boundaries_management,
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_management_west = 
  vis_base_west + 
  geom_spatvector(data = dat_odf_boundaries_management %>% crop(dat_or_west),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_management_northwest = 
  vis_base_northwest + 
  geom_spatvector(data = dat_odf_boundaries_management %>% crop(dat_or_northwest),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

#   Forests

vis_forests_all = 
  vis_base_all + 
  geom_spatvector(data = dat_odf_boundaries_forests,
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_forests_west = 
  vis_base_west + 
  geom_spatvector(data = dat_odf_boundaries_forests %>% crop(dat_or_west),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_forests_northwest = 
  vis_base_northwest + 
  geom_spatvector(data = dat_odf_boundaries_forests %>% crop(dat_or_northwest),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

#   Plan

vis_plan_all = 
  vis_base_all + 
  geom_spatvector(data = dat_odf_boundaries_plan,
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_plan_west = 
  vis_base_west + 
  geom_spatvector(data = dat_odf_boundaries_plan %>% crop(dat_or_west),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_plan_northwest = 
  vis_base_northwest + 
  geom_spatvector(data = dat_odf_boundaries_plan %>% crop(dat_or_northwest),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

#   Conservation

vis_conservation_all = 
  vis_base_all + 
  geom_spatvector(data = dat_odf_boundaries_conservation,
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_conservation_west = 
  vis_base_west + 
  geom_spatvector(data = dat_odf_boundaries_conservation %>% crop(dat_or_west),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

vis_conservation_northwest = 
  vis_base_northwest + 
  geom_spatvector(data = dat_odf_boundaries_conservation %>% crop(dat_or_northwest),
                  aes(fill = legend),
                  color = NA) +
  vis_theme

#   Combine

vis_patch = 
  ((vis_management_all / vis_management_west / vis_management_northwest) + plot_layout(heights = c(1, 1, 1)) |
     (vis_forests_all / vis_forests_west / vis_forests_northwest) + plot_layout(heights = c(1, 1, 1)) |
     (vis_plan_all / vis_plan_west / vis_plan_northwest) + plot_layout(heights = c(1, 1, 1)) |
     (vis_conservation_all / vis_conservation_west / vis_conservation_northwest) + plot_layout(heights = c(1, 1, 1))) /
  guide_area() +
  plot_layout(guides = "collect",
              heights = c(12, 1)) &
  theme(legend.title = element_blank(),
        # legend.text = element_text(size = 10),
        legend.position = "bottom",
        legend.direction = "horizontal")

ggsave("out/vis_patch.png",
       vis_patch,
       dpi = 300,
       width = 8,
       # height = 3.25,
       bg = NULL)

# HCP/HCA boundaries in a 1x1 w/ inset and legend at "northwester" scale

palette_more = c("Habitat Conservation Plan Extent" = moss, "Habitat Conservation Areas" = pine)

vis_more_base = 
  ggplot() +
  geom_spatvector(data = dat_or_northwester,
                  fill = "white") +
  geom_spatvector(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                  size = 1) +
  geom_spatvector_text(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       nudge_x = -0.024,
                       nudge_y = -0.024,
                       size = 12) +
  geom_spatvector(data = 
                    dat_odf_boundaries_plan %>% 
                    crop(dat_or_northwester) %>% 
                    mutate(legend_more = 
                             "Habitat Conservation Plan Extent" %>% 
                             factor(levels = c("Habitat Conservation Plan Extent", "Habitat Conservation Areas"),
                                    labels = c("Habitat Conservation Plan Extent", "Habitat Conservation Areas"))),
                  aes(fill = legend_more),
                  color = NA) +
  geom_spatvector(data = 
                    dat_odf_boundaries_conservation %>% 
                    crop(dat_or_northwester) %>% 
                    mutate(legend_more = 
                             "Habitat Conservation Areas" %>% 
                             factor(levels = c("Habitat Conservation Plan Extent", "Habitat Conservation Areas"),
                                    labels = c("Habitat Conservation Plan Extent", "Habitat Conservation Areas"))),
                  aes(fill = legend_more),
                  color = NA) +
  scale_fill_manual(values = palette_more) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 30, family = "Calibri"),
        legend.position = "bottom",
        legend.direction = "vertical")

vis_more_inset = 
  vis_more_base + inset_element(vis_inset, left = 0.63, bottom = 0.63, right = 0.955, top = 1)

ggsave("out/vis_more.png",
       vis_more_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)

# State Forests, Monochrome

vis_forests_monochrome_base = 
  ggplot() +
  geom_spatvector(data = dat_or_northwester,
                  fill = "white") +
  geom_spatvector(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                  size = 1) +
  geom_spatvector(data = 
                    dat_odf_boundaries_plan %>% 
                    crop(dat_or_northwester) %>% 
                    mutate(legend_more = "HCP State Forest Land"),
                  aes(fill = legend_more),
                  color = NA) +
  # Segments?
  # Text | Cities
  geom_spatvector_text(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       nudge_x = -0.024,
                       nudge_y = -0.024,
                       size = 12) +
  # Text | Forests, Clatsop
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwester) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 1),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.05,
                       nudge_y = 0.15) + 
  # Text | Forests, Tillamook
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwester) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 3),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.20,
                       nudge_y = -0.50) + 
  # Text | Forests, Santiam
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwester) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 2),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.15,
                       nudge_y = 0.15) + 
  scale_fill_manual(values = pine) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 30, family = "Calibri"),
        legend.position = "bottom",
        legend.direction = "vertical")

vis_forests_monochrome_inset = 
  vis_forests_monochrome_base + inset_element(vis_inset, left = 0.63, bottom = 0.63, right = 0.955, top = 1)

ggsave("out/vis_forests_monochrome.png",
       vis_forests_monochrome_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)

ggsave("out/vis_forests_monochrome.jpeg",
       vis_forests_monochrome_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)

# State Forests, Trichrome

palette_trichrome = c(moss, pine, bine)

vis_forests_trichrome_base = 
  ggplot() +
  geom_spatvector(data = dat_or_northwester,
                  fill = "white") +
  geom_spatvector(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                  size = 1) +
  geom_spatvector(data = 
                    dat_odf_boundaries_forests %>% 
                    crop(dat_or_northwester) %>% 
                    mutate(legend_more = 
                             case_when(forestname == 1 ~ "Clatsop State Forest",
                                       forestname == 2 ~ "Santiam State Forest",
                                       forestname == 3 ~ "Tillamook State Forest")),
                  aes(fill = legend_more),
                  color = NA) +
  # Text | Cities
  geom_spatvector_text(data = dat_cities %>% filter(City %in% c("Portland", "Salem")),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       nudge_x = -0.024,
                       nudge_y = -0.024,
                       size = 12) +
  # scale_fill_manual(values = palette_trichrome) +
  scale_fill_brewer(palette = "Greens",
                    limits = c("1", "2", "3", "Clatsop State Forest", "Santiam State Forest", "Tillamook State Forest"),
                    breaks = c("Clatsop State Forest", "Santiam State Forest", "Tillamook State Forest")) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 30, family = "Calibri"),
        legend.position = "bottom",
        legend.direction = "vertical")

vis_forests_trichrome_inset = 
  vis_forests_trichrome_base + inset_element(vis_inset, left = 0.63, bottom = 0.63, right = 0.955, top = 1)

ggsave("out/vis_forests_trichrome.png",
       vis_forests_trichrome_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)

# Bonus Area Calculations:

area_management = 
  dat_odf_boundaries_management %>% 
  mutate(Area_ha = expanse(., unit = "ha"),
         Area_ac = Area_ha * 2.47105) %>% 
  pull(Area_ac) %>% 
  sum

area_forests = 
  dat_odf_boundaries_forests %>% 
  mutate(Area_ha = expanse(., unit = "ha"),
         Area_ac = Area_ha * 2.47105) %>% 
  pull(Area_ac) %>% 
  sum

area_plan = 
  dat_odf_boundaries_plan %>% 
  mutate(Area_ha = expanse(., unit = "ha"),
         Area_ac = Area_ha * 2.47105) %>% 
  pull(Area_ac) %>% 
  sum

area_conservation = 
  dat_odf_boundaries_conservation %>% 
  mutate(Area_ha = expanse(., unit = "ha"),
         Area_ac = Area_ha * 2.47105) %>% 
  pull(Area_ac) %>% 
  sum

area_ratio = area_conservation / area_plan

# 2025/07/13 Update:
#  Monochrome; include Eugene and Bend; include Cascades.

vis_update_base = 
  ggplot() +
  geom_spatvector(data = dat_or_northwest,
                  fill = "white") +
  geom_spatvector(data = dat_cascades %>% crop(dat_or_northwest),
                  fill = "gray50",
                  color = NA) +
  geom_spatvector(data = dat_cities %>% filter(City %in% c("Portland", "Salem", "Eugene", "Bend")),
                  size = 1) +
  geom_spatvector(data = 
                    dat_odf_boundaries_plan %>% 
                    crop(dat_or_northwest) %>% 
                    mutate(legend_more = "HCP State Forest Land"),
                  aes(fill = legend_more),
                  color = NA) +
  # Segments?
  # Text | Cities
  geom_spatvector_text(data = dat_cities %>% filter(City %in% c("Portland", "Salem", "Eugene", "Bend")),
                       aes(label = City),
                       family = "Calibri",
                       hjust = 1,
                       vjust = 1,
                       nudge_x = -0.024,
                       nudge_y = -0.024,
                       size = 12) +
  # Text | Forests, Clatsop
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwest) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 1),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.05,
                       nudge_y = 0.15) + 
  # Text | Forests, Tillamook
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwest) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 3),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.20,
                       nudge_y = -0.50) + 
  # Text | Forests, Santiam
  geom_spatvector_text(data = dat_odf_boundaries_forests %>% 
                         crop(dat_or_northwest) %>% 
                         mutate(name = 
                                  case_when(forestname == 1 ~ "Clatsop\nState Forest",
                                            forestname == 3 ~ "Tillamook\n State Forest",
                                            forestname == 2 ~ "Santiam\nState Forest") %>% 
                                  factor) %>% 
                         filter(forestname == 2),
                       aes(label = name),
                       size = 13,
                       family = "Calibri",
                       hjust = 0,
                       vjust = 0,
                       lineheight = 0.25,
                       nudge_x = 0.15,
                       nudge_y = 0.15) + 
  scale_fill_manual(values = pine) +
  guides(fill = guide_legend(override.aes = list(linetype = 0)),
         color = guide_legend(override.aes = list(linetype = 0))) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 30, family = "Calibri"),
        legend.position = "bottom",
        legend.direction = "vertical")

vis_update_inset = 
  vis_update_base + inset_element(vis_inset, left = 0.63, bottom = 0.63, right = 0.955, top = 1)

ggsave("out/vis_update.png",
       vis_update_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)

ggsave("out/vis_update.jpeg",
       vis_update_inset,
       dpi = 300,
       width = 3.25,
       height = 4.00,
       bg = NULL)
