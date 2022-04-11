# -------------------------------------------------------------------------
# landscape metrics
# mauricio vancine
# mauricio.vancine@gmail.com
# 26-07-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(fasterize)
library(landscapemetrics)
library(landscapetools)
library(sf)
library(tidyverse)

# directory
path <- "/home/mude/data/gitlab/r-landscape-ecology/01_data/rio_claro_sp"
setwd(path)
getwd()
dir()

# import data -------------------------------------------------------------
# import
rc <- sf::read_sf("SP_3543907_USO.shp")
rc

ggplot() + 
  geom_sf(data = rc, aes(fill = CLASSE_USO), color = NA) + 
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  labs(x = "Longitude", y = "Latitude", fill = "Classes") +
  theme_bw()

# create number coloumn
rc <- rc %>% 
  dplyr::mutate(class = seq(5))
rc

# rasterize ---------------------------------------------------------------
# create raster
ra <- fasterize::raster(rc, res = 90)
ra

# rasterize
rc_raster <- fasterize::fasterize(sf = rc, raster = ra, field = "class")
rc_raster

# plot
fasterize::plot(rc_raster)

# ggplot
ggplot() +
  geom_raster(data = raster::rasterToPoints(rc_raster) %>% tibble::as_tibble(), 
              aes(x, y, fill = factor(layer))) +
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  labs(x = "Longitude", y = "Latitude", fill = "Classes") +
  theme_bw()

# landscapetools
landscapetools::show_landscape(rc_raster, discrete = TRUE)

# landscape metrics -------------------------------------------------------
# check raster
landscapemetrics::check_landscape(rc_raster)

# check all metrics
all_metrics <- landscapemetrics::list_lsm()
all_metrics

# calculate for example the Euclidean nearest-neighbor distance on patch level
landscapemetrics::lsm_p_enn(rc_raster)

# calculate the total area and total class edge length
landscapemetrics::lsm_l_ta(rc_raster)
landscapemetrics::lsm_c_te(rc_raster)

# calculate all metrics on patch level
lsm_patch <- landscapemetrics::calculate_lsm(rc_raster, level = "patch", progress = TRUE)
lsm_patch

# Plot landscape + landscape with labeled patches
landscapemetrics::show_patches(rc_raster, class = 4, labels = FALSE)
landscapemetrics::show_cores(rc_raster, class = 4, labels = FALSE)
landscapemetrics::show_lsm(rc_raster, what = "lsm_p_area", class = 4, label_lsm = TRUE, labels = FALSE)

# Spatialize landscape metric values
lsm_p_area_raster <- landscapemetrics::spatialize_lsm(rc_raster$layer, what = "lsm_p_cai", progress = TRUE)
lsm_p_area_raster
lsm_p_area_raster[[1]]
landscapetools::show_landscape(lsm_p_area_raster[[1]]$lsm_p_cai)

# end ---------------------------------------------------------------------