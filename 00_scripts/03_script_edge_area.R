# -------------------------------------------------------------------------
# landscape metrics
# mauricio vancine
# mauricio.vancine@gmail.com
# 13-08-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(fasterize)
library(landscapemetrics)
library(landscapetools)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# directory
path <- "/home/mude/data/gitlab/landscapemetrics"
setwd(path)
getwd()
dir()

# import data -------------------------------------------------------------
# import
rc <- sf::read_sf("./02_dados/vector/rio_claro/SP_3543907_USO.shp")

ggplot() + 
  geom_sf(data = rc, aes(fill = CLASSE_USO), color = NA) + 
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  labs(x = "Longitude", y = "Latitude", fill = "Classes") +
  theme_bw()

# create number coloumn
rc <- rc %>% 
  dplyr::mutate(class = seq(5))
sf::st_drop_geometry(rc)

# rasterize ---------------------------------------------------------------
# create raster
ra <- fasterize::raster(rc, res = 30)
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

# buffers -----------------------------------------------------------------
po <- sf::read_sf("./02_dados/vector/rio_claro/pontos_amostragem.shp")
po

bu <- sf::st_buffer(po, 2000)
bu

landscapetools::show_landscape(rc_raster, discrete = TRUE) +
  geom_sf(data = bu, fill = NA, color = "black", size = 1)

# crop and mask landscapes ------------------------------------------------
# select buffers
bu01 <- dplyr::filter(bu, id == 1)
bu01
ggplot(data = bu01) + geom_sf() + theme_bw()

bu02 <- dplyr::filter(bu, id == 2)
bu02
ggplot(data = bu02) + geom_sf() + theme_bw()

# crop and mask landscapes
la01 <- rc_raster %>% 
  raster::crop(bu01) %>% 
  raster::mask(bu01)
la01
landscapetools::show_landscape(la01, discrete = TRUE) + 
  geom_sf(data = bu01, fill = NA)

la02 <- rc_raster %>% 
  raster::crop(bu02) %>% 
  raster::mask(bu02)
la02
landscapetools::show_landscape(la02, discrete = TRUE) + 
  geom_sf(data = bu02, fill = NA)

# check rasters -----------------------------------------------------------
landscapemetrics::check_landscape(la01)
landscapemetrics::check_landscape(la02)

# list metrics ------------------------------------------------------------
# all
all_metrics <- landscapemetrics::list_lsm()
all_metrics

# patch metrics
patch_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "patch") %>% 
  dplyr::arrange(type)
patch_metrics

# class metrics
class_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "class") %>% 
  dplyr::arrange(type)
class_metrics

# landscape metrics
landscape_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "landscape") %>% 
  dplyr::arrange(type)
landscape_metrics

# export
dplyr::bind_rows(patch_metrics, class_metrics, landscape_metrics) %>% 
  readr::write_csv("all_metrics.csv")

# metrics -----------------------------------------------------------------
# area in patch level
area_p <- landscapemetrics::lsm_p_area(la01)
area_p

# area in class level
area_c <- landscapemetrics::lsm_c_ca(la01)
area_c

# area in patch level
area_l <- landscapemetrics::lsm_l_ta(la01)
area_l

# verify
area_p %>% 
  dplyr::group_by(class) %>% 
  dplyr::summarise(area = sum(value))
area_c

area_p %>% 
  dplyr::group_by(layer) %>% 
  dplyr::summarise(area = sum(value))
area_l

# calculate all metrics on patch level
lsm_patch <- landscapemetrics::calculate_lsm(landscape = la01, 
                                             level = "patch", 
                                             edge_depth = 1,
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_patch
lsm_patch$metric %>% unique

lsm_class <- landscapemetrics::calculate_lsm(landscape = la01, 
                                             level = "class", 
                                             edge_depth = 1,
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_class
lsm_class$metric %>% unique

lsm_landscape <- landscapemetrics::calculate_lsm(landscape = la01, 
                                                 level = "landscape",
                                                 edge_depth = 1,
                                                 full_name = TRUE, 
                                                 verbose = TRUE, 
                                                 progress = TRUE)
lsm_landscape
lsm_landscape$metric %>% unique

# maps --------------------------------------------------------------------
# plot landscape + landscape with labeled patches
landscapemetrics::show_patches(la01, class = 4, labels = FALSE)
landscapemetrics::show_cores(la01, class = 4, labels = FALSE)
landscapemetrics::show_lsm(la01, what = "lsm_p_area", class = 4, label_lsm = TRUE, labels = FALSE) +
  theme(legend.position = "none")

# spatialize landscape metric values --------------------------------------
lsm_p_area_raster <- landscapemetrics::spatialize_lsm(la01$layer, what = "lsm_p_area", progress = TRUE)
lsm_p_area_raster
landscapetools::show_landscape(lsm_p_area_raster[[1]]$lsm_p_area)

# end ---------------------------------------------------------------------