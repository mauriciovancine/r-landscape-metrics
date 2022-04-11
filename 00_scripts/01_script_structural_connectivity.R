# -------------------------------------------------------------------------
# structural connectivity metrics function
# mauricio vancine - mauricio.vancine@gmail.com
# 31-08-2019
# -------------------------------------------------------------------------

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(geobr)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(tmap)

# directory
path <- "../"
setwd(path)
getwd()
dir()

# import data -------------------------------------------------------------
# import
rc <- sf::read_sf("01_data/rio_claro_sp/SP_3543907_USO.shp")

# rio claro limit
rc_lim <- geobr::read_municipality(code_muni = 3543907, year = 2018)
rc_lim

tm_shape(rc) +
  tm_fill(col = "CLASSE_USO", title = "Class",  
          palette = c("blue", "orange", "gray", "forestgreen", "green")) +
  # tm_shape(rc_lim) +
  # tm_borders() +
  tm_grid(lwd = .1, labels.inside.frame = F, labels.rot = c(0, 90)) +
  tm_compass(type = "arrow", position = c("left", "top"), size = 3) +
  tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)

# select forest
rc_forest <- rc %>% 
  dplyr::filter(CLASSE_USO == "formação florestal")

tm_shape(rc_forest) +
  tm_fill(col = "forestgreen", title = "Class") +
  # tm_shape(rc_lim) +
  # tm_borders() +
  tm_grid(lwd = .1, labels.inside.frame = F, labels.rot = c(0, 90))+
  tm_compass(type = "arrow", position = c("left", "top"), size = 3) +
  tm_scale_bar(breaks = c(0, 3, 6), text.size = 1)

# poits sampling
set.seed(42)
po <- rc_forest %>% 
  sf::st_sample(size = 3, exact = FALSE) %>% 
  sf::as_Spatial() %>% 
  sf::st_as_sf() %>% 
  dplyr::mutate(id = 1:4)
po

tm_shape(rc_forest) +
  tm_fill(col = "forestgreen", title = "Class") +
  tm_grid(lwd = .1, labels.inside.frame = F, labels.rot = c(0, 90)) +
  tm_shape(po) +
  tm_dots(size = 3, shape = 21, col = "red", alpha = .7)

# function ----------------------------------------------------------------
lsm_patch_area_intersected <- function(points, forest, buffer_radius, distance_intersected, output){
  
  # directory
  setwd(output)
  
  # objetcs
  patch_area_intersected <- NULL
  
  # metric
  for(i in po$id %>% length %>% seq){
    
    # information
    print(paste0("Patch area intersected be calculated for point ", i))
    
    # buffer rad
    bu_rad <- points %>%
      dplyr::filter(id == i) %>% 
      st_buffer(buffer_radius)
    
    # buffer intersected
    bu_int <- points %>%
      dplyr::filter(id == i) %>% 
      st_buffer(distance_intersected)
    
    # clip patches intersected
    fo_int <- forest %>% 
      sf::st_intersection(bu_int)
    
    # patches intersected with buffer rad
    fo_int_bu_rad <- fo_int %>%
      sf::st_cast("POLYGON") %>% 
      dplyr::mutate(row_number = dplyr::row_number()) %>% 
      dplyr::filter(row_number %in% unlist(st_intersects(bu_rad, .))) %>% 
      sf::st_cast("MULTIPOLYGON")
    
    # area
    fo_int_bu_rad_area <- fo_int_bu_rad %>% 
      sf::st_area()
    
    # bind
    patch_area_intersected <- tibble::tibble(id = i,
                                             area_m2 = sum(fo_int_bu_rad_area),
                                             area_km2 = sum(fo_int_bu_rad_area)/1e6,
                                             area_ha = sum(fo_int_bu_rad_area)/1e4) %>%
      dplyr::bind_rows(patch_area_intersected, .)
    
    # map
    tm_shape(fo_int) +
      tm_polygons() +
      tm_shape(fo_int_bu_rad) +
      tm_polygons("forestgreen") +
      tm_shape(bu_int) +
      tm_borders(lwd = 3) +
      tm_shape(bu_rad) +
      tm_borders(lwd = 3, col = "red")
    ggsave(filename = paste0("landscape", i, ".tiff"), wi = 25, he = 25, un = "cm", dpi = 300, comp = "lzw")
    
  }
  
  # export
  readr::write_csv(patch_area_intersected, "patch_area_intersected.csv")
  
}


# use ---------------------------------------------------------------------
lsm_patch_area_intersected(points = po, 
                           forest = rc_forest, 
                           buffer_radius = 500, 
                           distance_intersected = 2000,
                           output = "/home/mude/data/gitlab/r-landscape-ecology/02_results")

# end ---------------------------------------------------------------------