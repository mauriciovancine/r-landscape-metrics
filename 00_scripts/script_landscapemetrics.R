# -------------------------------------------------------------------------
# pacote landscapemetrics
# mauricio vancine
# mauricio.vancine@gmail.com
# 27-11-2019
# -------------------------------------------------------------------------

# preparar o r  -----------------------------------------------------------
# memoria
rm(list = ls())

# instalar pacotes
# install.packages(c("tidyverse", "sf", "raster", "rgdal", "fasterize", "devtools",
#                    "landscapetools", "landscapemetrics", "tmap", "ggspatial"),
#                  dependencies = TRUE)
# 
# github
# devtools::install_github("thomasp85/patchwork")

# carregar pacotes
library(sf)
library(raster)
library(rgdal)
library(fasterize)
library(landscapetools)
library(landscapemetrics)
library(tmap)
library(ggspatial)
library(patchwork)
library(tidyverse)

# directorio
setwd("./")
getwd()
dir()

# importar dados ----------------------------------------------------------
# download: http://geo.fbds.org.br/SP/RIO_CLARO/USO/
# import vector
rc <- sf::read_sf("./02_dados/vector/SP_3543907_USO.shp")

# tabela de atributos
sf::st_drop_geometry(rc)

# mapa
ggplot() +
  geom_sf(data = rc, aes(fill = CLASSE_USO), color = NA) +
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  coord_sf() +
  labs(x = "Longitude", y = "Latitude", fill = "Classes") +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "cm"), pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  theme(legend.position = c(.15, .15),
        legend.background = element_rect(colour = "black"))

# criar uma coluna numerica para as classes de uso da terra
rc <- rc %>% 
  dplyr::mutate(classe_num = seq(5))

# tabela de atributos
sf::st_drop_geometry(rc)

# rasterizar --------------------------------------------------------------
# criar um raster vazio
ra <- fasterize::raster(rc, res = 30)
ra

# rasterizar
rc_raster <- fasterize::fasterize(sf = rc, raster = ra, field = "classe_num")
rc_raster

# mapa fasterize
fasterize::plot(rc_raster)

# mapa landscapetools
landscapetools::show_landscape(rc_raster, discrete = TRUE) +
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  theme(legend.position = "none")

# mapa ggplot2
ggplot() +
  geom_raster(data = raster::rasterToPoints(rc_raster) %>% tibble::as_tibble(),
              aes(x, y, fill = factor(layer))) +
  coord_sf() +
  scale_fill_manual(values = c("blue", "orange", "gray", "forestgreen", "green")) +
  labs(x = "Longitude", y = "Latitude", fill = "Classes") +
  theme_bw() +
  theme(legend.position = c(.2, .2),
        axis.text.y = element_text(angle = 90, hjust = .5))

# mapa tmap
tm_shape(rc_raster, bbox = raster::bbox(rc_raster) + c(-1e3, -1e3, 1e3, 1e3)) +
  tm_raster(style = "cat", palette = c("blue", "orange", "gray", "forestgreen", "green"),
            title = "Classes") +
  tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = .8) +
  tm_compass(position = c(.73, .08)) +
  tm_scale_bar(position = c(.63, 0), text.size = .65) +
  tm_layout(legend.position = c("left", "bottom")) 

# exportar raster
raster::writeRaster(x = rc_raster,
                    filename = "./02_dados/raster/SP_3543907_USO_raster_30m",
                    format = "GTiff",
                    options = c("COMPRESS=DEFLATE" , "TFW=TRUE"),
                    overwrite = TRUE)

# buffers -----------------------------------------------------------------
# coordenadas - amostragens de campo
co <- tibble::tibble(id = 1:10,
                     x = c(222993, 229276, 242815, 231477, 231477, 
                           241702, 237779, 239468, 236614, 230836), 
                     y = c(7526113, 7517654, 7528139, 7507708, 7512711, 
                           7535264, 7532688, 7520459, 7527841, 7524274))
co

# pontos
po <- sf::st_as_sf(x = co, coords = c("x", "y"), crs = raster::crs(rc))
po

# buffers
bu_2km <- sf::st_buffer(x = po, dist = 2000)
bu_2km

# mapa
tm_shape(rc_raster, bbox = raster::bbox(rc_raster) + c(-1e3, -1e3, 1e3, 1e3)) +
  tm_raster(style = "cat", palette = c("blue", "orange", "gray", "forestgreen", "green"),
            title = "Classes") +
  tm_shape(bu_2km) +
  tm_borders(col = "red", lwd = 2) +
  tm_shape(po) +
  tm_dots(size = .7, shape = 20, alpha = .7) +
  tm_grid(lines = FALSE, labels.rot = c(0, 90), labels.size = .8) +
  tm_compass(position = c(.73, .08)) +
  tm_scale_bar(position = c(.63, 0), text.size = .65) +
  tm_layout(legend.position = c("left", "bottom")) 

# exportar
sf::write_sf(po, "./02_dados/vector/pontos_amostragem.shp")
sf::write_sf(bu_2km, "./02_dados/vector/buffer_2km.shp")

# ajustar paisagens -------------------------------------------------------
# list
rc_raster_pa <- list()
rc_raster_pa

# crop e mask das paisagens
for(i in 1:10){
  
  # informacao
  print(paste0("Ajustando a paisagem ", i))
  
  
  # filter
  bu_2km_pa <- bu_2km %>% 
    dplyr::filter(id == i)
  
  # crop e mask
  rc_raster_pa[[i]] <- rc_raster %>% 
    raster::crop(bu_2km_pa) %>% 
    raster::mask(bu_2km_pa)
  
}

# nomes das paisagens
rc_raster_pa
names(rc_raster_pa)
names(rc_raster_pa) <- c(paste0("paisagem_0", 1:9), "paisagem_10")
names(rc_raster_pa)

# mapas
la01 <- landscapetools::show_landscape(rc_raster_pa$paisagem_01, discrete = TRUE) +
  scale_fill_manual(values = c("blue", "orange", "forestgreen")) +
  labs(title = "Paisagem 01") +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = .5))
la01

la02 <- landscapetools::show_landscape(rc_raster_pa$paisagem_02, discrete = TRUE)+
  scale_fill_manual(values = c("blue", "orange", "forestgreen")) +
  labs(title = "Paisagem 02") +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = .5))
la02

la03 <- landscapetools::show_landscape(rc_raster_pa$paisagem_03, discrete = TRUE)+
  scale_fill_manual(values = c("blue", "orange", "forestgreen")) +
  labs(title = "Paisagem 03") +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = .5))
la03

la04 <- landscapetools::show_landscape(rc_raster_pa$paisagem_04, discrete = TRUE)+
  scale_fill_manual(values = c("blue", "orange", "forestgreen")) +
  labs(title = "Paisagem 04") +
  theme(legend.position = "none",
        axis.text.y = element_text(angle = 90, hjust = .5))
la04

# todos os mapas - patchwork
la01 + la02 + la03 + la04

# exportar
for(i in 1:10){
  
  # informacao
  print(paste0("Exportanto a ", names(rc_raster_pa)[i]))
  
  # exportar
  raster::writeRaster(x = rc_raster_pa[[i]],
                      filename = paste0("./02_dados/raster/", names(rc_raster_pa)[i]),
                      format = "GTiff",
                      options = c("COMPRESS=DEFLATE" , "TFW=TRUE"),
                      overwrite = TRUE)
}

# checar os raster --------------------------------------------------------
landscapemetrics::check_landscape(rc_raster_pa)

# prerequisitos do raster
#' 1. sistema de referencias de coordenadas e projetada (crs)
#' 2. unidade esta em metros (units)
#' 3. classes como valores inteiros (class)
#' 4. numero de classes (n_class)

# listar as metricas ------------------------------------------------------
# metricas
all_metrics <- landscapemetrics::list_lsm()
all_metrics

# patch metrics
patch_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "patch") %>% 
  dplyr::arrange(type)
patch_metrics

patch_metrics %>%
  group_by(type) %>% 
  summarise(n = n())

# class metrics
class_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "class") %>% 
  dplyr::arrange(type)
class_metrics

class_metrics_type <- class_metrics %>%
  group_by(type) %>% 
  summarise(n = n())
class_metrics_type

class_metrics_type_unique <- class_metrics %>%
  distinct(name, .keep_all = TRUE) %>% 
  group_by(type) %>% 
  summarise(n_unicas = n())
class_metrics_type_unique

bind_cols(class_metrics_type, class_metrics_type_unique[, 2]) %>% 
  mutate(n_agregacao = n - n_unicas)

# landscape metrics
landscape_metrics <- landscapemetrics::list_lsm() %>%
  dplyr::filter(level == "landscape") %>% 
  dplyr::arrange(type)
landscape_metrics

landscape_metrics_type <- landscape_metrics %>%
  group_by(type) %>% 
  summarise(n = n())
landscape_metrics_type$n %>% sum

landscape_metrics_type_unique <- landscape_metrics %>%
  distinct(name, .keep_all = TRUE) %>% 
  group_by(type) %>% 
  summarise(n_unicas = n())
landscape_metrics_type_unique

bind_cols(landscape_metrics_type, landscape_metrics_type_unique[, 2]) %>% 
  mutate(n_agregacao = n - n_unicas)

# exportar
readr::write_csv(all_metrics, "./02_dados/metricas_lista/listagem_metricas.csv")

# calcular as metricas ----------------------------------------------------
# estrutura das funcoes
#' 1. prefixo: ‘lsm_’
#' 2. nível: ‘p’, ‘c’ e ‘l’ para patch‐, class‐ e landscape‐level
#' 3. metrica: patch area - ‘lsm_p_area’
#' 4. todas as funcoes funcionam para rasterlayers, rasterstack/rasterbrick ou list
#' 5. algumas funcoes permitem add parametros: edge depth ou cell neighbourhood rule

# area in patch level
area_p <- landscapemetrics::lsm_p_area(landscape = rc_raster_pa)
area_p

# area in class level
area_c <- landscapemetrics::lsm_c_ca(landscape = rc_raster_pa)
area_c

# area in patch level
area_l <- landscapemetrics::lsm_l_ta(landscape = rc_raster_pa)
area_l

# verify
area_p_class_sum <- area_p %>% 
  dplyr::group_by(layer, class) %>% 
  dplyr::summarise(area = sum(value))
area_p_class_sum$area
area_c$value

all(area_p_class_sum$area == area_c$value)

area_p_layer_sum <- area_p %>% 
  dplyr::group_by(layer) %>% 
  dplyr::summarise(area = sum(value))
area_p_layer_sum$area
area_l$value

all(area_p_layer_sum$area == area_l$value)

# calcular todas as metricas por nivel ------------------------------------
# patch level
lsm_patch <- landscapemetrics::calculate_lsm(landscape = rc_raster_pa, 
                                             level = "patch", 
                                             edge_depth = 1,
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_patch

# class level
lsm_class <- landscapemetrics::calculate_lsm(landscape = rc_raster_pa, 
                                             level = "class", 
                                             edge_depth = 1,
                                             full_name = TRUE, 
                                             verbose = TRUE, 
                                             progress = TRUE)
lsm_class

# calculate all metrics on landscape level
lsm_landscape <- landscapemetrics::calculate_lsm(landscape = rc_raster_pa, 
                                                 level = "landscape",
                                                 edge_depth = 1, # celulas
                                                 full_name = TRUE, 
                                                 verbose = TRUE, 
                                                 progress = TRUE)
lsm_landscape

# export
readr::write_csv(lsm_patch, "./02_dados/metricas_tabelas/metricas_patch.csv")
readr::write_csv(lsm_class, "./02_dados/metricas_tabelas/metricas_class.csv")
readr::write_csv(lsm_landscape, "./02_dados/metricas_tabelas/metricas_landscape.csv")

# maps --------------------------------------------------------------------
# plot landscape + landscape with labeled patches
landscapemetrics::show_patches(landscape = rc_raster_pa$paisagem_01, class = 4, labels = FALSE)

landscapemetrics::show_cores(rc_raster_pa$paisagem_01, class = 4, labels = FALSE)

landscapemetrics::show_lsm(rc_raster_pa$paisagem_01, what = "lsm_p_area", class = 4, 
                           label_lsm = TRUE, labels = FALSE)

# spatialize landscape metric values --------------------------------------
rc_raster_pa01_fo <- raster::reclassify(x = rc_raster_pa$paisagem_01, 
                                         rcl = c(0,3,NA, 3,4,1))
landscapetools::show_landscape(rc_raster_pa01_fo)

rc_raster_pa01_fo_patch <- landscapemetrics::spatialize_lsm(rc_raster_pa01_fo,
                                                             what = "patch", 
                                                             progress = TRUE)
rc_raster_pa01_fo_patch

landscapetools::show_landscape(rc_raster_pa01_fo_patch[[1]]$lsm_p_area) +
  labs(title = "Área")

for(i in 1:length(rc_raster_pa01_fo_patch[[1]])){
  
  # informacao
  print(names(rc_raster_pa01_fo_patch[[1]][i]))
  
  # exportar
  raster::writeRaster(x = rc_raster_pa01_fo_patch[[1]][[i]],
                      filename = paste0("./02_dados/metricas_raster/paisagem_01_", names(rc_raster_pa01_fo_patch[[1]][i])),
                      format = "GTiff",
                      options = c("COMPRESS=DEFLATE" , "TFW=TRUE"),
                      overwrite = TRUE)
}
  
# exemplo -----------------------------------------------------------------



# end ---------------------------------------------------------------------