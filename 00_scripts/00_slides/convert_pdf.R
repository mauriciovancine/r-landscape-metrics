#' ---
#' title: convert to xaringan presentation to pdf
#' author: mauricio vancine
#' date: 2020-10-24
#' ---

# packages
library(pagedown)
library(xaringan)
library(tidyverse)
library(here)

# convert rmarkdown
purrr::map(here("01_aulas", dir(path = here("01_aulas"), pattern = ".Rmd"))[9], pagedown::chrome_print, timeout = 5000)

# end ---------------------------------------------------------------------