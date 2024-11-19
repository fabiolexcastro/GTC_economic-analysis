
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, geodata, raster, glue, tidyverse, readxl, xlsx, openxlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
faos <- read.xlsx('./tbl/database_crops-faostat.xlsx')
faos <- as_tibble(faos)

# Filtering by production and years ---------------------------------------
faos <- filter(faos, Elemento == 'Producción')
faos <- filter(faos, year %in% 2018:2022)

# Join with the common label ----------------------------------------------
lbls <- faos %>% 
  distinct(Producto) %>% 
  mutate(specie = c('Cocoa', 'Cashew', 'Coconut', 'Oil palm', 'Rubber', 'Shea', 'Mango'))

faos <- inner_join(faos, lbls, by = c('Producto')) 

# Summary by the last 5 years ---------------------------------------------
faos <- dplyr::select(faos, specie, Elemento, Área, Unidad, year, value)
faos <- dplyr::select(faos, specie, year, prod = value)
faos <- faos %>% group_by(specie) %>% dplyr::summarise(prod = mean(prod, na.rm = T)) %>% ungroup()

## To save the summarise table
write.csv(faos, './tbl/faostat_prod_summary.csv', row.names = FALSE)

