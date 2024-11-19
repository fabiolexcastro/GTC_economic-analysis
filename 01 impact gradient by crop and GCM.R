

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, raster, glue, tidyverse, readxl, xlsx, openxlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)


# Load data ---------------------------------------------------------------
crnt <- terra::rast('./tif/rf/rf_current.tif')
ftre <- terra::rast('./tif/rf/future_crops-gcms.tif')
gcms <- names(ftre) %>% str_split(., '_') %>% map_chr(2) %>% unique()

crps <- names(crnt) %>% str_split(., '_') %>% map_chr(1)

##
all_options <- read_csv('./tbl/classesImpGraLimMix_V3.csv', show_col_types = FALSE); unique(all_options$category) 
labelss <- data.frame(value = c(0, 1, 2, 3, 4, 5), category = c('Unsuit', 'cope', 'adjust', 'transform', 'opportunity', 'resilience'))

# Make impact gradient ----------------------------------------------------

## 
make.impr <- function(crop, gcme){
  
  ### Proof 
  # crop <- 'Cocoa'
  # gcme <- 'ACCESS-CM2'
  
  ### To start thea analysis
  cat('Crop: ', crop, ' ', gcme, '\n')
  
  crn <- crnt[[grep(crop, names(crnt))]]
  
  ftr <- ftre[[grep(crop, names(ftre))]]
  ftr <- ftr[[grep(gcme, names(ftr))]]
  ftr <- ftr[[1]]
  
  ### To make the impact gradient
  
  ## To raster 
  crn <- raster(crn)
  ftr <- raster(ftr)
  
  ## To start
  msk <- crn * 0
  crd_df <- coordinates(crn)
  
  ## To extract the values
  x <- raster::extract(crn, crd_df, cellnumbers = TRUE) %>% as_data_frame()
  ncell <- dplyr::select(x, cells)
  x <- select_(x, names(crn))
  colnames(x) <- 'current'
  
  y <- raster::extract(ftr, crd_df[,c('x', 'y')], cellnumbers = TRUE) %>% as_data_frame()
  y <- select_(y, names(ftr))
  colnames(y) <- 'future'
  
  z <- data.frame(x, y, ncell) %>% as_tibble()
  rslts <- left_join(z, all_options, by = c('current', 'future'))
  labls <- as_tibble(labelss) %>% mutate(category = as.character(category))
  final <- left_join(rslts, labls, by = 'category') %>% dplyr::select(value) %>% pull(1)
  
  length(final)
  length(msk)
  hist(final)
  
  ## To build the final raster
  rst <- raster::setValues(msk, final)
  rst <- rast(rst)
  names(rst) <- glue('imp_{crop}_{gcme}')
  
  ## Finish 
  cat('Done!\n')
  return(rst)
  
}

## 
impr <- map(.x = 1:length(crps), .f = function(i){
  r <- map(.x = 1:length(gcms), .f = function(j){
    make.impr(crop = crps[i], gcme = gcms[j])
  }) %>% 
    reduce(., c)
}) %>% 
  reduce(., c)

terra::writeRaster(x = impr, filename = './tif/rf/imp_gra_gcms.tif', overwrite = TRUE)


