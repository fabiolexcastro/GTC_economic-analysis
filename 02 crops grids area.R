
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, geodata, raster, glue, tidyverse, readxl, xlsx, openxlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

## Crop Grids
hrvs <- terra::rast('./tif/cg/cropgrids_harvestarea.tif')
names(hrvs) <- c('Cocoa', 'Cashew', 'Coconut', 'Oil palm', 'Rubber', 'Mango')

## Impact gradients
impr <- terra::rast('./tif/rf/imp_gra_gcms.tif')
lbls <- data.frame(value = c(0, 1, 2, 3, 4, 5), category = c('Unsuit', 'cope', 'adjust', 'transform', 'opportunity', 'resilience'))

## Crops 
crps <- names(hrvs)

## Vector data 
gha0 <- gadm(country = 'GHA', level = 0, path = './tmpr')

# To get the area by each impg --------------------------------------------

## 
calc.area <- function(crop){
  
  # crop <- crps[1] # Faostat: 1484568 has
  
  ## Filtering crop
  cat('To start the analysis: ', crop, '\n')
  hrv <- hrvs[[grep(crop, names(hrvs))]]
  hrv <- terra::ifel(hrv <= 0, NA, hrv)
  
  ## Impact zone 
  imp <- impr[[grep(crop, names(impr))]]
  gcs <- names(imp) %>% str_split('_') %>% map_chr(3) %>% unique()
  
  ## Loop by each GCM 
  are <- map_dfr(.x = 1:length(gcs), .f = function(i){
    
    ## GCM 
    cat('GCM ', gcs[i], '\n')
    
    ## Filtering cope / adjust / transform
    ip <- imp[[grep(paste0(gcs[i], '$'), names(imp))]]
    ip <- terra::ifel(!ip %in% 1:3, 0, ip)
    ip <- terra::crop(ip, gha0)
    ip <- terra::mask(ip, gha0)
    names(ip) <- 'imp'
    
    ## Raster to polyon
    pl <- terra::as.polygons(ip)
    pl <- pl[pl$imp > 0,]
    
    ## Calc area by each impact gradient zone 
    ar <- map_dfr(.x = 1:nrow(pl), .f = function(z){
      p <- pl[z, ]
      h <- terra::crop(hrv, p) %>% terra::mask(., p)
      s <- terra::global(h, 'sum', na.rm = T) %>% as.numeric()
      a <- tibble(zone = p$imp, area = s)
      a <- inner_join(a, lbls, by = c('zone' = 'value')) #1484568
      return(a)
    }) %>% 
      mutate(gcm = gcs[i], .before = 'zone')
    
    return(ar)
    
  }) %>% 
    dplyr::select(-zone, gcm, category, area) %>% 
    mutate(specie = crop)
  
  ## Finish 
  cat('Done!\n')
  return(are)
  
}

## 
ares <- map_dfr(crps, calc.area)
ares[is.na(ares)] <- 0
ares <- dplyr::select(ares, Crop = specie, GCM = gcm, Impact = category, Has = area)
write.csv(ares, './tbl/area_imp-gcms.csv', row.names = FALSE)

