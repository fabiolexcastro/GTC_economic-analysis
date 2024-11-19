

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, sf, geodata, raster, glue, tidyverse, readxl, xlsx, openxlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
area <- read_csv('./tbl/area_imp-gcms.csv', show_col_types = FALSE)
faos <- read_csv('./tbl/faostat_prod_summary.csv', show_col_types = FALSE)

# Join both tables --------------------------------------------------------
tble <- full_join(area, faos, by = c('Crop' = 'specie'))

# Yields (Column F) -------------------------------------------------------
ylds <- tble %>% 
  group_by(Crop, GCM) %>% 
  dplyr::summarise(
    Has = sum(Has, na.rm = T)
  ) %>% 
  ungroup() %>% 
  full_join(., faos, by = c('Crop' = 'specie')) %>% 
  group_by(Crop, GCM) %>% 
  mutate(yield = prod / Has) %>% 
  ungroup()

write.csv(ylds, './tbl/yields.csv', row.names = FALSE)

ylds <- ylds %>% dplyr::select(Crop, GCM, yield)

# Prod / Impact zone (Column G) -------------------------------------------
tble <- full_join(tble, ylds, by = c('Crop', 'GCM'))

## Add low / med / high by each impact gradient zone ----------------------

tibble(Impact = c('cope', 'adjust', 'transform'))


## Cope
cope <- tble %>% 
  filter(Impact == 'cope') %>% 
  mutate(
    low_imp = yield * 0.9, 
    mid_imp = yield * 0.85, 
    hig_imp = yield * 0.8 
  ) %>% 
  ## To add the difference
  group_by(Crop, GCM, Impact) %>% 
  mutate(
    dfr_low = prod - sum(low_imp),
    dfr_mid = prod - sum(mid_imp),
    dfr_hig = prod - sum(hig_imp)
  ) %>% 
  ungroup()

## Adjust 
adjs <- tble %>% 
  filter(Impact == 'adjust') %>% 
  mutate(
    low_imp = yield * 0.8, 
    mid_imp = yield * 0.7, 
    hig_imp = yield * 0.6 
  ) %>% 
  ## To add the difference
  group_by(Crop, GCM, Impact) %>% 
  mutate(
    dfr_low = prod - sum(low_imp),
    dfr_mid = prod - sum(mid_imp),
    dfr_hig = prod - sum(hig_imp)
  ) %>% 
  ungroup()

## Tranform 
trns <- tble %>% 
  filter(Impact == 'transform') %>% 
  mutate(
    low_imp = yield * 0.8, 
    mid_imp = yield * 0.7, 
    hig_imp = yield * 0.6 
  ) %>% 
  ## To add the difference
  group_by(Crop, GCM, Impact) %>% 
  mutate(
    dfr_low = prod - sum(low_imp),
    dfr_mid = prod - sum(mid_imp),
    dfr_hig = prod - sum(hig_imp)
  ) %>% 
  ungroup()

## Join the three tables into only one
rslt <- rbind(cope, adjs, trns)

write.csv(rslt, './tbl/result.csv', row.names = FALSE)
