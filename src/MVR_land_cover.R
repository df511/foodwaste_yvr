
### MVR Land Cover processing
#### written by Daniel Forrest
### February 11, 2025


### raw data sourced from: https://open-data-portal-metrovancouver.hub.arcgis.com/datasets/5dd153684b9b41249c0dcf09e79c9b25

#load packages
library(sf)
library(here)
library(terra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(exactextractr)


file_path <- here("data","raw","MetroVan_LCC_2020_final.tif")
## object called "dat_grid"
load(file = here("data", "dat_fw_demo_25.Rdata"))

# Force reading as a raster with assumed dimensions
lcc2020 <- rast(file_path)

lcc2020 <- project(lcc2020, "EPSG:3857")
van_ext<- c(-13715667.2184339, -13702478.8456052, 6313777.37423673, 6325346.85547016)
lcc2020_van <- crop(lcc2020, van_ext)
lcc2020_van <- as.int(lcc2020_van)

lc_prop <- exact_extract(lcc2020_van, dat_grid, fun = "frac")
#### values from Report (No #14, Snow/Ice) https://metrovancouver.org/services/regional-planning/Documents/mv-land-cover-classification-sei-update-2022.pdf
colnames(lc_prop) <- c("Buildings", "Paved", "OtherBuilt", "Barren", "Soil","Conifer","Deciduous","Shrub","ModGrassHerb","NatGrassHerb","NonphotoVeg","Water","Shadow")

lc_prop$grid_id <- 1:nrow(lc_prop)
dat_grid <- left_join(dat_grid, lc_prop, by = "grid_id")

### save file
save(dat_grid, file = here("data","dat_fw_demo_lc_25.Rdata"))


