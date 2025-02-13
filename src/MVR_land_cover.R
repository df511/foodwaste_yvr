
### MVR Land Cover processing
#### written by Daniel Forrest
### February 11, 2025


### raw data sourced from: https://open-data-portal-metrovancouver.hub.arcgis.com/datasets/5dd153684b9b41249c0dcf09e79c9b25

#load packages
library(sf)
library(here)
library(terra)

### explore layers
st_layers(here("data","raw","LCC2020.gdb"))

### read multipolygon land cover layer
lc2020 <- st_read(here("data","raw","LCC2020.gdb"))  # Read specific layer

### read multipolygon land cover layer
bnd <- st_read(here("data","raw","LCC2020.gdb"), layer = "fras_bnd_LCC2020")  # Read specific layer


head(lc2020)

# Read the potential land cover attribute tables
vat <- st_read(here("data", "raw", "LCC2020.gdb"), layer = "VAT_LCC2020")

vat <- st_read(here("data", "raw", "LCC2020.gdb"), layer = "VAT_LCC2020")
unique(lc2020$RASTER)  # Check unique values in lc2020
unique(vat$Value)       # Compare with VAT_LCC2020 values


fras_blk <- st_read(here("data", "raw", "LCC2020.gdb"), layer = "fras_blk_LCC2020")

# View column names
names(vat)
names(fras_blk)

# View first few rows
head(vat)
head(fras_blk)


fras_blk <- st_read(here("data", "raw", "LCC2020.gdb"), layer = "fras_blk_LCC2020")
head(fras_blk)





