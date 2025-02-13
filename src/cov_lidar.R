### CoV_Lidar

####### raw data sourced from https://opendata.vancouver.ca/explore/dataset/lidar-2022/information/

#load packages
library(sf)
library(here)
library(terra)

### explore layers
st_layers(here("data","raw","lidar-2022", "lidar-2022.shp"))


### explore layers
cov_lidar <- st_read(here("data","raw","lidar-2022", "lidar-2022.shp"))

plot(cov_lidar)


