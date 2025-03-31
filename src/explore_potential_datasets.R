

library(here)
library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(progressr)
library(data.table)
library(readxl)


### collection schedules

single_collect <- st_read(here("data","raw","garbage_schedule_zones_sf_shp"))
multi_collect <- st_read(here("data","raw","garbage_schedule_zones_murb_shp"))

plot(multi_collect)


park_facilities <- fromJSON(here("data","raw" ,"parks-facilities.json"))

lowcost_food <- fromJSON(here("data","raw" ,"free-and-low-cost-food-programs.json"))

plot(lowcost_food)




community_gardens <- st_read(here("data","raw" ,"community-gardens-and-food-trees.geojson"))

plot(community_gardens$geometry)



shelters <- st_read(here("data","raw","homeless-shelter-locations"))
plot(shelters)


ggplot() +
  geom_sf(dat_grid_final, aes(color = "gray")) +  # No borders for a smooth heatmap
  geom_sf(shelters, aes(color = "red"))
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look
  
  
  ggplot() +
    geom_sf(data = dat_grid_final, fill = NA, color = "black", size = 0.3) +  # Grid cells
    geom_sf(data = shelters, aes(color = "Shelters"), size = 2) +  # Shelter points
    scale_color_manual(values = c("Shelters" = "red")) +  # Customize point color
    theme_minimal() +
    labs(title = "Shelters Overlapping Grid Cells",
         color = "Legend")
  
  
  
MSDI <- read_excel(here("data","raw", "INDQ_MSDI_Canada", "A-MSDIData_Can2021_en", "1. EquivalenceTableCanada2021_en.xlsx"))

# Filter the dataframe
MSDI_yvr <- MSDI %>% 
  filter(MUNIC == "5915022")


tax <- fromJSON(here("data","raw" ,"property-tax-report.json"))
properties <- st_read(here("data","raw" ,"property-parcel-polygons.geojson"))

load(here("data","grid_clipped_750m.Rdata"))

properties <- st_transform(properties, st_crs(dat_grid)) ### ensure the same CRS

save(sites_3857, file = here("data","sites_3857.Rdata"))

prop_cropped <- st_crop(properties, sites_3857)

plot(prop_cropped)a


waste_facilities <- fromJSON(here("data","raw" ,"permanent_waste_facilities.geojson"))
