
library(here)
library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(progressr)
library(data.table)


### Storefronts! ####
### Read storefronts file, metadata available here: https://opendata.vancouver.ca/explore/dataset/storefronts-inventory/information/
stores <- fromJSON(here("data","raw" ,"storefronts-inventory.json"))

### Extract lat/lon from the nested column
stores$longitude <- stores$geo_point_2d$lon
stores$latitude <- stores$geo_point_2d$lat
### Convert to sf object
stores_sf <- st_as_sf(stores, coords = c("longitude", "latitude"), crs = 4326)
### filter to only food related businesses
categories <- c("Food & Beverage", "Convenience Goods", "Entertainment and Leisure")

# Filter the sf dataframe
food_stores <- stores_sf %>%
  filter(retail_category %in% categories)
rm(stores,stores_sf)

# Check results
print(food_stores)

ggplot(data = food_stores) +
  geom_sf(aes(color = retail_category, fill = retail_category), 
          shape = 21, size = 3, alpha = 0.6) +  # Shape 21 allows fill + border
  scale_fill_viridis_d() +  # Use a nice color palette
  scale_color_viridis_d() + 
  theme_minimal() +
  labs(title = "Retail Store Categories")


## object called "dat_grid"
load(file = here("data", "dat_fw_demo_lc_750.Rdata"))
unique(food_stores$retail_category)
head(food_stores)


food_stores <- food_stores %>%
  mutate(
    lon = geo_point_2d$lon,
    lat = geo_point_2d$lat
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3857)

food_stores <- food_stores[, -c(1:4,8,9:11)]

food_stores <- st_transform(food_stores, st_crs(dat_grid))



# Perform spatial join: Assigns food store points to grid cells
joined <- st_join(dat_grid, food_stores, left = TRUE) 



# Enable progress bar
handlers(global = TRUE)


# Convert to data.table
dt_joined <- as.data.table(joined)

# Convert `retail_category` to a factor for faster comparison
dt_joined[, retail_category := as.factor(retail_category)]

# # Perform grouped aggregation
# dat_grid_final <- with_progress({
#   p <- progressor(along = unique(dt_joined$grid_id))  # Progress for unique grid IDs
#   
#   result <- dt_joined[, .(
#     Food_Beverage = as.integer(any(retail_category == "Food & Beverage", na.rm = TRUE)),
#     Convenience_Goods = as.integer(any(retail_category == "Convenience Goods", na.rm = TRUE)),
#     Entertainment_Leisure = as.integer(any(retail_category == "Entertainment and Leisure", na.rm = TRUE)),
#     geometry = geometry[1]  # Fast way to retain geometry
#   ), by = .(grid_id)]
#   
#   p()  # Update progress
#   
#   st_as_sf(result)  # Convert back to sf
# })


dat_grid_final <- with_progress({
  p <- progressor(along = unique(dt_joined$grid_id))  # Progress for unique grid IDs
  
  result <- dt_joined[, .(
    Food_Retail = as.integer(any(retail_category %in% c("Food & Beverage", "Convenience Goods", "Entertainment and Leisure"), na.rm = TRUE)),
    geometry = geometry[1]  # Retain one geometry per grid cell
  ), by = .(grid_id)]
  
  p()  # Update progress
  
  st_as_sf(result)  # Convert back to sf
})


ggplot(dat_grid_final) +
  geom_sf(aes(fill = factor(Food_Retail)), color = NA) +  # Remove borders
  scale_fill_manual(values = c("0" = "gray90", "1" = "red")) +
  theme_minimal() +
  labs(title = "Food & Beverage Locations", fill = "Presence")
