
library(here)
library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(progressr)
library(data.table)
library(tidygeocoder)


### Storefronts! ####
### Read storefronts file, metadata available here: https://opendata.vancouver.ca/explore/dataset/storefronts-inventory/information/
stores <- fromJSON(here("data","raw" ,"storefronts-inventory.json"))

### Extract lat/lon from the nested column
stores$longitude <- stores$geo_point_2d$lon
stores$latitude <- stores$geo_point_2d$lat
### Convert to sf object
stores_sf <- st_as_sf(stores, coords = c("longitude", "latitude"), crs = 4326)
### filter to only food related businesses
unique(stores_sf$retail_category)
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


######## food vendors

### Storefronts! ####
### Read storefronts file, metadata available here: https://opendata.vancouver.ca/explore/dataset/storefronts-inventory/information/
vendors <- fromJSON(here("data","raw" ,"food-vendors.json"))

### Extract lat/lon from the nested column
vendors$longitude <- vendors$geo_point_2d$lon
vendors$latitude <- vendors$geo_point_2d$lat
### Convert to sf object
vendors_sf <- st_as_sf(vendors, coords = c("longitude", "latitude"), crs = 4326)

plot(vendors_sf)


## object called "dat_grid"
load(file = here("data", "dat_fw_demo_lc_750.Rdata"))
unique(food_stores$retail_category)
head(food_stores)


ggplot() +
  # Plot food store points
  geom_sf(data = food_stores, aes(color = retail_category, fill = retail_category), 
          shape = 21, size = 3, alpha = 0.6) +  
  scale_fill_viridis_d() +  
  scale_color_viridis_d() + 
  
  # Overlay dat_grid (assuming `Any_Food_Retail` is a 1/0 column)
  geom_sf(data = dat_grid$geometry, aes(color = NA, alpha = 0.3)) +  
  scale_fill_manual(values = c("0" = "gray90", "1" = "red"), na.translate = FALSE, name = "Food Retail Grid") +
  
  theme_minimal() +
  labs(title = "Retail Store Categories with Grid Overlay")



ggplot() +
  # Plot food store points
  geom_sf(data = vendors_sf, 
          shape = 21, size = 1, alpha = 0.6) +  
  scale_fill_viridis_d() +  
  scale_color_viridis_d() + 
  
  # Overlay dat_grid (assuming `Any_Food_Retail` is a 1/0 column)
  geom_sf(data = dat_grid$geometry, aes(color = NA, alpha = 0.3)) +  
  scale_fill_manual(values = c("0" = "gray90", "1" = "red"), na.translate = FALSE, name = "Food Retail Grid") +
  
  theme_minimal() +
  labs(title = "Vendors with Grid Overlay")


food_stores <- food_stores %>%
  mutate(
    lon = geo_point_2d$lon,
    lat = geo_point_2d$lat
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3857)

food_stores <- food_stores[, -c(1:4,8,9:11)]

food_stores <- st_transform(food_stores, st_crs(dat_grid))



vendors_sf <- vendors_sf %>%
  mutate(
    lon = geo_point_2d$lon,
    lat = geo_point_2d$lat
  ) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 3857)
vendors_sf <- st_transform(vendors_sf, st_crs(dat_grid))

vendors_sf <- vendors_sf %>%
  rename(retail_category = vendor_type)

vendors_sf <- vendors_sf[, -c(1,3,5:9,12)]
vendors_sf$year_recorded <- "2020"

food_stores <- bind_rows(food_stores, vendors_sf)


# Perform spatial join: Assigns food store points to grid cells
joined <- st_join(dat_grid, food_stores, left = TRUE) 



# Enable progress bar
handlers(global = TRUE)


# Convert to data.table
dt_joined <- as.data.table(joined)

# Convert `retail_category` to a factor for faster comparison
dt_joined[, retail_category := as.factor(retail_category)]



dat_grid_final <- with_progress({
  p <- progressor(along = unique(dt_joined$grid_id))  # Progress for unique grid IDs
  
  # Identify grid cells containing at least one relevant food retail location
  food_grids <- dt_joined[, .(
    Food_Retail = as.integer(any(retail_category %in% c("Food & Beverage", "Convenience Goods", "Entertainment and Leisure"), na.rm = TRUE))
  ), by = .(grid_id)]
  
  p()  # Update progress
  
  # Merge back with original data to retain all columns
  result <- merge(dt_joined, food_grids, by = "grid_id", all.x = TRUE)
  
  st_as_sf(result)  # Convert back to sf
})

dat_grid_final <- dat_grid_final[,-c(44:48)]

### test plot
ggplot(dat_grid_final) +
  geom_sf(aes(fill = factor(Food_Retail)), color = NA) +  # Remove borders
  scale_fill_manual(values = c("0" = "gray90", "1" = "red")) +
  theme_minimal() +
  labs(title = "Food & Beverage Locations", fill = "Presence")




farmers_markets <- fromJSON(here("data","raw" ,"community-food-markets-and-farmers-markets.json"))


### replace incomplete addresses

farmers_markets[82,9] <- "1 Kingsway, Vancouver, BC"
farmers_markets[80,9] <- "Grandview Park, Vancouver, BC"
farmers_markets[73,9] <- "2290 E. 25th Ave, Vancouver, BC"
farmers_markets[65,9] <- "5500 East Blvd., Vancouver, BC"
farmers_markets[60,9] <- "1100 Station St, Vancouver, BC"
farmers_markets[51,9] <- "8680 Hudson Street, Vancouver, BC"
farmers_markets[42,9] <- "8680 Hudson Street, Vancouver, BC"
farmers_markets[38,9] <- "Hastings Skatepark, Vancouver, BC"
farmers_markets[37,9] <- "8680 Hudson Street, Vancouver, BC"
farmers_markets[34,9] <- "2305 West 7th Ave, Vancouver, BC"
farmers_markets[24,9] <- "2290 E. 25th Ave, Vancouver, BC"
farmers_markets[17,9] <- "1420 W. 12th. Ave, Vancouver, BC"
farmers_markets[13,9] <- "2305 West 7th Ave, Vancouver, BC"
farmers_markets[10,9] <- "1100  Station St, Vancouver BC"
farmers_markets[5,9] <- "Hastings Skatepark, Vancouver, BC"
farmers_markets[3,9] <- "1420 W. 12th. Ave, Vancouver, BC"

# Geocode addresses using Nominatim (free, but rate-limited)
farmers_markets <- farmers_markets %>%
  geocode(mergedaddress, method = "osm", lat = latitude, long = longitude)

farmers_markets <- farmers_markets[!is.na(farmers_markets$latitude),]

### Convert to sf object
farmers_markets_sf <- st_as_sf(farmers_markets, coords = c("longitude", "latitude"), crs = 4326)



plot(farmers_markets_sf)


##### Zero overlap!!!! with farmers markets
ggplot() +
  # Plot food store points
  geom_sf(data = farmers_markets_sf, shape = 21, size = 3, alpha = 0.6) +  
  scale_fill_viridis_d() +  
  scale_color_viridis_d() + 
  
  # Overlay dat_grid (assuming `Any_Food_Retail` is a 1/0 column)
  geom_sf(data = dat_grid$geometry, aes(color = NA, alpha = 0.3)) +  
  scale_fill_manual(values = c("0" = "gray90", "1" = "red"), na.translate = FALSE, name = "Food Retail Grid") +
  
  theme_minimal() +
  labs(title = "Retail Store Categories with Grid Overlay")


lowcost_food <- fromJSON(here("data","raw" ,"free-and-low-cost-food-programs.json"))
lowcost_food <- lowcost_food[!is.na(lowcost_food$latitude),]
### Convert to sf object
lowcost_food_sf <- st_as_sf(lowcost_food, coords = c("longitude", "latitude"), crs = 4326)

##### Zero overlap!!!! with farmers markets
ggplot() +
  # Plot food store points
  # Overlay dat_grid (assuming `Any_Food_Retail` is a 1/0 column)
  geom_sf(data = dat_grid$geometry, aes(color = NA, alpha = 0.3)) +
  geom_sf(data = lowcost_food_sf, shape = 21, size = 3, alpha = 0.6) + 
  theme_minimal() +
  labs(title = "Retail Store Categories with Grid Overlay")

###### 

### test plot
ggplot(data = farmers_markets$markettype) +
  theme_minimal() +
  labs(title = "Food & Beverage Locations", fill = "Presence")



