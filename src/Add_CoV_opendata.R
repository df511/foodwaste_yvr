
library(here)
library(sf)
library(jsonlite)
library(dplyr)
library(ggplot2)


### Storefronts! ####
### Read storefronts file, metadata available here: https://opendata.vancouver.ca/explore/dataset/storefronts-inventory/information/
stores <- fromJSON(here("data","raw" ,"storefronts-inventory.json"))

### Extract lat/lon from the nested column
stores$longitude <- stores$geo_point_2d$lon
stores$latitude <- stores$geo_point_2d$lat
### Convert to sf object
stores_sf <- st_as_sf(stores, coords = c("longitude", "latitude"), crs = 3857)
### filter to only food related businesses
categories <- c("Food & Beverage", "Convenience Goods", "Entertainment and Leisure")

# Filter the sf dataframe
food_stores <- stores_sf %>%
  filter(retail_category %in% categories)

# Check results
print(food_stores)
plot(food_stores["retail_category"])  # Color by 'retail_category'



ggplot(data = food_stores) +
  geom_sf(aes(color = retail_category, fill = retail_category), 
          shape = 21, size = 3, alpha = 0.6) +  # Shape 21 allows fill + border
  scale_fill_viridis_d() +  # Use a nice color palette
  scale_color_viridis_d() + 
  theme_minimal() +
  labs(title = "Retail Store Categories")


## object called "dat_grid"
load(file = here("data", "dat_fw_demo_lc_25.Rdata"))
unique(food_stores$retail_category)
head(food_stores)
# 
# # Perform a spatial join (assigning points to grid cells)
# joined <- st_join(dat_grid, food_stores, left = TRUE)
# 
# # Create one-hot encoding: Convert "retail_category" into separate columns
# dat_grid_onehot <- joined %>%
#   mutate(
#     Food_Beverage = ifelse(retail_category == "Food & Beverage", 1, 0),
#     Convenience_Goods = ifelse(retail_category == "Convenience Goods", 1, 0),
#     Entertainment_Leisure = ifelse(retail_category == "Entertainment and Leisure", 1, 0)
#   ) %>%
#   group_by(grid_id) %>%  # Ensure multiple points don't count multiple times
#   summarise(
#     Food_Beverage = max(Food_Beverage, na.rm = TRUE),
#     Convenience_Goods = max(Convenience_Goods, na.rm = TRUE),
#     Entertainment_Leisure = max(Entertainment_Leisure, na.rm = TRUE),
#     geometry = first(geometry)  # Keep spatial data
#   ) %>%
#   st_as_sf()
# 

