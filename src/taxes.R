

### Tax data formatting

### Started March 12, 2025 by Daniel Forrest

library(dplyr)

library(stringr)


tax <- fromJSON(here("data","raw" ,"property-tax-report.json"))
properties <- st_read(here("data","raw" ,"property-parcel-polygons.geojson"))
load(here("data","sites_3857.Rdata"))

properties <- st_transform(properties, st_crs(sites_3857)) ### ensure the same CRS
prop_cropped <- st_crop(properties, sites_3857)


tax <- tax %>%
  mutate(from_civic_number = as.integer(from_civic_number))  # Converts "001" → 1

tax <- tax %>%
  filter(tax_assessment_year == "2023")



# Function to fix directional prefixes
fix_street_name <- function(street) {
  street <- str_to_upper(str_trim(street))  # Convert to uppercase and trim spaces
  street <- str_replace_all(street, "\\s+", " ")  # Remove extra spaces
  
  # Ensure direction is always at the front (e.g., "BROADWAY E" → "E BROADWAY")
  street <- str_replace(street, "^(.*)\\s(E|W|N|S)$", "\\2 \\1")
  
  return(street)
}

# Apply function to both datasets
tax <- tax %>%
  mutate(street_name = fix_street_name(street_name))

prop_cropped <- prop_cropped %>%
  mutate(streetname = fix_street_name(streetname))

prop_cropped$area_m2 <- st_area(prop_cropped)  # Adds area column (m²)

prop_joined <- prop_cropped %>%
  inner_join(tax, by = c("tax_coord" = "land_coordinate"), relationship = "many-to-many")

prop_weighted <- prop_joined %>%
  group_by(tax_coord) %>%
  mutate(weight = n())  # Count rows per tax_coord

prop_weighted <- prop_weighted %>%
  summarise(
    weighted_land_value = sum(current_land_value * weight, na.rm = TRUE) / (sum(weight, na.rm = TRUE) * mean(area_m2, na.rm = TRUE)),
    weighted_improvement_value = sum(current_improvement_value * weight, na.rm = TRUE) / (sum(weight, na.rm = TRUE) * mean(area_m2, na.rm = TRUE))
  )


prop_weighted <- prop_joined %>%
  group_by(tax_coord) %>%
  summarise(
    weight = n(),  # Count of rows per tax_coord
    weighted_land_value = sum(current_land_value * weight, na.rm = TRUE) / (sum(weight, na.rm = TRUE) * mean(as.numeric(area_m2), na.rm = TRUE)),
    weighted_improvement_value = sum(current_improvement_value * weight, na.rm = TRUE) / (sum(weight, na.rm = TRUE) * mean(as.numeric(area_m2), na.rm = TRUE))
  ) %>%
  ungroup()  # Ensure the result is no longer grouped



tax_space <- prop_weighted[,c(3,4)]

tax_grid_joined <- st_join(dat_grid_final, tax_space, join = st_intersects)

tax_grid_joined <- tax_grid_joined %>%
  group_by(grid_id) %>%  # Assuming grid cells have a unique ID column
  summarise(
    mean_land_value = mean(weighted_land_value, na.rm = TRUE),
    mean_land_imp_value = mean(weighted_improvement_value, na.rm = TRUE)
  )


dat_grid_final <- dat_grid_final %>%
  left_join(st_drop_geometry(tax_grid_joined), by = "grid_id")


#### considering interactions
dat_grid_final$rent_val_interaction <- dat_grid_final$rent_pct*dat_grid_final$mean_land_value

dat_grid_final$own_val_interaction <- dat_grid_final$own_pct*dat_grid_final$mean_land_value



