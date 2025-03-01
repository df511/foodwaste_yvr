## load, compile, and clean data for food waste modelling

## Written by Daniel Forrest
## Started on February 27, 2025

# Load necessary libraries
library(sf)
library(terra)
library(dplyr)
library(readr)
library(tidyr)
library(readr)
library(ggplot2)
library(data.table)
library(rnaturalearth)
library(rnaturalearthdata)
library(cancensus)
library(viridis)
library(MASS)
library(here)
library(sf)
library(geojsonsf)
library(matrixStats)
library(ggmap)
library(tidyterra)
library(jsonlite)
library(exactextractr)
library(progressr)


#### The following script is divided into modules, depending on the data type and intended output, 
#### so as to reduce run time, repeat operations, and organize by source data
#### If you are running for the first time, set all of the following params to TRUE
#### IF you are running to only alter part of the data generating operation,
#### set the stages prior to the stage 

### Module 1: gen grids
gen_grids <- TRUE  # Change to FALSE to load data instead
### Module 2: gen food waste
gen_foodwaste <- TRUE  # Change to FALSE to load data instead
### Module 3: gen Canadian census dat (demographics)
gen_demog <- TRUE  # Change to FALSE to load data instead
### Module 4: read in land cover data, calculate proportion of cell containing eahc of 13 land cover classes
gen_lc <- TRUE
### Module 5: read in food retail location data
gen_foodretail <- TRUE


#########################################################################################################
############ Module 1: generate grids ###########################



if (gen_grids) {
### Set parameters

# Define grid cell size (e.g., 2x2)
### size of a single side in meters
#cell_size <- 2.2360679774998 ### for 5 sq. meters.
#cell_size <- 5 ## for 25 sq. meters.
cell_size <- 27.3861278 ### for ~750 sq. meters (I measured several parcels across the region and the range was ~600 to 900 sq. m)

#### build functions

# Function to create an internally fitting grid for each polygon
create_internal_grid <- function(polygon, cell_size) {
  bbox <- st_bbox(polygon)
  
  # Generate grid in metric units
  grid <- st_make_grid(polygon, cellsize = cell_size, square = TRUE, 
                       offset = c(bbox["xmin"], bbox["ymin"]))  
  
  # Convert to sf object
  grid_sf <- st_sf(geometry = grid)
  
  # Keep only full grid cells within the polygon
  grid_inside <- grid_sf[st_within(grid_sf, polygon, sparse = FALSE), ]
  
  return(grid_inside)
}



###### read files

lancaster <- st_read(here("data","sites","lancaster_sites.kml"))
weber <- st_read(here("data","sites","weber_sites.kml"))
weber_rep <- weber[1:3,] ## Weber sites I visited and surveyed (drop one outside of CoV borders)
plot(weber_rep) # check 
weber_lancaster <- do.call(rbind, list(lancaster, weber_rep))
#weber_lancaster_no43churchill <- do.call(rbind, list(lancaster, weber_rep2))

plot(weber_lancaster) # check

strathcona <- st_read(here("data","sites","strathcona_final.kml"))
plot(strathcona)
second_beach <- st_read(here("data","sites", "2nd_beach.kml"))
plot(second_beach)


sites <- do.call(rbind, list(lancaster, weber_rep, strathcona, second_beach))
sites_3857 <- st_transform(sites, crs = 3857)

save(sites_3857, file = here("data","sites_3857.Rdata"))


# Apply function to each polygon separately
grid_list <- lapply(st_geometry(sites_3857), create_internal_grid, cell_size = cell_size)

# Combine grids
final_grid <- do.call(rbind, grid_list)

# ✅ Calculate cell area in square meters
cell_areas <- st_area(final_grid)

# Print mean cell area (if needed)
mean_cell_area <- mean(cell_areas)
print(paste("Mean cell size:", mean_cell_area, "square meters"))

# # Plot results
# plot(st_geometry(sites_3857), col = "lightblue", border = "black")
# plot(st_geometry(final_grid), add = TRUE, border = "red")
# 

# Define output file name and resolution
pdf(here("figs","grid_plot_750m.pdf"), width = 80, height = 64)  # Adjust size as needed

# Plot the grid inside polygons
plot(st_geometry(sites_3857), col = "lightblue", border = "black")
plot(st_geometry(final_grid), add = TRUE, border = "red")

# Close the PDF device to save the file
dev.off()

save(final_grid, file = here("data","grid_clipped_750m.Rdata"))
#######
} else {
## file called "final_grid"
load(file = here("data", "grid_clipped_750m.Rdata"))
}

grid_clipped_750m <- final_grid
st_crs(grid_clipped_750m) <- 3857
# Add a unique ID column
grid_clipped_750m <- grid_clipped_750m %>%
  mutate(grid_id = row_number())


##############################################################################################
############ Module 2: import, clean, and grid-ify food waste data ###########################

if (gen_foodwaste) {

###### import food waste data
foodwaste <- read_csv(here("data", "food_waste_data_09_19_23.csv"))
foodwaste <-  foodwaste %>% fill(c(fid, site, date), .direction = "down")
foodwaste_poly <- st_read(here("data", "waste_receptacles.gpkg"))
#### set API key
options(cancensus.api_key = 'CensusMapper_e5809a95267db864f07fd91906dfdc24') ### insert your own CensusMapper key

#plot(foodwaste_poly)

foodwaste_poly$fid <- seq_len(nrow(foodwaste_poly)) #### replicate FID
merged_data <- merge(foodwaste_poly, foodwaste, by = "fid") ### merge data frames
# filtered_data <- merged_data[merged_data$fid == "155", ] ### Test view
#######remove spaces in column "site" 
merged_data$site <- gsub(" ", "", merged_data$site)

###### import survey data
surv_dat <- read_csv(here("data","survey_info_09_19_23.csv"))
#### filter out trial surveys
surv_dat$pickup_type[surv_dat$pickup_type == "no pickup"] <- "none"
surv_dat <- surv_dat[c(5:48),]
surv_dist <- read_csv(here("data","survey_area_distance.csv"))
surv_dist <- surv_dist[c(1:10),c(1:4)] #### NEED TO REPLACE 19_Yukon WITH SHORTENED SURVEY

###### merge survey data with food waste data
dat <- left_join(merged_data, surv_dat, by = c("site" = "site", "date" ="date"), relationship = "many-to-many")
dat <- left_join(dat, surv_dist, by = c("site" = "site"))


##### filter to only req'd columns
dat<- dat[,c(1,9:18, 23:36)]
## rename for interpretability
dat <- dat %>% rename(waste_type = type.y,
                      bin_form = form.y,
                      bin_state = state.y,
                      bin_contents = contents.y,
                      bin_count = count.y,
                      bin_notes = notes.y)

####### format and fill
dat <- as.data.table(dat)

dat$bin_count[is.na(dat$bin_count)] <- 1 #replace NAs with 1 (if missing, always a single bin/receptacle described)

# Filter out rows in "bin_count" with "_spot" in the string
dat <- dat[!grepl("_spot", site)]
#### Fix data entry error
#Identify the row number where the shift should occur
row_num <- 5375  
# Perform the shift #not performing as expected yet. 
dat[row_num, `:=`(fl_density = bin_count,
                  bin_contents = shift(dat[row_num, bin_contents], type = "lead"))]

# Function to convert columns from character to numeric
convert_to_numeric <- function(col) {
  # Check if the column is numeric (including integers and decimals)
  if (all(grepl("^\\d+\\.?\\d*$", col))) {
    as.numeric(col)
  } else {
    col
  }
}

# Identify numeric columns and convert them to numeric
dat[, (names(dat)) := lapply(.SD, convert_to_numeric), .SDcols = names(dat)]


print(dat)


##### convert to numeric
dat$bin_count <- as.numeric(dat$bin_count)
dat$bin_count[is.na(dat$bin_count)] <- 1 #replace NAs with 1 (if missing, always a single bin/receptacle described)
#remove spaces in column "site" 
dat[, site := gsub(" ", "", site)]

# Replace NAs and "no pickup" in the "pickup_type" column with "none"
dat[, pickup_type := ifelse(is.na(pickup_type) | pickup_type == "no pickup", "none", pickup_type)]
# Replace "garbage" in pickup_type with "misc." where site is "lancaster_fc"
dat[site == "lancaster_fc" & pickup_type == "garbage", pickup_type := "misc."]
dat[site == "lancaster_we", pickup_type := "misc."]
# Replace bin_form with "food_litter" where waste_type is "food_litter"
dat[waste_type == "food_litter", bin_form := "food_litter"]
# Replace bin_form with "food_litter" where waste_type is "food_litter"
dat[waste_type == "food_litter", bin_state := "food_litter"]

### create spatial data frame
dat_sf <- st_as_sf(dat, sf_column_name = "geometry")

######## CONSIDER ADJUSTING THESE BASED ON ASSUMPTIONS
# Define weights for each bin_state  #### will need SPP specific metric at some point
weights_state <- c( "food_litter" = 1, ### probably increase...
                    "open_w_food" = 0.5,
                    "open_inbag_food" = 0.25, #
                    "food_residue" = 0.25,
                    "restricted_food" = 0.1,
                    "open_unknown" = 0.1,
                    "open_inbag_unknown" = 0.05,
                    "restricted_unknown" = 0.05,
                    "restricted_nofood" = 0.05,
                    "open_no_food" = 0.05,
                    "open_empty" = 0.05,
                    "open_inbag_nofood" = 0.05,
                    "closed" = 0.01,
                    "locked" = 0.01,
                    "other" = 0.05,
                    "NA" = 0)


# Define weights for each bin_form  
weights_form <- c("dumpster" = 15,
                  "tote" = 3,
                  "home_bin" = 1,
                  "plastic_bag" = 1,
                  "food_litter" = 1,
                  "other" = 2,
                  "stationary" = 4, #about the same as a tote
                  "NA" = 1)

# Define weights for each waste_type
weights_type <- c("food_litter" = 10/10,
                  "compost" = 5/10,
                  "garbage" = 3/10,
                  "recycling-containers" = 2/10,
                  "recycling-glass" = 1/10,
                  "recycling-papercardboard" = 0/10,
                  "other" = 2/10,
                  "NA" = 1/10)

# Define weights for food litter density, multiply these by food litter area...c
weights_fld <- c("<visible" = 0.05,
                 "low" = 0.166,
                 "medium" = 0.5,
                 "high" = 0.833)



# Step 3: Calculate weighted values and adjustment factors
dat_sf <- dat_sf %>%
  mutate(
    state_weight = sapply(bin_state, function(x) weights_state[x]),
    form_weight = sapply(bin_form, function(x) weights_form[x]),
    type_weight = sapply(waste_type, function(x) weights_type[x]),
    fld_weight = sapply(fl_density, function(x) weights_fld[x]),
    # Apply additional multiplication for food_litter
    #additional_weight = ifelse(bin_form == "food_litter", (total_area * fld_weight/100), 1), ##### 
    weighted_value = bin_count * state_weight * form_weight * type_weight #* additional_weight 
  )


# If they are different, transform dat_sf to match grid_clipped_25m
dat_sf <- st_transform(dat_sf, st_crs(grid_clipped_750m))

# Compute centroids of the small polygons
dat_sf_centroids <- st_centroid(dat_sf)

# Perform a spatial join to associate each centroid with a grid cell
dat_sf_joined <- st_join(dat_sf_centroids, grid_clipped_750m, left = FALSE)

# Aggregate the weighted_value for each grid cell
aggregated_values <- dat_sf_joined %>%
  group_by(grid_id) %>%  # Replace grid_id with the actual identifier column in grid_clipped_25m
  summarise(total_weighted_value = sum(weighted_value, na.rm = TRUE))

# Merge back with the grid to retain geometries
dat_grid <- st_join(grid_clipped_750m, aggregated_values, left = TRUE)
dat_grid <- dat_grid %>%
  mutate(total_weighted_value = replace_na(total_weighted_value, 0))


dat_grid <- dat_grid %>%
  dplyr::select(-grid_id.y) %>%  # Drop "grid_id.y"
  dplyr::rename(grid_id = grid_id.x) %>%  # Rename "grid_id.x" to "grid_id"
  dplyr::rename(fw_score = total_weighted_value)  # Rename "total_weighted_value" to "fw_score"


#### Save and name file!!
save(dat_grid, file = here("data","dat_grid_750.Rdata"))

#######
} else {
## object called "dat_grid"
load(file = here("data", "dat_grid_750.Rdata"))
}

##############################################################################################
############ Module 3: import, clean, and grid-ify Canadian census data ###########################


if (gen_demog) {

### insert your own Google Maps key here
options(cancensus.api_key = 'CensusMapper_e5809a95267db864f07fd91906dfdc24') ### insert personalized API Key, sign-up here: https://censusmapper.ca/users/sign_up


### For how to use the cancensus package: https://cran.r-project.org/web/packages/cancensus/vignettes/cancensus.html


census_vec <- list_census_vectors(dataset ="CA21")

#read in census data via query of census mapper
census_data <- get_census(dataset = 'CA21', regions = list(CSD = "5915022"), 
                          
                          vectors = c("v_CA21_1","v_CA21_906", "v_CA21_6",
                                      "v_CA21_5808","v_CA21_5865", "v_CA21_4204", 
                                      "v_CA21_4875","v_CA21_4878", "v_CA21_4881",
                                      "v_CA21_4884", "v_CA21_4887", "v_CA21_4890",
                                      "v_CA21_4893", "v_CA21_4896", "v_CA21_4899",
                                      "v_CA21_4902", "v_CA21_4905", "v_CA21_4908",
                                      "v_CA21_4911","v_CA21_4914"),
                          labels = "detailed", geo_format = "sf", level = "DA")

#rename columns
census_data <- census_data %>% 
  rename(household_income = `v_CA21_906: Median total income of household in 2020 ($)`,
         population = `v_CA21_1: Population, 2021`,
         minority_pop = `v_CA21_4875: Total visible minority population`,
         black_pop = `v_CA21_4884: Black`, indig_pop = 'v_CA21_4204: Indigenous identity (39)',
         pop_km = `v_CA21_6: Population density per square kilometre`
  )

#calculate percemtages of ethnicities per population and create new cols in df
census_data$black_percent <- census_data$black_pop / census_data$population
census_data$indig_percent <- census_data$indig_pop / census_data$population
census_data$min_percent <- census_data$minority_pop / census_data$population

census_data <- census_data[, -c(2, 4, 5:7,9, 11:13)]  # Drops columns 2, 5, and 7


census_data <- st_transform(census_data, st_crs(dat_grid)) ### ensure the same CRS



# Perform a spatial join
dat_grid <- st_join(dat_grid, census_data)

### save in file, indicating contents (food waste + demographic vars)
save(dat_grid, file = here("data","dat_fw_demo_750.Rdata"))
} else {
  ## object called "dat_grid"
  load(file = here("data", "dat_fw_demo_750.Rdata"))
}


##############################################################################################
############ Module 3: import, clean, and grid-ify Metro Van land cover data ###########################

### expect this module to take ~3 minutes to run

if (gen_lc) {
file_path <- here("data","raw","MetroVan_LCC_2020_final.tif")

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
rm(lcc2020,lcc2020_van, lc_prop)
### save file
save(dat_grid, file = here("data","dat_fw_demo_lc_750.Rdata"))

} else {
  ## object called "dat_grid"
  load(file = here("data", "dat_fw_demo_lc_750.Rdata"))
}


#########################################################################################################
############## Module 5: read in food retail location data #################


if (gen_foodretail) {

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


# dat_grid_final <- with_progress({
#   p <- progressor(along = unique(dt_joined$grid_id))  # Progress for unique grid IDs
#   
#   result <- dt_joined[, .(
#     Food_Retail = as.integer(any(retail_category %in% c("Food & Beverage", "Convenience Goods", "Entertainment and Leisure"), na.rm = TRUE)),
#     geometry = geometry[1]  # Retain one geometry per grid cell
#   ), by = .(grid_id)]
#   
#   p()  # Update progress
#   
#   st_as_sf(result)  # Convert back to sf
# })

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

