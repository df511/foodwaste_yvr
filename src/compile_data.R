## load, compile, and clean data for food waste modelling

## Written by Daniel Forrest
## Started on February 27, 2025

# Load necessary libraries
library(sf)
library(terra)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(readr)
library(stringr)
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
#### If you are running for the first time, set all of the following parameters to TRUE
#### If you are running to alter a portion of the data compilation code,
#### set all stages prior to the focal stage to FALSE

### Module 1: gen grids
gen_grids <- FALSE  # Change to FALSE to load data instead
### Module 2: gen food waste
gen_foodwaste <- FALSE  # Change to FALSE to load data instead
### Module 3: gen Canadian census dat (demographics)
gen_demog <- TRUE  # Change to FALSE to load data instead
### Module 4: read in land cover data, calculate proportion of cell containing each of 13 land cover classes
gen_lc <- TRUE
### Module 5: read in food retail location data
gen_foodretail <- TRUE
### Module 6: read in food retail location data
gen_shelters <- TRUE
### Module 7: read in property values from tax data
gen_propvals <- TRUE

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
  
  # Function to create an internally fitting grid for each polygon with its site name
  create_internal_grid <- function(polygon, cell_size, site_name) {
    bbox <- st_bbox(polygon)
    
    # Generate grid in metric units
    grid <- st_make_grid(polygon, cellsize = cell_size, square = TRUE, 
                         offset = c(bbox["xmin"], bbox["ymin"]))  
    
    # Convert to sf object
    grid_sf <- st_sf(geometry = grid)
    
    # Keep only full grid cells within the polygon
    grid_inside <- grid_sf[st_within(grid_sf, polygon, sparse = FALSE), ]
    
    # Add site name as an attribute
    grid_inside$name <- site_name
    
    return(grid_inside)
  } 
  
  
  ###### read files
  
  lancaster <- st_read(here("data","sites","lancaster_sites.kml"))
  weber <- st_read(here("data","sites","weber_site_edits.kml"))
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
  sites_3857$Name <- c("lancaster_cw","lancaster_sh", "lancaster_mk" ,
                       "lancaster_qe", "lancaster_fc",  "lancaster_dt",
                       "lancaster_we", "lancaster_ub",
                       "43_churchill","14_spruce", "19_yukon",  "strathcona", "2nd_beach")
  
  # ### plot to check correct naming
  # # Ensure 'name' column exists
  # if (!"Name" %in% colnames(sites_3857)) {
  #   stop("Column 'name' not found in sites_3857!")
  # }
  # 
  # # Create the plot
  # ggplot() +
  #   geom_sf(data = sites_3857, fill = "lightblue", color = "black") +  # Plot polygons
  #   geom_sf_text(data = sites_3857, aes(label = Name), size = 3, color = "red") +  # Add names as text
  #   theme_minimal() +
  #   labs(title = "Sites with Names", x = "Longitude", y = "Latitude")
  # 
  # 
  save(sites_3857, file = here("data","sites_3857.Rdata"))
  
  
  # Apply function to each polygon, retaining "name" column
  grid_list <- mapply(create_internal_grid, 
                      st_geometry(sites_3857), 
                      cell_size, 
                      sites_3857$Name, 
                      SIMPLIFY = FALSE)
  
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

### calculate number of site visits
total_visits <- surv_dat %>%
  group_by(site) %>%
  summarize(total_visits = n_distinct(date))


###### merge survey data with food waste data
dat <- left_join(merged_data, surv_dat, by = c("site" = "site", "date" ="date"), relationship = "many-to-many")
dat <- left_join(dat, surv_dist, by = c("site" = "site"))
##Join the unique visits data with the main data
dat <- dat %>%
  left_join(total_visits, by = "site")

##### filter to only req'd columns
dat<- dat[,c(1,9:18, 23:36, 39)]
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
#dat[row_num, `:=`(fl_density = bin_count,
#                  bin_contents = shift(dat[row_num, bin_contents], type = "lead"))]


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

# ### create spatial data frame
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



# Vectorized lookup using match() instead of sapply()
dat_sf <- dat_sf %>%
  mutate(
    state_weight = weights_state[match(bin_state, names(weights_state))],
    form_weight = weights_form[match(bin_form, names(weights_form))],
    type_weight = weights_type[match(waste_type, names(weights_type))],
    fld_weight = weights_fld[match(fl_density, names(weights_fld))],
    weighted_value = bin_count * state_weight * form_weight * type_weight
  )

# 
# 
# # Convert to data.table if not already
# setDT(dat_sf)

# # Summarize using data.table syntax
# summed_df <- dat[, .(sum_weighted_value = sum(weighted_value, na.rm = TRUE)), 
#                     by = .(fid,site, pickup_type, date)]


# If they are different, transform dat_sf to match grid_clipped_25m
dat_sf <- st_transform(dat_sf, st_crs(grid_clipped_750m))

# Compute centroids of the small polygons
dat_sf_centroids <- st_centroid(dat_sf)

# Perform a spatial join to associate each centroid with a grid cell
dat_sf_joined <- st_join(dat_sf_centroids, grid_clipped_750m, left = FALSE)

# # Convert to data.table if not already
setDT(dat_sf_joined)

# Create adjusted_sum_weighted_value column with case-like logic
dat_sf_joined[, adjusted_weighted_value := weighted_value * fifelse(
  pickup_type == "misc.", 1 / total_visits,
  fifelse(pickup_type %in% c("recycling", "garbage"), 1 / 14,
          fifelse(pickup_type == "none", 12 / 14, 1)))  # Default case: 1
]

fw_scores <- dat_sf_joined[, .(fw_score_weighted = sum(adjusted_weighted_value, na.rm = TRUE),
                                       fw_score_max = max(adjusted_weighted_value, na.rm = TRUE)), 
                                   by = .(grid_id)]  # Replace grid_id with actual column name



dat_grid <- merge(grid_clipped_750m, fw_scores, by = "grid_id", all.x = TRUE)

dat_grid <- dat_grid %>%
  mutate(fw_score_weighted = replace_na(fw_score_weighted, 0),
         fw_score_max= replace_na(fw_score_max, 0))

# Access the attribute data of the sf object and replace -Inf with NA
dat_grid[] <- lapply(dat_grid[], function(x) {
  if (is.numeric(x)) {
    x[x == -Inf] <- NA
  }
  return(x)
})

dat_grid <- dat_grid %>%
  dplyr::rename(site = name)


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
                                      "v_CA21_4238", "v_CA21_4239",
                                      "v_CA21_4875","v_CA21_4878", "v_CA21_4881",
                                      "v_CA21_4884", "v_CA21_4887", "v_CA21_4890",
                                      "v_CA21_4893", "v_CA21_4896", "v_CA21_4899",
                                      "v_CA21_4902", "v_CA21_4905", "v_CA21_4908",
                                      "v_CA21_4911","v_CA21_4914",
                                      "v_CA21_9", "v_CA21_10",## gender
                                      "v_CA21_4297", "v_CA21_4298", "v_CA21_4299", "v_CA21_4300", ##housing suitability, repairs...
                                      "v_CA21_435","v_CA21_436","v_CA21_437","v_CA21_438", "v_CA21_439", "v_CA21_440", ##housing types; https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=144257
                                      "v_CA21_4392", "v_CA21_4401", "v_CA21_4407", "v_CA21_4410", ##immigration... citizens/non-citizens, non-immigrants, immigrants
                                      "v_CA21_4923", "v_CA21_4938"), ### ethnicity, English, Chinese, re: Ley 1995
                          labels = "detailed", geo_format = "sf", level = "DA")


var <- colnames(census_data[,41])
var <- var[1]
clean_var1 <- gsub("\\\\", "", var)
var <- colnames(census_data[,42])
var <- var[1]
clean_var2 <- gsub("\\\\", "", var)
var <- colnames(census_data[,43])
var <- var[1]
clean_var3 <- gsub("\\\\", "", var)
var <- colnames(census_data[,44])
var <- var[1]
clean_var4 <- gsub("\\\\", "", var)

#rename columns
census_data <- census_data %>% 
  rename(household_income = `v_CA21_906: Median total income of household in 2020 ($)`,
         population = `v_CA21_1: Population, 2021`,
         minority_pop = `v_CA21_4875: Total visible minority population`,
         black_pop = `v_CA21_4884: Black`, 
         indig_pop = `v_CA21_4204: Indigenous identity (39)`,
         pop_km = `v_CA21_6: Population density per square kilometre`,
         owners = `v_CA21_4238: Owner`,
         renters = `v_CA21_4239: Renter`,
         female = `v_CA21_10: Total - Age`,
         male = `v_CA21_9: Total - Age`,
         single_detached = `v_CA21_435: Single-detached house`,
         semi_detached = `v_CA21_436: Semi-detached house`,
         row_houses = `v_CA21_437: Row house`,
         apt_flat = `v_CA21_438: Apartment or flat in a duplex`,
         low_rise = `v_CA21_439: Apartment in a building that has fewer than five storeys`,
         high_rise = `v_CA21_440: Apartment in a building that has five or more storeys`,
         thirtyonshelter_notsuitable = !!sym(clean_var1),
         thirtyonshelter_majorrepairs = !!sym(clean_var2),
         notsuitable_majorrepairs = !!sym(clean_var3),
         thirtyonshelter_notsuitable_majorepairs = !!sym(clean_var4),
         citizens = `v_CA21_4392: Canadian citizens`,
         non_citizens = `v_CA21_4401: Not Canadian citizens`,
         non_immigrants = `v_CA21_4407: Non-immigrants`, 
         immigrants = `v_CA21_4410: Immigrants`,
         English = `v_CA21_4923: English`,
         Chinese = `v_CA21_4938: Chinese`
         )

#calculate percemtages of ethnicities per population and create new cols in df
census_data$black_pct <- census_data$black_pop / census_data$population
census_data$indig_pct <- census_data$indig_pop / census_data$population
census_data$min_pct <- census_data$minority_pop / census_data$population
census_data$own_pct <- census_data$owners / census_data$population
census_data$rent_pct <- census_data$renters / census_data$population
census_data$male_pct <- census_data$male / census_data$population
census_data$female_pct <- census_data$female / census_data$population
census_data$english_pct <- census_data$English / census_data$population
census_data$chinese_pct <- census_data$Chinese / census_data$population
census_data$citizen_pct <- census_data$citizens/ census_data$population
census_data$noncitizen_pct <- census_data$non_citizens / census_data$population
census_data$nonimmigrant_pct <- census_data$non_immigrants/ census_data$population
census_data$immigrant_pct <- census_data$immigrants / census_data$population

census_data <- census_data[, -c(2, 4,6,7,9, 11:13)]  # Drops columns 2, 5, and 7

MSDI <- read_excel(here("data","raw", "INDQ_MSDI_Canada", "A-MSDIData_Can2021_en", "1. EquivalenceTableCanada2021_en.xlsx"))

# Filter the dataframe
MSDI_yvr <- MSDI %>% 
  filter(MUNIC == "5915022")

MSDI_yvr <- MSDI_yvr %>% mutate(DA = as.character(DA))

census_data <- census_data %>%
  left_join(MSDI_yvr %>% select(DA, SCOREMAT, SCORESOC), by = c("name" = "DA"))


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

### Read vendors file, metadata available here: https://opendata.vancouver.ca/explore/dataset/food-vendors/information/
vendors <- fromJSON(here("data","raw" ,"food-vendors.json"))

### Extract lat/lon from the nested column
vendors$longitude <- vendors$geo_point_2d$lon
vendors$latitude <- vendors$geo_point_2d$lat
### Convert to sf object
vendors_sf <- st_as_sf(vendors, coords = c("longitude", "latitude"), crs = 4326)


## object called "dat_grid"
load(file = here("data", "dat_fw_demo_lc_750.Rdata"))

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



lowcost_food <- fromJSON(here("data","raw" ,"free-and-low-cost-food-programs.json"))
lowcost_food <- lowcost_food[!is.na(lowcost_food$latitude),]
### Convert to sf object
lowcost_food_sf <- st_as_sf(lowcost_food, coords = c("longitude", "latitude"), crs = 4326)
lowcost_food_sf <- lowcost_food_sf %>%
  mutate(lon = st_coordinates(.)[,1],  # X = longitude
         lat = st_coordinates(.)[,2])  # Y = latitude
lowcost_food_sf <- st_transform(lowcost_food_sf, st_crs(dat_grid))

lowcost_food_sf$retail_category <- "free_lowcost_food"
lowcost_food_sf <- lowcost_food_sf %>%
  rename(business_name = program_name)

lowcost_food_sf  <- lowcost_food_sf  %>%
  mutate(year = substr(as.character(last_update_date), 1, 4)) %>%  # Extract year
  select(-last_update_date)  # Optionally drop original column

lowcost_food_sf  <- lowcost_food_sf[, c(1,24:28)]
lowcost_food_sf$retail_category <- "free_lowcost_food"
lowcost_food_sf <- lowcost_food_sf %>%
  rename(year_recorded = year)

food_stores <- bind_rows(food_stores, lowcost_food_sf)


restaurants_CoV <- st_read(here("data","restaurants_fastfood_CoV_quickOSM.geojson"))

restaurants_CoV <- restaurants_CoV %>% 
  select(amenity, shop, name)

restaurants_CoV <- restaurants_CoV %>% 
  rename(retail_category =amenity,
         business_name = name)
restaurants_CoV <- restaurants_CoV %>%
  mutate(retail_category = ifelse(is.na(retail_category), shop, retail_category)) %>%
  select(-shop)


restaurants_CoV <- restaurants_CoV %>%
  mutate(lon = st_coordinates(.)[,1],  # X = longitude
         lat = st_coordinates(.)[,2]) 

restaurants_CoV$year_recorded <- "2025"


food_stores <- bind_rows(food_stores, restaurants_CoV)



# ggplot() +
#   geom_sf(data = food_stores, aes(color = retail_category, fill = retail_category), 
#           shape = 21, size = 3, alpha = 0.6) +  # Shape 21 allows fill + border
#   scale_fill_viridis_d() +  # Use a nice color palette
#   scale_color_viridis_d() + 
#   geom_sf(data = sites_3857, color = "orange", fill = NA) +
#   theme_minimal() +
#   labs(title = "Retail Store Categories")


# Perform spatial join: Assigns food store points to grid cells
joined <- st_join(dat_grid, food_stores, left = TRUE) 




# Convert to data.table
dt_joined <- as.data.table(joined)

# Convert `retail_category` to a factor for faster comparison
dt_joined[, retail_category := as.factor(retail_category)]

retail_categories <- unique(food_stores$retail_category)
retail_categories <- retail_categories[!retail_categories %in% c("NA", NA)]


# # Enable progress bar
handlers(global = TRUE)
dat_grid_final <- with_progress({
  p <- progressor(along = unique(dt_joined$grid_id))  # Progress for unique grid IDs
  
  # Identify grid cells containing at least one relevant food retail location
  food_grids <- dt_joined[, .(
    Food_Retail = as.integer(any(retail_category %in% retail_categories, na.rm = TRUE))
  ), by = .(grid_id)]
  
  p()  # Update progress
  
  # Merge back with original data to retain all columns
  result <- merge(dt_joined, food_grids, by = "grid_id", all.x = TRUE)
  
  st_as_sf(result)  # Convert back to sf
  
})

dat_grid_final <- dat_grid_final[,-c(79:83)]

#### gen nearest food retailer column
dat_grid_final <- dat_grid_final %>%
  mutate(nearest_food_retail = apply(st_distance(geometry, food_stores), 1, min))

### test plot
ggplot(dat_grid_final) +
  geom_sf(aes(fill = factor(Food_Retail)), color = NA) +  # Remove borders
  scale_fill_manual(values = c("0" = "gray90", "1" = "red")) +
  theme_minimal() +
  labs(title = "Food & Beverage Locations", fill = "Presence")





save(dat_grid_final, file = here("data","dat_fw_demo_lc_retail_750.Rdata"))



} else {
  ## object called "dat_grid"
  load(file = here("data", "dat_fw_demo_lc_retail_750.Rdata"))
}


#########################################################################################################
############## Module 6: read in nearest homeless shelter #################

if (gen_shelters) {
### data sourced from: https://opendata.vancouver.ca/explore/dataset/homeless-shelter-locations/information/
shelters <- st_read(here("data","raw","homeless-shelter-locations"))

shelters <- shelters %>%
  mutate(lon = st_coordinates(.)[,1],  # X = longitude
         lat = st_coordinates(.)[,2])  # Y = latitude
shelters <- st_transform(shelters, st_crs(dat_grid_final))


shelters$year <- "2025"

shelters <- shelters[, c(1,9:11)]

# Compute the minimum distance from each grid cell to the nearest shelter
dat_grid_final <- dat_grid_final %>%
  mutate(nearest_shelter_dist = apply(st_distance(geometry, shelters), 1, min))
save(dat_grid_final, file = here("data","dat_fw_demo_lc_retail_shelters_750.Rdata"))



} else {
  ## object called "dat_grid_final"
  load(file = here("data", "dat_fw_demo_lc_retail_shelters_750.Rdata"))
}
