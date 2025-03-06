
### Clean YVR food waste data, collected in Vancouver, BC, Canada, May through August, 2023

### written by Daniel Forrest
### Started February 6, 2025 

#### load libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(sf)
library(data.table)
library(ggplot2)
#library(osmdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(cancensus)
library(viridis)
library(MASS)
library(here)


###### import food waste data
foodwaste <- read_csv(here("data", "food_waste_data_09_19_23.csv"))
foodwaste <-  foodwaste %>% fill(c(fid, site, date), .direction = "down")
foodwaste_poly <- st_read(here("data", "waste_receptacles.gpkg"))
#### set API key
options(cancensus.api_key = 'CensusMapper_e5809a95267db864f07fd91906dfdc24')

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
dat <- left_join(merged_data, surv_dat, by = c("site" = "site", "date" ="date"))
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


result <- dat[, .(sum_bin_count = sum(bin_count)), by = .(site, pickup_type)]


############# # plot bin counts per site per pickup type 
# ggplot(result, aes(x = site, y = sum_bin_count, fill = pickup_type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Sum of Bin Count per Site by Pickup Type",
#        x = "Site",
#        y = "Sum Bin Count",
#        fill = "Pickup Type") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
############################ spatial_plots


### create spatial data frame
dat_sf <- st_as_sf(dat, sf_column_name = "geometry")




# Plot the spatial geometries color-coded by bin_form
ggplot() +
  geom_sf(data = sites_3857, aes(color = "yellow")) +
  geom_sf(data= dat_sf, aes(color = bin_form)) +
  theme_minimal() +
  labs(title = "Spatial Geometries Color-coded by Bin Form",
       color = "Bin Form")




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


## file called "final_grid"
load(file = here("data", "grid_clipped_25m.Rdata"))
grid_clipped_25m <- final_grid

st_crs(grid_clipped_25m) <- 3857
# Add a unique ID column
grid_clipped_25m <- grid_clipped_25m %>%
  mutate(grid_id = row_number())


# If they are different, transform dat_sf to match grid_clipped_25m
dat_sf <- st_transform(dat_sf, st_crs(grid_clipped_25m))

# Compute centroids of the small polygons
dat_sf_centroids <- st_centroid(dat_sf)

# Perform a spatial join to associate each centroid with a grid cell
dat_sf_joined <- st_join(dat_sf_centroids, grid_clipped_25m, left = FALSE)

# Aggregate the weighted_value for each grid cell
aggregated_values <- dat_sf_joined %>%
  group_by(grid_id) %>%  # Replace grid_id with the actual identifier column in grid_clipped_25m
  summarise(total_weighted_value = sum(weighted_value, na.rm = TRUE))

# Merge back with the grid to retain geometries
dat_grid <- st_join(grid_clipped_25m, aggregated_values, left = TRUE)
dat_grid <- dat_grid %>%
  mutate(total_weighted_value = replace_na(total_weighted_value, 0))


dat_grid <- dat_grid %>%
  dplyr::select(-grid_id.y) %>%  # Drop "grid_id.y"
  dplyr::rename(grid_id = grid_id.x) %>%  # Rename "grid_id.x" to "grid_id"
  dplyr::rename(fw_score = total_weighted_value)  # Rename "total_weighted_value" to "fw_score"



save(dat_grid, file = here("data","dat_grid_25.Rdata"))

# Define output file name and resolution
pdf(here("figs","food_waste_score_map_750m.pdf"), width = 80, height = 64)  # Adjust size as needed

ggplot(dat_grid) +
  geom_sf(aes(fill = log(fw_score)), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Weighted Value") +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()

#######