
### Clean food waste data

### written by Daniel Forrest
### February 6, 2025 

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

dat_sf <- st_as_sf(dat, sf_column_name = "geometry")

# Plot the spatial geometries color-coded by bin_form
ggplot(dat_sf) +
  geom_sf(aes(color = bin_form)) +
  theme_minimal() +
  labs(title = "Spatial Geometries Color-coded by Bin Form",
       color = "Bin Form")

# Filter the data for bin_form "dumpster"
dumpster_sf <- dat_sf[dat_sf$bin_form == "dumpster", ]

# Plot the spatial geometries for bin_form "dumpster" color-coded by bin_form
ggplot(dumpster_sf) +
  geom_sf(aes(fill = bin_form, color = bin_form)) +
  theme_minimal() +
  labs(title = "Spatial Geometries for Bin Form 'Dumpster'",
       fill = "Bin Form",
       color = "Bin Form") # Label for the legend


region <- list_census_regions("CA16") %>% filter(name=="Vancouver",level=="CSD")
vancouver <- get_census("CA16",regions=as_census_region_list(region),geo_format = "sf",level = "Regions")
bbox=st_bbox(vancouver)


# Plot the spatial geometries for bin_form "dumpster" with the City of Vancouver boundary
ggplot()+
  geom_sf(data = vancouver$geometry , fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(data = dumpster_sf, aes(fill = bin_form, color = bin_form)) +
  scale_color_manual(values = c("dumpster" = "#1f78b4")) + # Customize color for "dumpster"
  scale_fill_manual(values = c("dumpster" = "#1f78b4")) + # Same color for fill
  theme_minimal() +
  labs(title = "Spatial Geometries for Bin Form 'Dumpster' with Vancouver Boundary",
       fill = "Bin Form",
       color = "Bin Form") # Label for the legend

# Filter the data for bin_form "dumpster"
stationary_sf <- dat_sf[dat_sf$bin_form == "food", ]

# Plot the spatial geometries for bin_form "dumpster" color-coded by bin_form
ggplot(stationary_sf) +
  geom_sf(data = vancouver$geometry , fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(aes(fill = bin_form, color = bin_form)) +
  theme_minimal() +
  labs(title = "Spatial Geometries for Bin Form 'stationary'",
       fill = "Bin Form",
       color = "Bin Form") # Label for the legend



# Plot the spatial geometries color-coded by bin_form and outlined with the same color
ggplot(dat_sf) +
  geom_sf(data = vancouver$geometry , fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(aes(fill = bin_form, color = bin_form)) + # Using fill and color to match
  theme_minimal() +
  labs(title = "Spatial Geometries Color-coded by Bin Form",
       fill = "Bin Form",
       color = "Bin Form") # Label for the legend


################################
##### read in transects data

lancaster_path <- './initial_setup/lancaster_sites.kml'
lancaster <- st_read(lancaster_path)

weber_path <- './initial_setup/weber_sites.kml'
weber <- st_read(weber_path)
weber_rep <- weber[1:3,]
weber_lancaster <- do.call(rbind, list(lancaster, weber_rep))
#### PLOT 
#plot(weber_lancaster)

site_geoms <- weber_lancaster[,c(1,3)]
site_geoms$Name <- tolower(site_geoms$Name)

# Plot the site boundaries overlaid with names to check for name replacement
ggplot() +
  geom_sf(data = site_geoms$geometry) +
  geom_sf_label(data = site_geoms, aes(label = Name), size = 2.5, color = "black") +
  theme_void()


# Replace "weber3" with "43_churchill" in the Name column
site_geoms$Name[site_geoms$Name == "weber3"] <- "43_churchill"
# Replace "weber1" with "43_churchill" in the Name column
site_geoms$Name[site_geoms$Name == "weber2"] <- "14_spruce"
# Replace "weber3" with "43_churchill" in the Name column
site_geoms$Name[site_geoms$Name == "weber1"] <- "19_yukon"

site_geoms$area <- st_area(site_geoms)

site_geoms <- site_geoms %>%
  rename(site = Name,
         site_area = area) #### rename column to match other df


# Plot food waste data over surveyed plots 
ggplot() +
  geom_sf(data = vancouver$geometry , fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(dat = weber_lancaster) +
  geom_sf(dat = dat_sf, aes(color = bin_form)) +
  theme_minimal() +
  labs(title = "Spatial Geometries Color-coded by Bin Form",
       color = "Bin Form")


# Calculate the area of each polygon
dat[, area_m2 := st_area(geometry)]

# Group by site and sum areas
total_area_per_site <- dat[, .(total_area_m2 = sum(area_m2, na.rm = TRUE)), by = site]

# Print the result
print(total_area_per_site)

# Summarize the total area per unique value in the "site" and "bin_type" columns
total_area_per_site_bin_type <- dat[, .(total_area = sum(area_m2, na.rm = TRUE)), by = .(site, bin_form, bin_state, pickup_type, waste_type, fl_density,date)]

# Print the result
print(total_area_per_site_bin_type)

# Convert the data.table to a data frame if needed
total_area_df <- as.data.frame(total_area_per_site_bin_type)

# Convert the 'total_area' column to numeric
total_area_df$total_area <- as.numeric(total_area_df$total_area)


test <- left_join(total_area_df, surv_dist, by = "site")
total_area_df <- left_join(total_area_df, site_geoms, by = c("site"))
total_area_df <- total_area_df %>%
  mutate(area_prop = total_area / site_area)
total_area_df$area_prop <- as.numeric(total_area_df$area_prop)

#### remove 2023 additions
total_area_df <- total_area_df %>%
  filter(!site %in% c("2nd_beach", "strathcona"))

####### sum bins by site, pickup type, form, accessibility state, and type of waste
result <- dat[, .(sum_bin_count = sum(bin_count)), by = .(site, bin_form, bin_state, pickup_type, waste_type, fl_density, date)]

#### remove 2023 additions
filtered_df <- result %>%
  filter(!site %in% c("2nd_beach", "strathcona"))

df <- merge(filtered_df, total_area_df)

df <- df[-468,]#### erroneous

############ CREATE THE WEIGHTED METRIC

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

# Define weights for food litter density, multiply these by food litter area...
weights_fld <- c("<visible" = 0.05,
                 "low" = 0.166,
                 "medium" = 0.5,
                 "high" = 0.833)


###### for garbage and recycling days, each are 1/14, 1/14; for none weight by 12/14
###### for misc. each gets even weight, so divide by number of visits 


### calculate number of site visits
unique_visits <- surv_dat %>%
  group_by(site) %>%
  summarize(unique_visits = n_distinct(date))

##Join the unique visits data with the main data
df <- df %>%
  left_join(unique_visits, by = "site")


# Step 3: Calculate weighted values and adjustment factors
df <- df %>%
  mutate(
    state_weight = sapply(bin_state, function(x) weights_state[x]),
    form_weight = sapply(bin_form, function(x) weights_form[x]),
    type_weight = sapply(waste_type, function(x) weights_type[x]),
    fld_weight = sapply(fl_density, function(x) weights_fld[x]),
    # Apply additional multiplication for food_litter
    #additional_weight = ifelse(bin_form == "food_litter", (total_area * fld_weight/100), 1), ##### 
    weighted_value = sum_bin_count * state_weight * form_weight * type_weight #* additional_weight 
  )

# Step 4: Sum the weighted values by pickup_type for each site
summed_df <- df %>%
  group_by(site, pickup_type, date) %>%
  summarize(sum_weighted_value = sum(weighted_value, na.rm = TRUE), .groups = 'drop')

# Step 5: Multiply these sums by the adjustment factors
weighted_df <- summed_df %>%
  mutate(adjusted_sum_weighted_value = sum_weighted_value * case_when(
    pickup_type == "misc." ~ 1 / unique_visits_df$unique_visits[match(site, unique_visits_df$site)],
    pickup_type == "recycling" ~ 1 / 14,
    pickup_type == "garbage" ~ 1 / 14,
    pickup_type == "none" ~ 12 / 14,
    TRUE ~ 1  # Default case, though not expected
  ))

# Step 6: Calculate the weighted mean value for each site
final_df <- weighted_df %>%
  group_by(site) %>%
  summarize(food_waste_accessibility = sum(adjusted_sum_weighted_value, na.rm = TRUE), .groups = 'drop')

# Display the result
print(final_df)




# Create a bar plot
ggplot(final_df, aes(x = site, y = food_waste_accessibility)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Food Waste Accessibility by Site",
       x = "Site Name",
       y = "Food Waste Accessibility")



food_waste_inspace_wmean <- left_join(site_geoms, final_df, by = "site")


# Plot food waste data over surveyed plots
ggplot() +
  geom_sf(data = vancouver$geometry, fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(data = food_waste_inspace_wmean$geometry) +
  geom_sf(data = food_waste_inspace_wmean, aes(fill = food_waste_accessibility)) +
  scale_fill_viridis(option = "viridis", direction = -1) + # Use viridis color map with dark colors for high values
  theme_minimal() +
  labs(
    title = "Food Waste Accessibility per Site (Weighted Mean)",
    fill = "Food Waste Accessibility"
  )

####### For Max value

# Step 6: Calculate the maximum adjusted_sum_weighted_value for each site
max_df <- weighted_df %>%
  group_by(site) %>%
  summarize(max_food_waste_accessibility = max(sum_weighted_value, na.rm = TRUE), .groups = 'drop')

# Display the result
print(max_df)

food_waste_inspace_max <- left_join(site_geoms, max_df, by = "site")


# Plot food waste data over surveyed plots
ggplot() +
  geom_sf(data = vancouver$geometry, fill = NA, color = "black", size = 1.5) + # Outline of Vancouver
  geom_sf(data = food_waste_inspace_max$geometry) +
  geom_sf(data = food_waste_inspace_max, aes(fill = max_food_waste_accessibility)) +
  scale_fill_viridis(option = "viridis", direction = -1) + # Use viridis color map with dark colors for high values
  theme_minimal() +
  labs(
    title = "Food Waste Accessibility per Site (Maximum Visit)",
    fill = "Food Waste Accessibility"
  )

####### Merge food waste scores

foodwaste_scores <- merge(max_df,final_df)


########

transect_covers <- st_read("./data/transect_cover_freq.shp")
transect_covers$Name <- tolower(transect_covers$Name)

foodwaste_scores <- foodwaste_scores %>%
  mutate(site = case_when(
    site == "14_spruce" ~ "weber_sp",
    site == "19_yukon" ~ "weber_yk",
    site == "43_churchill" ~ "weber_ch",
    TRUE ~ site  # Retain other values as is
  ))

df <- left_join(transect_covers, foodwaste_scores, by = c("Name" = "site"))
df  <- df  %>% rename(max_foodwaste = max_food_waste_accessibility,
                      mean_foodwaste = food_waste_accessibility)


df <- df[-c(12),]

# Replace values in the 'transect' column with the last two letters, upper-case
df <- df %>%
  mutate(Name = toupper(substr(Name, nchar(Name) - 1, nchar(Name))))


# Reorder dataframe by alphabetical order of "Name" column
df <- df[order(df$Name), ]

df <- as.data.frame(df)

# Remove columns "geometry" and "Description"
df <- df[, !(names(df) %in% c("geometry", "Descriptio"))]


# Replace NA with 0 or any other value if needed
df[is.na(df)] <- 0

#df_t <- t(df[,-1])
#colnames(df_t) <- df[,1]

write.csv(df,"./data/env_dat.csv", row.names = FALSE)
