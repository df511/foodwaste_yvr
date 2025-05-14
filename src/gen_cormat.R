###### Generate correlation matrices
#### written by Daniel Forrest
#### March 5, 2025


library(here)
library(readr)
library(dplyr)
library(reshape2)
library(RColorBrewer)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  return(cormat[hc$order, hc$order])  # Return the reordered matrix
}


#### cor matrix 750. m #####


load(file = here("data", "dat_no_ub_scaled_750.Rdata"))

dat_no_ub_scaled_no0s <- dat_no_ub_scaled %>%
  filter(fw_score_weighted > -0.1632483) ### 0 equivalent val (minimum fw_score_weighted val after scaling, not to be used as response)
# 
# #check that classes are all numeric for predictor variables
# class(dat_grid_final)
# for (val in 1:82) {
#   print(val)
#   print(class(dat_grid_final[[val]]))
# }

# my_data <- dat_scaled
# my_data <- as.data.frame(my_data)
# my_data <- my_data[, -c(1,2,5,48)]
# 
# my_data <- na.omit(my_data)





my_data <- dat_no_ub_scaled_no0s %>% select(where(is.numeric))
my_data <- as.data.frame(my_data)

my_data <- my_data[, colSums(is.na(my_data)) == 0]  # Remove NA columns


# #cormat <- cor(my_data)
# cormat <- cor(my_data[, -c(1,4,8,11,14,15,20, 21,23,24, 30:32, 91)])
# 
# #cormat <- cormat[-37,-37] ### remove NA column one of the repairs needed, FIX THIS
# 
# # for (val in 1:79) {
# #   print(val)
# #   print(class(my_data[,val]))
# # }

# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct","nearest_shelter_dist","nearest_food_retail", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct",
                   "female_pct","one_parent_pct", "avg_household_size","housing_highdensity",
                   "pct_major_repairs", "pct_rent_thirty", "pct_rent_subsidized", "pct_unsuitable_housing")

# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data)]

# Subset data to only include selected variables
my_data_subset <- my_data[, selected_vars]
cormat <- cor(my_data_subset)

cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)
melted_cormat <- melt(lower_tri)



# # Filter correlations greater than 0.5 (absolute value)
# melted_cormat <- melted_cormat[abs(melted_cormat$value) > 0.5, ]
# melted_cormat <- melted_cormat[complete.cases(melted_cormat), ]

magma_colors <- magma(10)

heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = magma_colors[7],     # Dark orange (negative correlations)
    mid = "white",              # Neutral zero
    high = magma_colors[3],     # Deep purple (positive correlations)
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson R"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_blank()
  )

heatmap

print(heatmap)





# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data)]

# Subset data to only include selected variables
my_data_subset <- my_data[, selected_vars]
cormat2 <- cor(my_data_subset)
# 
# # Convert correlations to R-squared values
# rsq_mat <- cormat2^2
# 
# # Reorder correlation matrix
# rsq_mat <- reorder_cormat(rsq_mat)

# Get lower triangle of the correlation matrix
lower_tri <- get_lower_tri(cormat2)

# Melt the correlation matrix for visualization
melted_cormat <- melt(lower_tri, na.rm = TRUE)

# Generate heatmap with R² labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add R² values
  scale_fill_gradient(low = "white", high = "darkcyan", limits = c(0, 1), name = "R² Value") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

print(heatmap)



# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct","nearest_shelter_dist","nearest_food_retail", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct",
                   "female_pct","one_parent_pct", "avg_household_size","housing_highdensity",
                   "pct_major_repairs", "pct_rent_thirty", "pct_rent_subsidized", "pct_unsuitable_housing")

# 
# 
# # Define the variables to keep in the heatmap
# selected_vars <- c("rent_pct","nearest_shelter_dist","indig_pct", 
#                    "female_pct", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct", "one_parent_pct", "avg_household_size","housing_highdensity", "housing_lowdensity",
#                    "pct_major_repairs", "pct_rent_thirty","housing_suitability", "pct_rent_subsidized", "pct_unsuitable_housing", 
#                    "latinamerican_pct","chinese_pct", "indig_pct", "korean_pct", "japanese_pct", "southeastasian_pct", "southasian_pct", "arab_pct", "notvisminority_pct", "black_pct","filipino_pct")  # Replace with actual column names
# 





# # Define the variables to keep in the heatmap
# selected_vars <- c("rent_pct", "single_detached" ,
#                    "nearest_food_retail","nearest_shelter_dist","indig_pct", 
#                    "female_pct", "SCOREMAT", "SCORESOC", "pop_km",
#                    "household_income", "housing_highdensity", "housing_lowdensity",
#                    "thirtyonshelter_majorrepairs", "notsuitable_majorrepairs", "pct_rent_subsidized", "pct_housing_notsuitable","pct_rent_thiry", "housing_low_density", "housing_high_density", "poorservice_housing")  # Replace with actual column names
# 

# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data)]

# Subset data to only include selected variables
my_data_subset <- my_data[, selected_vars]
cormat2 <- cor(my_data_subset)

# Convert correlations to R-squared values
rsq_mat <- cormat2^2

# Reorder correlation matrix
rsq_mat <- reorder_cormat(rsq_mat)

# Get lower triangle of the correlation matrix
lower_tri <- get_lower_tri(rsq_mat)

# Melt the correlation matrix for visualization
melted_cormat <- melt(lower_tri, na.rm = TRUE)


heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkcyan", high = "darkorange3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + theme_minimal()+theme(axis.text.x = element_text(angle = 60,hjust=0.95,vjust=0.95))

heatmap


# Generate heatmap with R² labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add R² values
  scale_fill_gradient(low = "white", high = "darkcyan", limits = c(0, 1), name = "R² Value") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

print(heatmap)







########### only positive fw_score data!



my_data2 <- dat_scaled_no0s %>% select(where(is.numeric))
my_data2 <- as.data.frame(my_data2)
my_data2 <- my_data2[, colSums(is.na(my_data2)) == 0]  # Remove NA columns


#cormat <- cor(my_data)
cormat <- cor(my_data2[, -c(1,4,8,11,14,15,20, 21,23,24, 30:32, 91)])

#cormat <- cormat[-37,-37] ### remove NA column one of the repairs needed, FIX THIS

# for (val in 1:79) {
#   print(val)
#   print(class(my_data[,val]))
# }


cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)
melted_cormat <- melt(lower_tri)



# Filter correlations greater than 0.5 (absolute value)
melted_cormat <- melted_cormat[abs(melted_cormat$value) > 0.5, ]
melted_cormat <- melted_cormat[complete.cases(melted_cormat), ]

heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkcyan", high = "darkorange3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + theme_minimal()+theme(axis.text.x = element_text(angle = 60,hjust=0.95,vjust=0.95))

heatmap







# Define the variables to keep in the heatmap
selected_vars <- c("fw_score_weighted", "rent_pct", "single_detached" ,"noncitizen_pct", 
                   "nearest_food_retail","nearest_shelter_dist","indig_pct", 
                   "female_pct", "SCOREMAT", "SCORESOC", "Paved", "NaturalVegSum", "BuiltSum", "GrassSoilSum", "pop_km",
                   "household_income", "housing_highdensity", "housing_lowdensity",
                   "thirtyonshelter_majorrepairs", "notsuitable_majorrepairs")  # Replace with actual column names

# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data2)]

# Subset data to only include selected variables
my_data_subset <- my_data2[, selected_vars]
cormat2 <- cor(my_data_subset)

# Convert correlations to R-squared values
rsq_mat <- cormat2^2

# Reorder correlation matrix
rsq_mat <- reorder_cormat(rsq_mat)

# Get lower triangle of the correlation matrix
lower_tri <- get_lower_tri(rsq_mat)

# Melt the correlation matrix for visualization
melted_cormat <- melt(lower_tri, na.rm = TRUE)

# Generate heatmap with R² labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add R² values
  scale_fill_gradient(low = "white", high = "darkcyan", limits = c(0, 1), name = "R² Value") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

print(heatmap)




# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct", "single_detached" ,
                   "nearest_food_retail","nearest_shelter_dist","indig_pct", 
                   "female_pct", "SCOREMAT", "SCORESOC", "pop_km",
                   "household_income", "housing_highdensity", "housing_lowdensity",
                   "thirtyonshelter_majorrepairs", "notsuitable_majorrepairs", "pct_rent_subsidized", "pct_housing_notsuitable","pct_rent_thiry", "housing_low_density", "housing_high_density", "poorservice_housing")  # Replace with actual column names


# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data2)]

# Subset data to only include selected variables
my_data_subset <- my_data2[, selected_vars]
cormat2 <- cor(my_data_subset)

# Convert correlations to R-squared values
rsq_mat <- cormat2^2

# Reorder correlation matrix
rsq_mat <- reorder_cormat(rsq_mat)

# Get lower triangle of the correlation matrix
lower_tri <- get_lower_tri(rsq_mat)

# Melt the correlation matrix for visualization
melted_cormat <- melt(lower_tri, na.rm = TRUE)

# Generate heatmap with R² labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add R² values
  scale_fill_gradient(low = "white", high = "darkcyan", limits = c(0, 1), name = "R² Value") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

print(heatmap)




# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct","nearest_shelter_dist","indig_pct", 
                   "female_pct", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct", "one_parent_pct", "avg_household_size","housing_highdensity", "housing_lowdensity",
                   "pct_major_repairs", "pct_rent_thirty", "pct_rent_subsidized", "pct_unsuitable_housing", 
                   "latinamerican_pct","chinese_pct", "korean_pct", "japanese_pct", "southeastasian_pct", "southasian_pct", "arab_pct", "notvisminority_pct", "black_pct","filipino_pct")  # Replace with actual column names

# 
# 
# # Define your variable list
# selected_vars <- c(
#   "rent_pct", "single_detached",
#   "nearest_food_retail", "nearest_shelter_dist", "indig_pct", 
#   "female_pct", "SCOREMAT", "SCORESOC", "pop_km",
#   "household_income", "housing_highdensity", "housing_lowdensity",
#   "thirtyonshelter_majorrepairs", "notsuitable_majorrepairs", "pct_rent_subsidized", 
#   "pct_housing_notsuitable", "pct_rent_thiry", 
#   "housing_low_density", "housing_high_density", "poorservice_housing", "service_road_weight"
# )

# Filter to existing columns
selected_vars <- selected_vars[selected_vars %in% colnames(my_data2)]
my_data_subset <- my_data2[, selected_vars]

# Compute correlation matrix
cormat <- cor(my_data_subset, use = "complete.obs")

# Optional: reorder correlation matrix (if you have a reorder_cormat function)
cormat <- reorder_cormat(cormat)

# Lower triangle only
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

lower_tri <- get_lower_tri(cormat)

# Melt for ggplot
melted_cormat <- melt(lower_tri, na.rm = TRUE)
# Sample from magma palette for low and high
magma_colors <- magma(10)

heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  scale_fill_gradient2(
    low = magma_colors[7],     # Dark orange (negative correlations)
    mid = "white",              # Neutral zero
    high = magma_colors[3],     # Deep purple (positive correlations)
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson R"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title = element_blank()
  )

print(heatmap)