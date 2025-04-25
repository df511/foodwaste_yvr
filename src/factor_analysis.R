## Factor Analysis of Housing Vars

library(ggplot2)
library(reshape2)
library(ggrepel)
library(gridExtra)

# Load your data (replace "path_to_your_data.csv" with the actual path to your CSV file)
load(file = here("data", "dat_scaled_750.Rdata"))

my_data <- dat_scaled %>% select(where(is.numeric))
my_data <- as.data.frame(my_data)
my_data <- my_data[, colSums(is.na(my_data)) == 0]  # Remove NA columns


# # Define your variable list
# selected_vars <- c(
#   "rent_pct", "single_detached", "nearest_shelter_dist",
#   "female_pct", "SCOREMAT", "SCORESOC", "pop_km",
#   "housing_highdensity", "housing_lowdensity", # "household_income", (removed household income because of overlap with SCOREMAT)
#   "pct_rent_thirty", "pct_housing_notsuitable", "pct_rent_subsidized", 
#   "major_repairs", "minor_repairs", "total_occupied_housing"
# )

# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct","nearest_shelter_dist","indig_pct", 
                   "female_pct", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct", "one_parent_pct", "avg_household_size","housing_highdensity", "housing_lowdensity",
                   "pct_major_repairs", "pct_rent_thirty", "pct_rent_subsidized", "pct_unsuitable_housing", 
                   "latinamerican_pct","chinese_pct", "korean_pct", "japanese_pct", "southeastasian_pct", "southasian_pct", "arab_pct", "notvisminority_pct", "black_pct","filipino_pct")  # Replace with actual column names


# Define the variables to keep in the heatmap
selected_vars <- c("rent_pct","nearest_shelter_dist", "pop_km","household_income","employed_pct" ,"no_diploma_pct","separated_divorced_widowed_pct",
                   "female_pct","one_parent_pct", "avg_household_size","housing_highdensity", "housing_lowdensity",
                   "pct_major_repairs", "pct_rent_thirty", "pct_rent_subsidized", "pct_unsuitable_housing")




# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data)]

# Subset data to only include selected variables
my_data_subset <- my_data[, selected_vars]

# Ensure no missing values
my_data_subset <- na.omit(my_data_subset)

# Compute eigenvalues of the correlation matrix
eigen_vals <- eigen(cor(my_data_subset))$values

# Scree plot
plot(eigen_vals, type = "b", pch = 19,
     main = "Scree Plot",
     xlab = "Factor Number", ylab = "Eigenvalue")
abline(h = 1, col = "red", lty = 2)  # Kaiser criterion line


# Set number of factors to extract
num_factors <- 10

# Perform factor analysis
fa_result <- factanal(my_data_subset, factors = num_factors, rotation = "varimax", scores = "regression")

fa_scores <-as.data.frame(fa_result$scores)

# View summary of the result
print(fa_result)

# Extract factor loadings
loadings_df <- data.frame(fa_result$loadings[, 1:num_factors])
loadings_df$Variable <- rownames(loadings_df)
loadings_melted <- melt(loadings_df, id.vars = "Variable", variable.name = "Factor", value.name = "Loading")

# Plot factor loadings
fa_loadings_plot <- ggplot(loadings_melted, aes(x = Variable, y = Loading, fill = Factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Factor Loadings", x = "Variable", y = "Loading") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fa_loadings_plot)



# Convert to data frame for plotting
loadings_df <- as.data.frame(unclass(fa_result$loadings))
loadings_df$Variable <- rownames(loadings_df)

# Reshape to long format manually
loadings_long <- reshape(loadings_df, 
                         varying = names(loadings_df)[1:num_factors], 
                         v.names = "Loading",
                         timevar = "Factor",
                         times = paste0("Factor", 1:num_factors),
                         direction = "long")

# Plot (using base R barplot)
par(mar = c(10, 4, 4, 2))  # increase bottom margin
with(loadings_long, {
  barplot(Loading, names.arg = Variable, las = 2,
          main = "Factor Loadings", col = as.factor(Factor))
})

# Extract and scale loadings
loadings <- fa_result$loadings[, 1:5]


# Convert to data frame
loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df)

# Optional: scale loadings for visibility
scale_factor <- 1.5
loadings_df$Factor1 <- loadings_df[,1] * scale_factor
loadings_df$Factor2 <- loadings_df[,2] * scale_factor
loadings_df$Factor3 <- loadings_df[,3] * scale_factor
loadings_df$Factor4 <- loadings_df[,4] * scale_factor
loadings_df$Factor5 <- loadings_df[,5] * scale_factor

# Plot
ggplot(loadings_df, aes(x = 0, y = 0, xend = Factor1, yend = Factor2)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(aes(x = Factor1, y = Factor2, label = Variable), size = 4) +
  coord_equal() +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Factor Analysis Loadings Biplot",
       x = "Factor 1", y = "Factor 2") +
  theme_minimal()



# Plot
ggplot(loadings_df, aes(x = 0, y = 0, xend = Factor1, yend = Factor3)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(aes(x = Factor1, y = Factor3, label = Variable), size = 4) +
  coord_equal() +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Factor Analysis Loadings Biplot",
       x = "Factor 1", y = "Factor 3") +
  theme_minimal()



# Plot
ggplot(loadings_df, aes(x = 0, y = 0, xend = Factor1, yend = Factor4)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(aes(x = Factor1, y = Factor4, label = Variable), size = 4) +
  coord_equal() +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Factor Analysis Loadings Biplot",
       x = "Factor 1", y = "Factor 4") +
  theme_minimal()


# Plot
ggplot(loadings_df, aes(x = 0, y = 0, xend = Factor1, yend = Factor5)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(aes(x = Factor1, y = Factor5, label = Variable), size = 4) +
  coord_equal() +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Factor Analysis Loadings Biplot",
       x = "Factor 1", y = "Factor 5") +
  theme_minimal()




dat_combined <- cbind(dat_scaled, fa_scores)



#####################################################

# Define your variable list
selected_vars <- c(
  "rent_pct", "single_detached", "nearest_shelter_dist", "indig_pct", 
  "female_pct", "SCOREMAT", "SCORESOC", "pop_km",
  "household_income", "housing_highdensity", "housing_lowdensity",
  "pct_rent_thirty", "pct_housing_notsuitable", "pct_rent_subsidized", 
  "major_repairs", "minor_repairs", "immigrant_pct", "citizen_pct", "total_occupied_housing", 
  "min_pct", "black_pct", "chinese_pct", "english_pct"
)





# Ensure the selected variables exist in the dataset
selected_vars <- selected_vars[selected_vars %in% colnames(my_data)]

# Subset data to only include selected variables
my_data_subset <- my_data[, selected_vars]

# Ensure no missing values
my_data_subset <- na.omit(my_data_subset)

# Compute eigenvalues of the correlation matrix
eigen_vals <- eigen(cor(my_data_subset))$values

# Scree plot
plot(eigen_vals, type = "b", pch = 19,
     main = "Scree Plot",
     xlab = "Factor Number", ylab = "Eigenvalue")
abline(h = 1, col = "red", lty = 2)  # Kaiser criterion line


# Set number of factors to extract
num_factors <- 8

# Perform factor analysis
fa_result <- factanal(my_data_subset, factors = num_factors, rotation = "varimax", scores = "regression")

# View summary of the result
print(fa_result)

# Extract factor loadings
loadings_df <- data.frame(fa_result$loadings[, 1:num_factors])
loadings_df$Variable <- rownames(loadings_df)
loadings_melted <- melt(loadings_df, id.vars = "Variable", variable.name = "Factor", value.name = "Loading")

# Plot factor loadings
fa_loadings_plot <- ggplot(loadings_melted, aes(x = Variable, y = Loading, fill = Factor)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Factor Loadings", x = "Variable", y = "Loading") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fa_loadings_plot)



# Convert to data frame for plotting
loadings_df <- as.data.frame(unclass(fa_result$loadings))
loadings_df$Variable <- rownames(loadings_df)

# Reshape to long format manually
loadings_long <- reshape(loadings_df, 
                         varying = names(loadings_df)[1:num_factors], 
                         v.names = "Loading",
                         timevar = "Factor",
                         times = paste0("Factor", 1:num_factors),
                         direction = "long")

# Plot (using base R barplot)
par(mar = c(10, 4, 4, 2))  # increase bottom margin
with(loadings_long, {
  barplot(Loading, names.arg = Variable, las = 2,
          main = "Factor Loadings", col = as.factor(Factor))
})

# Extract and scale loadings
loadings <- fa_result$loadings[, 1:2]


# Convert to data frame
loadings_df <- as.data.frame(loadings)
loadings_df$Variable <- rownames(loadings_df)

# Optional: scale loadings for visibility
scale_factor <- 1.5
loadings_df$Factor1 <- loadings_df[,1] * scale_factor
loadings_df$Factor2 <- loadings_df[,2] * scale_factor

# Plot
ggplot(loadings_df, aes(x = 0, y = 0, xend = Factor1, yend = Factor2)) +
  geom_segment(arrow = arrow(length = unit(0.2, "cm")), color = "red") +
  geom_text_repel(aes(x = Factor1, y = Factor2, label = Variable), size = 4) +
  coord_equal() +
  xlim(-1.5, 1.5) + ylim(-1.5, 1.5) +
  labs(title = "Factor Analysis Loadings Biplot",
       x = "Factor 1", y = "Factor 2") +
  theme_minimal()

