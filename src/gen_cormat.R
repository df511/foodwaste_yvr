###### Generate correlation matrices
#### written by Daniel Forrest
#### March 5, 2025


library(here)
library(readr)
library(dplyr)


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


load(file = here("data", "dat_scaled_750.Rdata"))

#check that classes are all numeric for predictor variables
class(dat_grid_final)
for (val in 1:82) {
  print(val)
  print(class(dat_grid_final[[val]]))
}

# my_data <- dat_scaled
# my_data <- as.data.frame(my_data)
# my_data <- my_data[, -c(1,2,5,48)]
# 
# my_data <- na.omit(my_data)





my_data <- dat_scaled %>% select(where(is.numeric))
my_data <- as.data.frame(my_data)

my_data <- my_data[, colSums(is.na(my_data)) == 0]  # Remove NA columns


#cormat <- cor(my_data)
cormat <- cor(my_data[, -c(1,4,8,11,14,15,20, 21,23,24, 30:32, 79)])

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
                   "female_pct", "SCOREMAT", "SCORESOC", "Paved", "NaturalVegSum", "BuiltSum", "GrassSoilSum")  # Replace with actual column names

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

# Generate heatmap with R² labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) +  # Add R² values
  scale_fill_gradient(low = "white", high = "darkcyan", limits = c(0, 1), name = "R² Value") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

print(heatmap)
