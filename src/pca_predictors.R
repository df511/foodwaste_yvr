## predictor PCA


# Load necessary libraries
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(ggrepel)

# Load your data (replace "path_to_your_data.csv" with the actual path to your CSV file)
load(file = here("data", "dat_scaled_750.Rdata"))

my_data <- dat_scaled %>% select(where(is.numeric))
my_data <- as.data.frame(my_data)
my_data <- my_data[, colSums(is.na(my_data)) == 0]  # Remove NA columns



# Perform PCA
pca_result <- prcomp(my_data[,-c(1,84,87)]) ### remove first and last columns

# Summarize the PCA result
summary(pca_result)


# Extract proportion of variance explained
variance_explained <- summary(pca_result)$importance[2, ]

# Cumulative variance explained
cumulative_variance_explained <- cumsum(variance_explained)

# Create a data frame for plotting
variance_df <- data.frame(
  PC = 1:length(variance_explained),
  Variance_Explained = variance_explained,
  Cumulative_Variance_Explained = cumulative_variance_explained
)

# Plot scree plot and cumulative variance explained
scree_plot <- ggplot(variance_df, aes(x = PC, y = Variance_Explained)) +
  geom_bar(stat = "identity") +
  geom_line(aes(y = Cumulative_Variance_Explained), color = "red", size = 1) +
  geom_point(aes(y = Cumulative_Variance_Explained), color = "red", size = 2) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained") +
  theme_minimal()

cumulative_variance_plot <- ggplot(variance_df, aes(x = PC, y = Cumulative_Variance_Explained)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Cumulative Variance Explained", x = "Principal Component", y = "Cumulative Variance Explained") +
  theme_minimal()

# Arrange plots
grid.arrange(scree_plot, cumulative_variance_plot, ncol = 2)

# Print the cumulative variance explained
print(cumulative_variance_explained)


print(scree_plot)


# Extract loadings
loadings <- pca_result$rotation

# Print the loadings
print(loadings)

# Convert loadings to a data frame for easier plotting
loadings_df <- as.data.frame(loadings)

# Add a column for variable names
loadings_df$Variable <- rownames(loadings_df)

# Melt the data frame for ggplot
loadings_melted <- reshape2::melt(loadings_df, id.vars = "Variable")

# Plot the loadings
ggplot(loadings_melted, aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loadings of Variables on Principal Components", x = "Variables", y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Plot the loadings
pcavector_plot <- ggplot(loadings_melted[1:24,], aes(x = Variable, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Loadings of Variables on Principal Components", x = "Variables", y = "Loadings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Specify the file path where you want to save the PDF
#pdf_file <- here("figures", "pca123_vectors.pdf")

# Open a PDF graphics device
#pdf(pdf_file)

# Print the combined plot
 print(pcavector_plot)
# 
# # Close the PDF graphics device
# dev.off()


# Create a data frame for the loadings
loadings_df <- data.frame(
  Variable = rownames(loadings),
  PC1 = loadings[, 1],
  PC2 = loadings[, 2]
)

# # Extract scores (principal components) for the first two PCs
# scores <- pca_result$x[, 1:2]
# scores_df <- data.frame(scores)

# Plot the loadings as vectors
pca1 <- ggplot() +
  #geom_point(data = scores_df, aes(x = PC1, y = PC2), alpha = 0.5) +  # Plot scores
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +  # Plot loadings
  geom_text_repel(data = loadings_df, aes(x = PC1, y = PC2, label = Variable), 
                  size = 4, color = "black") +  # Label the vectors
  labs(title = "PCA Biplot",
       x = "PC1",
       y = "PC2") +
  theme_minimal()


# Specify the file path where you want to save the PDF
pdf_file <- here("figs", "pca_12.pdf")

# Open a PDF graphics device
pdf(pdf_file)

# Print the combined plot
 print(pca1)

# Close the PDF graphics device
dev.off()


# Create a data frame for the loadings
loadings_df <- data.frame(
  Variable = rownames(loadings),
  PC1 = loadings[, 1],
  PC3 = loadings[, 3]
)


# Plot the loadings as vectors
pca2 <- ggplot() +
  #geom_point(data = scores_df, aes(x = PC1, y = PC2), alpha = 0.5) +  # Plot scores
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC1, yend = PC3), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +  # Plot loadings
  geom_text_repel(data = loadings_df, aes(x = PC1, y = PC3, label = Variable), 
                  size = 4, color = "black") +  # Label the vectors
  labs(title = "PCA Biplot",
       x = "PC1",
       y = "PC3") +
  theme_minimal()

# Specify the file path where you want to save the PDF
pdf_file <- here("figures", "pca_13.pdf")

# Open a PDF graphics device
pdf(pdf_file)

# Print the combined plot
print(pca2)

# Close the PDF graphics device
dev.off()



##########################3
# Create a data frame for the loadings
loadings_df <- data.frame(
  Variable = rownames(loadings),
  PC3 = loadings[, 3],
  PC4 = loadings[, 4]
)


# Plot the loadings as vectors
ggplot() +
  #geom_point(data = scores_df, aes(x = PC1, y = PC2), alpha = 0.5) +  # Plot scores
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC3, yend = PC4), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +  # Plot loadings
  geom_text_repel(data = loadings_df, aes(x = PC3, y = PC4, label = Variable), 
                  size = 4, color = "red") +  # Label the vectors
  labs(title = "PCA Biplot",
       x = "PC3",
       y = "PC4") +
  theme_minimal()







# Create a data frame for the loadings
loadings_df <- data.frame(
  Variable = rownames(loadings),
  PC5 = loadings[, 5],
  PC6 = loadings[, 6]
)


# Plot the loadings as vectors
ggplot() +
  #geom_point(data = scores_df, aes(x = PC1, y = PC2), alpha = 0.5) +  # Plot scores
  geom_segment(data = loadings_df, aes(x = 0, y = 0, xend = PC5, yend = PC6), 
               arrow = arrow(length = unit(0.2, "cm")), color = "red") +  # Plot loadings
  geom_text_repel(data = loadings_df, aes(x = PC5, y = PC6, label = Variable), 
                  size = 4, color = "red") +  # Label the vectors
  labs(title = "PCA Biplot",
       x = "PC5",
       y = "PC6") +
  theme_minimal()



# Get the loadings
loadings <- pca_result$rotation


df_mat <- as.matrix(df[,-c(1)])
loadings_mat <- as.matrix(loadings)


# Transform the original standardized data using the loadings
transformed_data <- as.data.frame(df_mat %*% loadings_mat)

transformed_data <- cbind(df[,1], transformed_data)

transformed_data[,-c(1)] <- scale(transformed_data[,-c(1)])

#write.csv(transformed_data,"./data/pca_env_dat.csv", row.names = FALSE)



