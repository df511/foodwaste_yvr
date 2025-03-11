###### Generate correlation matrices
#### written by Daniel Forrest
#### March 5, 2025


library(here)
library(readr)


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
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}


#### cor matrix 750. m #####


load(file = here("data", "dat_fw_demo_lc_retail_shelters_750.Rdata"))

#check that classes are all numeric for predictor variables
class(dat_grid_final)
for (val in 1:50) {
  print(val)
  print(class(dat_grid_final[[val]]))
}

my_data <- dat_grid_final
my_data <- as.data.frame(my_data)
my_data <- my_data[, -c(1,5,48)]

my_data <- na.omit(my_data)

# 
# for (val in 1:47) {
#   print(val)
#   print(class(my_data[,val]))
# }
# 
# 
cormat <- cor(my_data)


cormat <- reorder_cormat(cormat)
lower_tri <- get_lower_tri(cormat)
melted_cormat <- melt(lower_tri)


# Filter correlations greater than 0.5 (absolute value)
melted_cormat <- melted_cormat[abs(melted_cormat$value) > 0.5, ]

heatmap <- ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkcyan", high = "darkorange3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + theme_minimal()+theme(axis.text.x = element_text(angle = 60,hjust=0.95,vjust=0.95))

heatmap




# Abbreviate variable names
melted_cormat$Var1 <- abbreviate(melted_cormat$Var1, minlength = 10)
melted_cormat$Var2 <- abbreviate(melted_cormat$Var2, minlength = 10)

# Create heatmap with abbreviated labels
heatmap <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkcyan", high = "darkorange3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name = "Pearson\nCorrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 60, hjust = 0.95, vjust = 0.95))

heatmap

