library(ggplot2)

ggplot(dat_grid_final, aes(x = fw_score, y = household_income)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "FW Score vs. Household Income",
       x = "FW Score",
       y = "Household Income")


ggplot(dat_grid_final, aes(x = indig_percent, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Household Income vs. FW Score",
       y = "FW Score",
       x = "Household Income")

ggplot(dat_grid_final, aes(x = pop_km, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Pop Dens. vs. FW Score",
       y = "FW Score",
       x = "population density")

ggplot(dat_grid_final, aes(x = Paved, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Paved. vs. FW Score",
       y = "FW Score",
       x = "Paved Cover")

ggplot(dat_grid_final, aes(x = Conifer, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Paved. vs. FW Score",
       y = "FW Score",
       x = "Coniferous")




ggplot(dat_grid_final, aes(x = Buildings, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Buildings vs. FW Score",
       y = "FW Score",
       x = "Buildings")


ggplot(dat_grid_final, aes(x = minority_pop, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Vis. Minorities vs. FW Score",
       y = "FW Score",
       x = "Visible Minorities")

ggplot(dat_grid_final, aes(x = Food_Retail, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Vis. Minorities vs. FW Score",
       y = "FW Score",
       x = "food retail")


ggplot(dat_grid_final, aes(x = nearest_shelter_dist, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  theme_minimal() +
  labs(title = "Nearest Homeless Shelter vs. FW Score",
       y = "FW Score",
       x = "Distance to Nearest Homeless Shelter")


library(ggplot2)
library(ggpubr)

ggplot(dat_grid_final, aes(x = nearest_shelter_dist, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = FALSE) +  
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")), 
    formula = y ~ x,
    label.x = max(dat_grid_final$nearest_shelter_dist) * 0.7,  # Adjust X position (Right)
    label.y = max(dat_grid_final$fw_score) * 0.9  # Adjust Y position (Top)
  ) +  
  theme_minimal() +
  labs(title = "Nearest Homeless Shelter vs. FW Score",
       y = "FW Score",
       x = "Distance to Nearest Homeless Shelter")


library(ggplot2)
library(ggpubr)
library(broom)  # For extracting regression stats

# Fit linear model
model <- lm(fw_score ~ nearest_shelter_dist, data = dat_grid_final)

# Extract equation components
eq <- as.character(as.expression(
  substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2 * "," ~~ p ~ "=" ~ pval, 
             list(a = round(coef(model)[1], 6), 
                  b = round(coef(model)[2], 6), 
                  r2 = round(summary(model)$r.squared, 3), 
                  pval = signif(summary(model)$coefficients[2, 4], 3)))
))

# Plot with manually added equation
ggplot(dat_grid_final, aes(x = nearest_shelter_dist, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  
  annotate("text", x = max(dat_grid_final$nearest_shelter_dist) * 0.7, 
           y = max(dat_grid_final$fw_score) * 0.9, 
           label = eq, parse = TRUE, hjust = 0) +  
  theme_minimal() +
  labs(title = "Nearest Homeless Shelter vs. FW Score",
       y = "FW Score",
       x = "Distance to Nearest Homeless Shelter")



# Fit linear model
model <- lm(fw_score ~ nearest_food_retail,  data = dat_grid_final)

# Extract equation components
eq <- as.character(as.expression(
  substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2 * "," ~~ p ~ "=" ~ pval, 
             list(a = round(coef(model)[1], 6), 
                  b = round(coef(model)[2], 6), 
                  r2 = round(summary(model)$r.squared, 3), 
                  pval = signif(summary(model)$coefficients[2, 4], 3)))
))

# Plot with manually added equation
ggplot(dat_grid_final, aes(x = nearest_food_retail, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  
  annotate("text", x = max(dat_grid_final$nearest_food_retail) * 0.7, 
           y = max(dat_grid_final$fw_score) * 0.9, 
           label = eq, parse = TRUE, hjust = 0) +  
  theme_minimal() +
  labs(title = "Nearest Food Retailer vs. FW Score",
       y = "FW Score",
       x = "Distance to Nearest Food Retailer")



# Fit linear model
model <- lm(fw_score ~ SCOREMAT,  data = dat_grid_final)

# Extract equation components
eq <- as.character(as.expression(
  substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2 * "," ~~ p ~ "=" ~ pval, 
             list(a = round(coef(model)[1], 6), 
                  b = round(coef(model)[2], 6), 
                  r2 = round(summary(model)$r.squared, 3), 
                  pval = signif(summary(model)$coefficients[2, 4], 3)))
))

# Plot with manually added equation
ggplot(dat_grid_final, aes(x = SCOREMAT, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  
  annotate("text", x = max(dat_grid_final$SCOREMAT) * 0.7, 
           y = max(dat_grid_final$fw_score) * 0.9, 
           label = eq, parse = TRUE, hjust = 0) +  
  theme_minimal() +
  labs(title = "Material Deprivation vs. FW Score",
       y = "FW Score",
       x = "Material Deprivation")



# Fit linear model
model <- lm(fw_score ~ SCORESOC,  data = dat_grid_final)

# Extract equation components
eq <- as.character(as.expression(
  substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2 * "," ~~ p ~ "=" ~ pval, 
             list(a = round(coef(model)[1], 6), 
                  b = round(coef(model)[2], 6), 
                  r2 = round(summary(model)$r.squared, 3), 
                  pval = signif(summary(model)$coefficients[2, 4], 3)))
))

# Plot with manually added equation
ggplot(dat_grid_final, aes(x = SCORESOC, y = fw_score)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  
  annotate("text", x = max(dat_grid_final$SCORESOC) * 0.7, 
           y = max(dat_grid_final$fw_score) * 0.9, 
           label = eq, parse = TRUE, hjust = 0) +  
  theme_minimal() +
  labs(title = "Social Deprivation vs. FW Score",
       y = "FW Score",
       x = "Social Deprivation")

