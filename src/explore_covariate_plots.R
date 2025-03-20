library(ggplot2)
library(ggpubr)
library(broom)  # For extracting regression stats


dat_clean <- na.omit(dat_grid_final)



# remove_outliers <- function(df, column) {
#   Q1 <- quantile(df[[column]], 0.25, na.rm = TRUE)
#   Q3 <- quantile(df[[column]], 0.75, na.rm = TRUE)
#   IQR_value <- Q3 - Q1
#   
#   df %>%
#     filter(df[[column]] >= (Q1 - 1.5 * IQR_value) & df[[column]] <= (Q3 + 1.5 * IQR_value))
# }
# 
# 
# dat_clean <- remove_outliers(dat_clean, "mean_land_value")
# dat_clean <- remove_outliers(dat_clean, "mean_land_imp_value")


dat_clean <- dat_clean[!dat_clean$fw_score == 0,]

dat_scaled <- dat_clean %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))  # Centers and scales by 1 SD


# Define variable name
var <- "chinese_pct"


# Dynamically reference the variable
#model <- lm(as.formula(paste("fw_score ~", var)), data = dat_scaled)
model <- lm(as.formula(paste("fw_score ~",var)), data = dat_scaled)


# Extract equation components dynamically
eq <- as.expression(
  substitute(italic(y) == a + b %.% italic(x) * "," ~~ R^2 ~ "=" ~ r2 * "," ~~ p ~ "=" ~ pval, 
             list(a = round(coef(model)[1], 6), 
                  b = round(coef(model)[2], 6), 
                  r2 = round(summary(model)$r.squared, 3), 
                  pval = signif(summary(model)$coefficients[2, 4], 3)))
)

# Plot with dynamically updated labels
ggplot(dat_scaled, aes_string(x = var, y = "fw_score")) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +  
  annotate("text", 
           x = max(dat_scaled[[var]], na.rm = TRUE) * 0.4, 
           y = max(dat_scaled$fw_score, na.rm = TRUE) * 0.4, 
           label = eq, hjust = 0, parse = TRUE)+ 
  theme_minimal() +
  labs(title = paste(var, "vs. FW Score"),
       y = "FW Score",
       x = var)




