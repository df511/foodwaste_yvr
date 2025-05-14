library(gsl)
### Load posterior samples from Stan
load(file = here("data", "fit_ZIW_A_FA1_FA3_B_roadweight_natveg_noub.Rdata"))
posterior <- rstan::extract(fit_ZIW_A_FA1_FA3_B_roadweight_natveg_noub)

# Compute medians from posterior samples
mu_med     <- median(posterior$mu)
beta1_med  <- median(posterior$beta1)
beta2_med  <- median(posterior$beta2)
gamma_med  <- median(posterior$gamma)
delta1_med <- median(posterior$delta1)
delta2_med <- median(posterior$delta2)

### Load the spatial data
load(file = here("data", "van_dat_final_750.Rdata"))

# Covariates from your spatial data
X1 <- van_dat$Factor1
X3 <- van_dat$Factor3
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Compute sigma (scale parameter of Weibull)
log_scale <- mu_med + beta1_med * X1 + beta2_med * X3
sigma <- exp(log_scale)

# Weibull shape parameter (from posterior)
alpha_med <- median(posterior$alpha)

# Expected value of non-zero Weibull
weibull_mean <- sigma * gamma(1 + 1 / alpha_med)

# Zero-inflation probability
pi <- plogis(gamma_med + delta1_med * P1 + delta2_med * P2)

# Final expected value accounting for zero-inflation
expected_y <- (1 - pi) * weibull_mean

# Spatial object with prediction
predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),
  expected_fw_score = expected_y
)

# Plot
pdf(here("figs", "expected_fw_score_weibull_FA1FA3.pdf"), width = 12, height = 8)

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = expected_fw_score), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Expected FW Score") +
  theme_minimal() +
  theme(panel.grid = element_blank())

dev.off()



# Plot
pdf(here("figs", "expected_fw_score_weibull_FA1FA3_lims_0_20.pdf"), width = 12, height = 8)

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = expected_fw_score), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Expected FW Score", limits = c(0,20)) +
  theme_minimal() +
  theme(panel.grid = element_blank())

dev.off()

library(matrixStats)


#library(tidyverse)
library(sf)
library(here)
library(ggplot2)

### Load posterior samples from Stan
load(file = here("data", "fit_A_FA1_FA3_B_roadweight_natveg_noub_ulim.Rdata"))
posterior <- rstan::extract(fit_A_FA1_FA3_B_roadweight_natveg_noub)

# Compute medians from posterior samples
mu_med     <- median(posterior$mu)
beta1_med  <- median(posterior$beta1)
beta2_med  <- median(posterior$beta2)
gamma_med  <- median(posterior$gamma)
delta1_med <- median(posterior$delta1)
delta2_med <- median(posterior$delta2)

### Load the spatial data
load(file = here("data", "van_dat_final_750.Rdata"))

# Covariates from your spatial data
X1 <- van_dat$Factor1
X3 <- van_dat$Factor3
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Compute the median of the lognormal distribution (no sigma)
log_mean <- mu_med + beta1_med * X1 + beta2_med * X3
median_lognorm <- exp(log_mean)

# Zero-inflation probability
pi <- plogis(gamma_med + delta1_med * P1 + delta2_med * P2)

# Final simplified expected value accounting for zero inflation (but not truncation or skew)
expected_y <- (1 - pi) * median_lognorm

# Apply truncation: cap values at 6
upper_limit <- 25
expected_y <- pmin(expected_y, upper_limit)

# Build spatial data frame with simplified expected value
predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),
  expected_fw_score = expected_y
)

# Plot the result
pdf(here("figs", "expected_fw_score_no_sigma_ulim6_map.pdf"), width = 12, height = 8)

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = expected_fw_score), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Expected FW Score") +
  theme_minimal() +
  theme(panel.grid = element_blank())

dev.off()

#### using POSTERIOR PREDICTIVE DIST.
# Load posterior samples from Stan
load(file = here("data", "fit_A_FA1_FA3_B_roadweight_natveg_noub.Rdata"))
posterior <- rstan::extract(fit_A_FA1_FA3_B_roadweight_natveg_noub)

# Compute medians from posterior samples
mu_med     <- median(posterior$mu)
beta1_med  <- median(posterior$beta1)
beta2_med  <- median(posterior$beta2)
sigma_med  <- median(posterior$sigma)
gamma_med  <- median(posterior$gamma)
delta1_med <- median(posterior$delta1)
delta2_med <- median(posterior$delta2)

# Load the spatial data
load(file = here("data", "van_dat_final_750.Rdata"))

# Covariates from your spatial data
X1 <- van_dat$Factor1
X3 <- van_dat$Factor3
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Define the truncation upper limit
upper_limit <- 6

# Compute log-mean and sigma
log_mean <- mu_med + beta1_med * X1 + beta2_med * X3
sigma <- sigma_med

# Expected value of the right-truncated lognormal
expected_logmean <- exp(log_mean + 0.5 * sigma^2)
z1 <- (log(upper_limit) - log_mean - sigma^2) / sigma
z2 <- (log(upper_limit) - log_mean) / sigma

E_trunc_lognorm <- expected_logmean * (pnorm(z1) / pnorm(z2))

# Zero-inflation probability
pi <- plogis(gamma_med + delta1_med * P1 + delta2_med * P2)

# Final expected value accounting for zero inflation
expected_y <- (1 - pi) * E_trunc_lognorm

# Build spatial data frame with expected value
predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),
  expected_fw_score = expected_y
)

# Plot the result
pdf(here("figs", "expected_fw_score_from_median_truncZILN.pdf"), width = 12, height = 8)

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = expected_fw_score), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Expected FW Score") +
  theme_minimal() +
  theme(panel.grid = element_blank())

dev.off()





#####
load(file = here("data","fit_A_FA1_FA3_B_roadweight_natveg_noub.Rdata"))
posterior <- rstan::extract(fit_A_FA1_FA3_B_roadweight_natveg_noub_ulim)

# Extract each posterior sample (e.g., 4000 draws)
mu_draws     <- posterior$mu         # vector of length 4000
beta1_draws  <- posterior$beta1
beta2_draws  <- posterior$beta2
sigma_draws  <- posterior$sigma
gamma_draws  <- posterior$gamma
delta1_draws <- posterior$delta1
delta2_draws <- posterior$delta2

load(file = here("data", "van_dat_final_750.Rdata"))

X1 <- van_dat$Factor1
X3 <- van_dat$Factor3 
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Let's say you sample 100 draws for faster computation
n_draws <- 100
draw_ids <- sample(1:length(mu_draws), n_draws)

N_new <- length(X1)
posterior_preds <- matrix(NA, nrow = N_new, ncol = n_draws)
# Define the truncation upper limit
upper_limit <- 6

for (i in seq_along(draw_ids)) {
  d <- draw_ids[i]
  
  log_mean <- mu_draws[d] + beta1_draws[d] * X1 + beta2_draws[d] * X2
  pi <- plogis(gamma_draws[d] + delta1_draws[d] * P1 + delta2_draws[d] * P2)
  
  # Initialize expected_y for each iteration
  expected_y <- numeric(N_new)
  for (n in 1:N_new) {
    if (runif(1) < pi[n]) {
      expected_y[n] <- 0  # Zero-inflation: if zero, set the score to 0
    } else {
      # Truncated lognormal sampling
      y_sample <- upper_limit + 1  # Start above the upper limit
      tries <- 0
      max_tries <- 1000
      
      while (y_sample >= upper_limit && tries < max_tries) {
        y_sample <- exp(log_mean[n] + 0.5 * sigma_draws[d]^2) * rnorm(1)  # Truncated lognormal
        tries <- tries + 1
      }
      
      if (tries < max_tries) {
        expected_y[n] <- y_sample
      } else {
        expected_y[n] <- upper_limit * 0.999  # Fallback just below the upper limit
      }
    }
    
    # Enforce non-negative predictions (min value of 0)
    expected_y[n] <- pmax(expected_y[n], 0)
  }
  
  posterior_preds[, i] <- expected_y
}


fw_score_mean <- rowMeans(posterior_preds)

predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),  # Extract the geometry from the original sf dataframe
  fw_score_mean = fw_score_mean  # Add the posterior mean predictions as a new column
)

# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA3_B_roadweights_natveg_ulim_750m.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




#####

posterior <- rstan::extract(fit_A_FA1_FA3_B_roadweight_natveg_noub_ulim)

# Extract each posterior sample (e.g., 4000 draws)
mu_draws     <- posterior$mu         # vector of length 4000
beta1_draws  <- posterior$beta1
beta2_draws  <- posterior$beta2
sigma_draws  <- posterior$sigma
gamma_draws  <- posterior$gamma
delta1_draws <- posterior$delta1
delta2_draws <- posterior$delta2

load(file = here("data", "van_dat_final_750.Rdata"))

X1 <- van_dat$Factor1
X3 <- van_dat$Factor3 
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Let's say you sample 100 draws for faster computation
n_draws <- 100
draw_ids <- sample(1:length(mu_draws), n_draws)

N_new <- length(X1)
posterior_preds <- matrix(NA, nrow = N_new, ncol = n_draws)
# Define the truncation upper limit
upper_limit <- 6

for (i in seq_along(draw_ids)) {
  d <- draw_ids[i]
  
  log_mean <- mu_draws[d] + beta1_draws[d] * X1 + beta2_draws[d] * X2
  pi <- plogis(gamma_draws[d] + delta1_draws[d] * P1 + delta2_draws[d] * P2)
  
  # Initialize expected_y for each iteration
  expected_y <- numeric(N_new)
  for (n in 1:N_new) {
    if (runif(1) < pi[n]) {
      expected_y[n] <- 0  # Zero-inflation: if zero, set the score to 0
    } else {
      # Truncated lognormal sampling
      y_sample <- upper_limit + 1  # Start above the upper limit
      tries <- 0
      max_tries <- 1000
      
      while (y_sample >= upper_limit && tries < max_tries) {
        y_sample <- exp(log_mean[n] + 0.5 * sigma_draws[d]^2) * rnorm(1)  # Truncated lognormal
        tries <- tries + 1
      }
      
      if (tries < max_tries) {
        expected_y[n] <- y_sample
      } else {
        expected_y[n] <- upper_limit * 0.999  # Fallback just below the upper limit
      }
    }
    
    # Enforce non-negative predictions (min value of 0)
    expected_y[n] <- pmax(expected_y[n], 0)
  }
  
  posterior_preds[, i] <- expected_y
}


fw_score_max <- rowMeans(posterior_preds)

predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),  # Extract the geometry from the original sf dataframe
  fw_score_max = fw_score_max # Add the posterior mean predictions as a new column
)

# Define output file name and resolution
pdf(here("figs","vancouver_fwmax_pred_nohier_A_FA1_FA3_B_roadweights_natveg_ulim_750m.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_max), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




###### 

load(file = here("data","fit_A_FA1_FA2_FA3_B_roadweight_noub.Rdata"))

posterior <- rstan::extract(fit_A_FA1_FA3_B_roadweight_noub)
# Extract each posterior sample (e.g., 4000 draws)
mu_draws     <- posterior$mu         # vector of length 4000
beta1_draws  <- posterior$beta1
beta2_draws  <- posterior$beta2
sigma_draws  <- posterior$sigma
gamma_draws  <- posterior$gamma
delta1_draws <- posterior$delta1


load(file = here("data", "van_dat_final_750.Rdata"))

X1 <- van_dat$Factor1
X2 <- van_dat$Factor3
P1 <- van_dat$service_road_weight


# Let's say you sample 100 draws for faster computation
n_draws <- 100
draw_ids <- sample(1:length(mu_draws), n_draws)

N_new <- length(X1)
posterior_preds <- matrix(NA, nrow = N_new, ncol = n_draws)

for (i in seq_along(draw_ids)) {
  d <- draw_ids[i]
  
  log_mean <- mu_draws[d] + beta1_draws[d] * X1 + beta2_draws[d] * X2
  pi <- plogis(gamma_draws[d] + delta1_draws[d] * P1)
  
  # Expected value of zero-inflated lognormal:
  expected_y <- (1 - pi) * exp(log_mean + 0.5 * sigma_draws[d]^2)
  
  posterior_preds[, i] <- expected_y
}


fw_score_mean <- rowMedians(posterior_preds)


predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),  # Extract the geometry from the original sf dataframe
  fw_score_mean = fw_score_mean  # Add the posterior mean predictions as a new column
)


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA3_B_roadweights_750m_log.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = log(fw_score_mean)), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA3_B_roadweights_750m_lims_05.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals" , limits = c(0, 5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA3_B_roadweights_750m_lims.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals" , limits = c(0, 10)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




# Define output file name and resolution
pdf(here("figs","vancouver_Factor1.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(van_dat) +
  geom_sf(aes(fill = Factor1), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Factor1") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




# Define output file name and resolution
pdf(here("figs","vancouver_Factor3.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(van_dat) +
  geom_sf(aes(fill = Factor3), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Factor3") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()






##############



load(file = here("data","fit_A_FA1_FA3_B_roadweight_natveg_noub_ulim.Rdata"))

posterior <- rstan::extract(fit_A_FA1_FA2_FA3_B_roadweight_natveg_noub_ulim)
# Extract each posterior sample (e.g., 4000 draws)
mu_draws     <- posterior$mu         # vector of length 4000
beta1_draws  <- posterior$beta1
beta2_draws  <- posterior$beta2
sigma_draws  <- posterior$sigma
gamma_draws  <- posterior$gamma
delta1_draws <- posterior$delta1
delta2_draws <- posterior$delta2


load(file = here("data", "van_dat_final_750.Rdata"))

X1 <- van_dat$Factor1
X3 <- van_dat$Factor3
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum


# Let's say you sample 100 draws for faster computation
n_draws <- 100
draw_ids <- sample(1:length(mu_draws), n_draws)

N_new <- length(X1)
posterior_preds <- matrix(NA, nrow = N_new, ncol = n_draws)

for (i in seq_along(draw_ids)) {
  d <- draw_ids[i]
  
  log_mean <- mu_draws[d] + beta1_draws[d] * X1 + beta2_draws[d] * X2
  pi <- plogis(gamma_draws[d] + delta1_draws[d] * P1 + delta2_draws[d] * P2)
  
  # Expected value of zero-inflated lognormal:
  expected_y <- (1 - pi) * exp(log_mean + 0.5 * sigma_draws[d]^2)
  
  posterior_preds[, i] <- expected_y
}


fw_score_mean <- rowMedians(posterior_preds)


predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),  # Extract the geometry from the original sf dataframe
  fw_score_mean = fw_score_mean  # Add the posterior mean predictions as a new column
)


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA2_FA3_B_roadweights_750m.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()

# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA2_FA3_B_roadweights_natveg_750m_log.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = log(fw_score_mean)), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA2_FA3_B_roadweights_natveg_750m_lims_05.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals" , limits = c(0, 5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()


# Define output file name and resolution
pdf(here("figs","vancouver_fw_pred_nohier_A_FA1_FA3_B_roadweights_750m_lims.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals" , limits = c(0, 10)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




###################

library(here)
library(rstan)
library(ggplot2)
library(viridis)
library(matrixStats)
library(sf)

# Load fitted model
#load(file = here("data","fit_A_FA1_FA3_B_roadweight_natveg_noub.Rdata"))

# Extract posterior draws
posterior <- rstan::extract(fit_ZIW_A_FA1_FA3_B_roadweight_natveg_noub)
mu_draws     <- posterior$mu         # intercept
beta1_draws  <- posterior$beta1
beta2_draws  <- posterior$beta2
beta3_draws  <- posterior$beta3
shape_draws  <- posterior$shape      # shape parameter for Weibull
gamma_draws  <- posterior$gamma      # zero-inflation intercept
delta1_draws <- posterior$delta1     # zero-inflation coeffs
delta2_draws <- posterior$delta2

# Load covariates
load(file = here("data", "van_dat_final_750.Rdata"))

X1 <- van_dat$Factor1
X2 <- van_dat$Factor3
P1 <- van_dat$service_road_weight
P2 <- van_dat$NaturalVegSum

# Sample 100 posterior draws for prediction
n_draws <- 100
draw_ids <- sample(1:length(mu_draws), n_draws)

N_new <- length(X1)
posterior_preds <- matrix(NA, nrow = N_new, ncol = n_draws)

# Compute expected values for zero-inflated Weibull
for (i in seq_along(draw_ids)) {
  d <- draw_ids[i]

  # Linear predictor for scale (lambda)
  log_lambda <- mu_draws[d] + beta1_draws[d] * X1 + beta2_draws[d] * X2
  lambda <- exp(log_lambda)

  # Zero-inflation probability
  pi <- plogis(gamma_draws[d] + delta1_draws[d] * P1 + delta2_draws[d] * P2)

  # Weibull shape parameter
  k <- shape_draws[d]

  # Expected value of zero-inflated Weibull
  expected_y <- (1 - pi) * lambda * gamma(1 + 1 / k)

  posterior_preds[, i] <- expected_y
}

# Posterior mean of expected values
fw_score_mean <- rowMedians(posterior_preds)

# Create sf object with predictions
predicted_fw_score <- st_sf(
  geometry = st_geometry(van_dat),
  fw_score_mean = fw_score_mean
)

# -----------------------
# Plot 1: default scale
pdf(here("figs","vancouver_fw_pred_weibull.pdf"), width = 12, height = 8)
ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Predicted Food Waste Vals") +
  theme_minimal() +
  theme(panel.grid = element_blank())
dev.off()

# Plot 2: log scale
pdf(here("figs","vancouver_fw_pred_weibull_log.pdf"), width = 12, height = 8)
ggplot(predicted_fw_score) +
  geom_sf(aes(fill = log(fw_score_mean)), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Log Predicted FW Vals") +
  theme_minimal() +
  theme(panel.grid = element_blank())
dev.off()

# Plot 3: capped at 5
pdf(here("figs","vancouver_fw_pred_weibull_lims_05.pdf"), width = 12, height = 8)
ggplot(predicted_fw_score) +
  geom_sf(aes(fill = fw_score_mean), color = NA) +
  scale_fill_viridis_c(option = "inferno", name = "Predicted FW", limits = c())
                                                                             







##################################
# Define output file name and resolution
pdf(here("figs","vancouver_Factor1.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(van_dat) +
  geom_sf(aes(fill = Factor1), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Factor1") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()




# Define output file name and resolution
pdf(here("figs","vancouver_Factor2.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(van_dat) +
  geom_sf(aes(fill = Factor1), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Factor1") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()



# Define output file name and resolution
pdf(here("figs","vancouver_Factor3.pdf"), width = 12, height = 8)  # Adjust size as needed

ggplot(van_dat) +
  geom_sf(aes(fill = Factor3), color = NA) +  # No borders for a smooth heatmap
  scale_fill_viridis_c(option = "inferno", name = "Factor3") + #, limits = c(0.0000001, 0.5)) +  # "inferno" for bright-darker range
  theme_minimal() +
  theme(panel.grid = element_blank())  # Remove grid lines for a cleaner look

# Close the PDF device to save the file
dev.off()







