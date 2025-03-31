// Stan model predicting food waste scores per grid cell (non-hierarchical)

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real<lower=0> nearest_food_retail[N];  // Nearest food retail distances
  real<lower=0> fw_score[N];  // Food waste scores
  real<lower=0, upper=1> p_zero;  // Zero-inflation probability (scalar)
}

// Parameters block
parameters {
  real<lower=0> alpha_base;  // Base intercept for food waste score
  real<lower=0> food_mean_base;  // Base mean for nearest food retail
  real<lower=0> food_sd_base;  // Base SD for nearest food retail

  real<lower=0> food_mean;  // Mean for nearest food retail
  real<lower=0> food_sd;    // SD for nearest food retail
}

// Model block
model {
  // Prior for food waste score intercept
  alpha_base ~ normal(0.2, 0.3);  // A simple prior for alpha_base
  
  // Prior for nearest food retail
  food_mean ~ normal(100, 2000);
  food_sd ~ uniform(50, 500);

  // Likelihood for nearest_food_retail
  for (n in 1:N) {
    nearest_food_retail[n] ~ normal(food_mean, food_sd);
  }

  // Zero-inflated food waste score likelihood
  for (n in 1:N) {
    if (fw_score[n] == 0) {
      target += bernoulli_logit_lpmf(1 | p_zero);
    } else {
      target += gamma_lpdf(fw_score[n] | alpha_base * 2, 0.5);  // Use global alpha
    }
  }
}

// Generated quantities block
generated quantities {
  real p_zero_pred[N];
  real fw_score_pred[N];
  
  for (n in 1:N) {
    // Zero-inflation prediction
    p_zero_pred[n] = bernoulli_logit_rng(p_zero);
    if (p_zero_pred[n] == 0) {
      fw_score_pred[n] = gamma_rng(alpha_base * 2, 0.5);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}
