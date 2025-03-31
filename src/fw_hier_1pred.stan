data {
  int<lower=1> N;              // Total number of grid cells
  int<lower=1> num_sites;      // Number of sites
  int<lower=1> site_id[N];     // Site identifier for each grid cell
  real<lower=0> nearest_food_retail[N];  // Nearest food retail distances
  real<lower=0> fw_score[N];   // Food waste scores
  real<lower=0, upper=1> p_zero[num_sites];  // Zero-inflation probability for each site
}

parameters {
  real<lower=0> alpha[num_sites];  // FIX: Ensure positive shape parameter
  real alpha_base;
  real<lower=0> alpha_sd;
  
  real food_mean[num_sites];  
  real<lower=0.1> food_sd[num_sites];  // Avoid values too close to 0
}

model {
  // Priors for site-specific alphas
  alpha ~ lognormal(log(0.2), 0.5);  // Ensures positivity but allows flexibility
  // Other priors
  alpha_base ~ normal(0.2, 0.05);
  alpha_sd ~ normal(0.3, 0.1);
  
  food_mean ~ uniform(100, 2000);
  food_sd ~ uniform(0.1, 10);  // Avoid zero

  // Likelihood
  for (n in 1:N) {
    nearest_food_retail[n] ~ normal(food_mean[site_id[n]], food_sd[site_id[n]]);
  }

  // Zero-Inflated Gamma Likelihood
  for (n in 1:N) {
    real p = inv_logit(p_zero[site_id[n]]);
    if (fw_score[n] == 0) {
      target += bernoulli_lpmf(1 | p);
    } else {
      target += log_mix(p,
                        bernoulli_lpmf(0 | p),
                        gamma_lpdf(fw_score[n] | alpha[site_id[n]], 0.5));  // FIX: Alpha now constrained
    }
  }
}


generated quantities {
  real p_zero_pred[N];
  real fw_score_pred[N];
  
  for (n in 1:N) {
    // Predicting zero occurrence based on logistic transformation
    p_zero_pred[n] = bernoulli_rng(inv_logit(p_zero[site_id[n]]));
    if (p_zero_pred[n] == 0) {
      fw_score_pred[n] = gamma_rng(alpha[site_id[n]], 0.5);  // Gamma distribution for predicted values
    } else {
      fw_score_pred[n] = 0;  // Predicted zero for zero-inflated values
    }
  }
}
