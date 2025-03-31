// Stan model predicting food waste scores per grid cell (no zero-inflation, no hierarchy, using normal distribution)

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real X1[N];  // Nearest food retail distances
  real fw_score[N];  // Food waste scores
}

// Parameters block
parameters {
  real alpha;  // Global intercept for food waste score
  real beta;   // Coefficient for nearest_food_retail as a predictor
  real sigma;  // Standard deviation of food waste score
}

// Model block
model {
  // Priors for the parameters
  alpha ~ normal(0.5, 0.1);  // A reasonable prior for alpha_base (intercept)
  beta ~ normal(0, 0.1);     // Prior for the effect of nearest_food_retail
  sigma ~ uniform(0, 5);    // Prior for the standard deviation of food waste score
  
  // Likelihood for food waste scores with nearest_food_retail as a predictor
  for (n in 1:N) {
    fw_score[n] ~ normal(alpha + beta * X1[n], sigma);
    // Food waste score modeled as a linear combination of intercept + nearest_food_retail effect
  }
}

// Generated quantities block
generated quantities {
  real fw_score_pred[N];  // Predicted food waste scores
  
  for (n in 1:N) {
    fw_score_pred[n] = normal_rng(alpha + beta * X1[n], sigma);
    // Generate predicted food waste scores using the posterior distributions of alpha_base, beta_food, and fw_score_sd
  }
}



