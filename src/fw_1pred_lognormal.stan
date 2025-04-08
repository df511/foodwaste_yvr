// Stan model predicting food waste scores per grid cell using a Lognormal likelihood

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real<lower=0> X1[N];  // Predictor: Proportion of renters (or other continuous variable)
  real<lower=0> fw_score_weighted[N];  // Food waste scores (must be strictly positive for Lognormal)
}

// Parameters block
parameters {
  real mu;  // Mean parameter (log-scale)
  real beta;  // Coefficient for the effect of X1 (proportion of renters or other predictor)
  real<lower=0> sigma;  // Standard deviation parameter for Lognormal distribution
}

// Transformed parameters block
transformed parameters {
  real log_mean[N];  // Log-scale mean
  for (n in 1:N) {
    log_mean[n] = mu + beta * X1[n];  // Linear predictor on log scale
  }
}

// Model block
model {
  // Priors for the parameters
  mu ~ normal(0, 1);       // Prior for intercept (log mean)
  beta ~ normal(0, 1);   // Prior for the effect of X1 (predictor)
  sigma ~ normal(0, 1);  // Weakly informative prior for standard deviation

  // Likelihood using Lognormal distribution
  for (n in 1:N) {
    fw_score_weighted[n] ~ lognormal(log_mean[n], sigma);  
    // Lognormal(log_mean, sigma) parameterization
  }
}

// Generated quantities block
generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution

  for (n in 1:N) {
    fw_score_pred[n] = lognormal_rng(log_mean[n], sigma);  
    // Generate predicted food waste scores using the posterior predictive distribution
  }
}
