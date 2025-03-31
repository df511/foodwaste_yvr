// Stan model predicting food waste scores with multiple predictors (Gamma likelihood)

data {
  int<lower=1> N;  // Total number of grid cells
  real X1[N];  // Predictor 1 (e.g., proportion of renters)
  real X2[N];  // Predictor 2 (e.g., proportion of migrants)
  real X3[N];  // Predictor 3 (e.g., dist to food retail)
  real<lower=0> fw_score[N];  // Food waste scores (must be strictly positive)
}

parameters {
  real alpha;  // Intercept
  real beta1;  // Coefficient for X1
  real beta2;  // Coefficient for X2
  real beta3;  // Coefficient for X3
  real<lower=0> phi;  // Precision parameter for Gamma distribution
}

transformed parameters {
  real<lower=0> mu[N];  // Mean of Gamma distribution
  for (n in 1:N) {
    mu[n] = exp(alpha + beta1 * X1[n] + beta2 * X2[n] + beta3 * X3[n]);  
    // Linear predictor in log-space
  }
}

model {
  // Priors for parameters
  alpha ~ normal(0.5, 0.1);  
  beta1 ~ normal(0, 1);   
  beta2 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  phi ~ gamma(2, 0.5);  

  // Likelihood: Gamma distribution
  for (n in 1:N) {
    fw_score[n] ~ gamma(phi, phi / mu[n]);  
  }
}

generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution
  for (n in 1:N) {
    fw_score_pred[n] = gamma_rng(phi, phi / mu[n]);  
  }
}
