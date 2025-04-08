// Written by Daniel Forrest
// March 31, 2025

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real X1[N];  // Predictor: Proportion of renters (or other predictor)
  real P1[N]; // Predictor of Bernoulli: pavement
  real<lower=0> fw_score_weighted[N];  // Food waste scores (can include zeros)
}

// Parameters block
parameters {
  real mu;  // Mean parameter (log-scale)
  real beta;  // Coefficient for X1 in lognormal model
  real<lower=0> sigma;  // Standard deviation for lognormal model
  
  // Parameters for zero-inflation model (Bernoulli)
  real gamma0;  // Intercept for logistic regression
  real gamma1;  // Coefficient for P1 in Bernoulli model
}

// Transformed parameters block
transformed parameters {
  real log_mean[N];  // Log-scale mean for lognormal model
  real<lower=0,upper=1> pi[N];  // Zero-inflation probability per grid cell

  for (n in 1:N) {
    log_mean[n] = mu + beta * X1[n];  // Lognormal linear predictor
    pi[n] = inv_logit(gamma0 + gamma1 * P1[n]);  // Logistic regression for zero-inflation probability
  }
}

// Model block
model {
  // Priors
  mu ~ normal(0, 1);
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);
  
  gamma0 ~ normal(0, 1);  // Prior for zero-inflation intercept
  gamma1 ~ normal(0, 1);  // Prior for zero-inflation predictor effect

  // Likelihood using zero-inflated lognormal distribution
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);  // Probability of being zero
    } else {
      target += bernoulli_lpmf(0 | pi[n]) + lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma);
    }
  }
}

// Generated quantities block
generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution
  real pi_out[N];  // Store estimated zero-inflation probabilities

  for (n in 1:N) {
    pi_out[n] = pi[n];  // Save estimated probabilities

    if (bernoulli_rng(pi[n]) == 0) {  // Sample from Bernoulli with predictor-dependent probability
      fw_score_pred[n] = lognormal_rng(log_mean[n], sigma);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}
