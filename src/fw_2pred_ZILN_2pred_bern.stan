// Written by Daniel Forrest
// Debugged and optimized
// April 2, 2025

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  vector[N] X1;    // Predictor: Proportion of renters (or other predictor)
  vector[N] X2;    // Predictor: Proportion of renters (or other predictor)
  vector[N] P1;    // Predictor for zero-inflation (e.g., pavement)
  vector[N] P2;    // Predictor for zero-inflation (e.g., pavement)
  real<lower=0> fw_score_weighted[N];  // Food waste scores (including zeros)
}

// Parameters block
parameters {
  real mu;                 // Mean parameter (log-scale)
  real beta1;               // Coefficient for X1 in lognormal model
  real beta2;               // Coefficient for X1 in lognormal model
  real<lower=0> sigma;     // Standard deviation for lognormal model

  // Parameters for zero-inflation (Bernoulli)
  real gamma;             // Intercept for zero-inflation model
  real delta1;             // Coefficient for P1 in zero-inflation model
  real delta2;             // Coefficient for P2 in zero-inflation model
}

// Transformed parameters block
transformed parameters {
  vector[N] log_mean = mu + beta1 * X1 + beta2 * X2;  // Lognormal predictor
  vector<lower=0,upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);  // Zero-inflation probability
}

// Model block
model {
  // Priors
  mu ~ normal(-3.5, 1);            // Slightly wider prior
  beta1 ~ normal(0, 0.5);          // Slightly wider prior
  beta2 ~ normal(0, 0.5);          // Slightly wider prior
  sigma ~ normal(0, 0.5);         // Weakly informative prior

  gamma ~ normal(1, 1);        // Prior for zero-inflation intercept
  delta1 ~ normal(0, 0.5);        // Prior for zero-inflation predictor effect
  delta2 ~ normal(0, 0.5);        // Prior for zero-inflation predictor effect

  // Likelihood using zero-inflated lognormal distribution
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);  // Zero-inflation probability
    } else {
      target += bernoulli_lpmf(0 | pi[n]) + lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma);
    }
  }
}

// Generated quantities block
generated quantities {
  vector[N] fw_score_pred;  // Posterior predictive distribution
  vector[N] pi_pred = pi;   // Store predicted pi values

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 0) {
      fw_score_pred[n] = lognormal_rng(log_mean[n], sigma);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}
