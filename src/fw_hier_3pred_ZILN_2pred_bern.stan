// Written by Daniel Forrest
// Hierarchical model: varying intercept by site
// April 7, 2025

// Data block
data {
  int<lower=1> N;                     // Total number of grid cells
  int<lower=1> J;                     // Number of sites (e.g., 13)
  int<lower=1,upper=J> site_id[N];    // Site ID for each grid cell
  vector[N] X1;                       // Predictor 1
  vector[N] X2;                       // Predictor 2
  vector[N] X3;                       // Predictor 3
  vector[N] P1;                       // Predictor1 for zero-inflation
  vector[N] P2;                       // Predictor2 for zero-inflation
  real<lower=0> fw_score_weighted[N]; // Food waste scores (including zeros)
}

// Parameters block
parameters {
  real mu0;                      // Global mean for lognormal intercepts
  real<lower=0> tau_mu;          // Std dev of site-level intercepts
  vector[J] mu_raw;              // Raw intercepts for sites

  real beta1;                    // Coefficient for X1
  real beta2;                    // Coefficient for X2
  real beta3;                    // Coefficient for X3
  real<lower=0> sigma;           // Std dev for lognormal model

  real gamma;                    // Intercept for zero-inflation
  real delta1;                   // Coefficient for P1
  real delta2;                   // Coefficient for P1
}

// Transformed parameters block
transformed parameters {
  vector[J] mu = mu0 + tau_mu * mu_raw;  // Site-specific intercepts
  vector[N] log_mean;
  vector<lower=0,upper=1>[N] pi;

  for (n in 1:N) {
    log_mean[n] = mu[site_id[n]] + beta1 * X1[n] + beta2 * X2[n] + beta3 * X3[n];
    pi[n] = inv_logit(gamma + delta1 * P1[n] + delta2 * P2[n]);
  }
}

// Model block
model {
  // Hyperpriors
  mu0 ~ normal(-4.5, 3);
  tau_mu ~ normal(0, 1);
  mu_raw ~ normal(0, 1);

  // Priors
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  beta3 ~ normal(0, 1);
  sigma ~ normal(0, 1);

  gamma ~ normal(0.8, 0.2);
  delta1 ~ normal(0, 1);
  delta2 ~ normal(0, 1);

  // Likelihood
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);
    } else {
      target += bernoulli_lpmf(0 | pi[n]) + lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma);
    }
  }
}

// Generated quantities block
generated quantities {
  vector[N] fw_score_pred;
  vector[N] pi_pred = pi;

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 0) {
      fw_score_pred[n] = lognormal_rng(log_mean[n], sigma);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}
