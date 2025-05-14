
// Written by Daniel Forrest
// Hierarchical model: varying intercept by site
// May 8, 2025


data {
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1,upper=J> site_id[N];
  int<lower=1> K;                          // Number of pickup types
  int<lower=1,upper=K> pickup_id[N]; // Pickup type index for each obs
  vector[N] X1;
  vector[N] X2;
  vector[N] P1;
  real<lower=0> fw_score_weighted[N];
  real<lower=0> upper_limit;
}


parameters {
  real mu0;
  real<lower=0> tau_mu;
  vector[J] mu_raw;

  real<lower=0> tau_pickup;       // NEW: std dev for pickup type intercepts
  vector[K] pickup_raw;           // NEW: pickup-type intercepts (standardized)

  real beta1;
  real beta2;
  real<lower=0> sigma;

  real gamma;
  real delta1;
}


transformed parameters {
  vector[J] mu = mu0 + tau_mu * mu_raw;
  vector[K] pickup_eff = tau_pickup * pickup_raw;  // NEW: scaled pickup-type intercepts
  vector[N] log_mean;
  vector<lower=0,upper=1>[N] pi;

  for (n in 1:N) {
  log_mean[n] = mu[site_id[n]] + beta1 * X1[n] + beta2 * X2[n];
  pi[n] = inv_logit(gamma + delta1 * P1[n] + pickup_eff[pickup_id[n]]);  // moved pickup effect here
}
}
model {
  // Hyperpriors
  mu0 ~ normal(-4.5, 3);
  tau_mu ~ normal(0, 1);
  mu_raw ~ normal(0, 1);

  tau_pickup ~ normal(0, 1);         // NEW
  pickup_raw ~ normal(0, 1);         // NEW

  // Priors
  beta1 ~ normal(0, 1);
  beta2 ~ normal(0, 1);
  sigma ~ normal(0, 1);

  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 1);

  // Likelihood with truncation
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);
    } else if (fw_score_weighted[n] < upper_limit) {
      target += bernoulli_lpmf(0 | pi[n]) +
                lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma) -
                lognormal_lcdf(upper_limit | log_mean[n], sigma);
    } else {
      target += negative_infinity();
    }
  }
}

generated quantities {
  vector[N] fw_score_pred;
  vector[N] pi_pred = pi;

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 1) {
      fw_score_pred[n] = 0;
    } else {
      real y_sample = upper_limit + 1;
      int tries = 0;
      int max_tries = 1000;

      while (y_sample >= upper_limit && tries < max_tries) {
        y_sample = lognormal_rng(log_mean[n], sigma);  // already includes site + pickup effect
        tries += 1;
      }

      fw_score_pred[n] = (tries < max_tries) ? y_sample : upper_limit * 0.999;
    }
  }
}
