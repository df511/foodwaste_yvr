
// Written by Daniel Forrest
// Hierarchical model: varying intercept by site
// May 8, 2025


data {
  int<lower=1> N;                     // Number of observations
  int<lower=1> J;                     // Number of sites
  int<lower=1,upper=J> site_id[N];    // Site ID
  vector[N] X1;                       // Predictor 1
  vector[N] X2;                       // Predictor 2
  vector[N] P1;                       // Predictor for zero-inflation
  real<lower=0> fw_score_weighted[N]; // Response variable
  real<lower=0> upper_limit;          // Truncation point
}

parameters {
  real mu0;
  real<lower=0> tau_mu;
  vector[J] mu_raw;

  real beta1;
  real beta2;
  real<lower=0> sigma;

  real gamma;
  real delta1;
}

transformed parameters {
  vector[J] mu = mu0 + tau_mu * mu_raw;
  vector[N] log_mean;
  vector<lower=0,upper=1>[N] pi;

  for (n in 1:N) {
    log_mean[n] = mu[site_id[n]] + beta1 * X1[n] + beta2 * X2[n];
    pi[n] = inv_logit(gamma + delta1 * P1[n]);
  }
}

model {
  // Hyperpriors
  mu0 ~ normal(-4.5, 3);
  tau_mu ~ normal(0, 1);
  mu_raw ~ normal(0, 1);

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
      target += negative_infinity();  // reject if observed > upper_limit
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
        y_sample = lognormal_rng(log_mean[n], sigma);
        tries += 1;
      }

      fw_score_pred[n] = (tries < max_tries) ? y_sample : upper_limit * 0.999;
    }
  }
}
