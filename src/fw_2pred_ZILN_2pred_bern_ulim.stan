// Written by Daniel Forrest
// Debugged and optimized
// Updated with right truncation at 10
// May 8, 2025

// Zero-inflated lognormal model with two predictors and upper truncation
// Written by Daniel Forrest; Modified for truncation by ChatGPT (May 2025)
data {
  int<lower=1> N;                         // Total number of observations
  vector[N] X1;                           // Predictor 1 for positive component
  vector[N] X2;                           // Predictor 2 for positive component
  vector[N] P1;                           // Predictor 1 for zero-inflation
  vector[N] P2;                           // Predictor 2 for zero-inflation
  real<lower=0> fw_score_weighted[N];     // Observed response variable (with zeros)
  real<lower=0> upper_limit;              // Upper limit for truncation (e.g., 10)
}

parameters {
  real mu;                      // Lognormal base mean (intercept)
  real beta1;                   // Coefficient for X1
  real beta2;                   // Coefficient for X2
  real<lower=0> sigma;          // Lognormal scale

  real gamma;                   // Intercept for zero-inflation
  real delta1;                  // Coefficient for P1 (zero-inflation)
  real delta2;                  // Coefficient for P2 (zero-inflation)
}

transformed parameters {
  vector[N] log_mean = mu + beta1 * X1 + beta2 * X2;
  vector<lower=0,upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);  // P(being zero)
}

model {
  // Priors
  mu ~ normal(-3.5, 1);
  beta1 ~ normal(0, 0.5);
  beta2 ~ normal(0, 0.5);
  sigma ~ exponential(1);

  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 0.5);
  delta2 ~ normal(0, 0.5);

  // Likelihood with truncation
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);  // Zero inflation
    } else if (fw_score_weighted[n] < upper_limit) {
      target += bernoulli_lpmf(0 | pi[n]) +
                lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma) -
                lognormal_lcdf(upper_limit | log_mean[n], sigma);  // Truncation adjustment
    } else {
      target += negative_infinity();  // Reject values beyond the upper limit
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
      // Truncated lognormal sampling via rejection
      real y_sample = upper_limit + 1;  // initialize above upper limit
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
