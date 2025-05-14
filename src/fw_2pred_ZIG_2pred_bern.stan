// Gamma zero-inflated model version

data {
  int<lower=1> N;
  vector[N] X1;
  vector[N] X2;
  vector[N] P1;
  vector[N] P2;
  real<lower=0> fw_score_weighted[N];
}

parameters {
  real mu;                // Intercept on log-scale
  real beta1;
  real beta2;
  real<lower=0> phi;      // Gamma shape parameter

  real gamma;             // Zero-inflation intercept
  real delta1;
  real delta2;
}

transformed parameters {
  vector[N] mu_gamma = exp(mu + beta1 * X1 + beta2 * X2); // Mean of Gamma
  vector<lower=0>[N] rate = phi ./ mu_gamma;              // Rate = shape / mean
  vector<lower=0, upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);
}

model {
  // Priors
  mu ~ normal(-3.5, 1);
  beta1 ~ normal(0, 0.5);
  beta2 ~ normal(0, 0.5);
  phi ~ gamma(2, 1);  // Weakly informative, favoring moderate shape

  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 0.5);
  delta2 ~ normal(0, 0.5);

  // Likelihood
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);  // Inflated zero
    } else {
      target += bernoulli_lpmf(0 | pi[n]) +
                gamma_lpdf(fw_score_weighted[n] | phi, rate[n]);
    }
  }
}

generated quantities {
  vector[N] fw_score_pred;
  vector[N] pi_pred = pi;

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 0) {
      fw_score_pred[n] = gamma_rng(phi, rate[n]);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}
