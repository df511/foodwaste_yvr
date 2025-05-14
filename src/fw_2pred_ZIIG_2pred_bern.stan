// Zero-inflated inverse Gaussian model with 2 predictors
functions {
  real inv_gauss_lpdf(real y, real mu, real lambda) {
    return 0.5 * log(lambda / (2 * pi())) - 1.5 * log(y) 
           - (lambda * square(y - mu)) / (2 * square(mu) * y);
  }
}

data {
  int<lower=1> N;                      // Number of observations
  vector[N] X1;                        // Predictors for mean
  vector[N] X2;
  vector[N] P1;                        // Predictors for zero-inflation
  vector[N] P2;
  real<lower=0> fw_score_weighted[N]; // Outcome (can include zeros)
}

parameters {
  real mu0;             // Intercept for inverse Gaussian mean
  real beta1;
  real beta2;
  real<lower=0> lambda; // Shape parameter for inverse Gaussian

  real gamma;           // Intercept for zero-inflation
  real delta1;
  real delta2;
}

transformed parameters {
  vector<lower=0>[N] mu_inv_gauss = exp(mu0 + beta1 * X1 + beta2 * X2);
  vector<lower=0, upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);
}

model {
  // Priors
  mu0 ~ normal(-2, 1);
  beta1 ~ normal(0, 0.5);
  beta2 ~ normal(0, 0.5);
  lambda ~ normal(5, 2);

  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 0.5);
  delta2 ~ normal(0, 0.5);

  // Likelihood
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);
    } else {
      target += bernoulli_lpmf(0 | pi[n]) +
                inv_gauss_lpdf(fw_score_weighted[n] | mu_inv_gauss[n], lambda);
    }
  }
}

generated quantities {
  vector[N] fw_score_pred;  // Posterior predictive draws

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 1) {
      fw_score_pred[n] = 0;
    } else {
      real mu = mu_inv_gauss[n];
      real v = normal_rng(0, 1);
      real y = v * v;
      real x = mu + (mu * mu * y) / (2 * lambda) -
               (mu / (2 * lambda)) * sqrt(4 * mu * lambda * y + mu * mu * y * y);
      real z = uniform_rng(0, 1);
      if (z <= mu / (mu + x)) {
        fw_score_pred[n] = x;
      } else {
        fw_score_pred[n] = mu * mu / x;
      }
    }
  }
}

