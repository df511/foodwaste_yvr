data {
  int<lower=1> N;
  vector[N] X1;
  vector[N] X2;
  vector[N] P1;
  vector[N] P2;
  real<lower=0> fw_score_weighted[N];  // Includes zeros
}

parameters {
  real mu;
  real beta1;
  real beta2;
  real<lower=0> sigma;
  real alpha;  // Skewness parameter

  real gamma;
  real delta1;
  real delta2;
}

transformed parameters {
  vector[N] log_mu = mu + beta1 * X1 + beta2 * X2;
  vector<lower=0,upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);
}

model {
  // Priors
  mu ~ normal(-3.5, 1);
  beta1 ~ normal(0, 0.5);
  beta2 ~ normal(0, 0.5);
  sigma ~ normal(0, 0.5);
  alpha ~ normal(0, 2);  // skewness

  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 0.5);
  delta2 ~ normal(0, 0.5);

  // Likelihood
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);
    } else {
      target += bernoulli_lpmf(0 | pi[n]) +
                skew_normal_lpdf(log(fw_score_weighted[n]) | log_mu[n], sigma, alpha) -
                log(fw_score_weighted[n]);  // Jacobian adjustment
    }
  }
}

generated quantities {
  vector[N] fw_score_pred;

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 1) {
      fw_score_pred[n] = 0;
    } else {
      fw_score_pred[n] = exp(skew_normal_rng(log_mu[n], sigma, alpha));
    }
  }
}
