// Zero-Inflated Weibull Model

data {
  int<lower=1> N;
  vector[N] X1;
  vector[N] X2;
  vector[N] P1;
  vector[N] P2;
  real<lower=0> fw_score_weighted[N];
}

parameters {
  real<lower=0> alpha;           // Weibull shape
  real mu;                       // Intercept on log scale (scale of Weibull)
  real beta1;
  real beta2;
  
  real gamma;                    // Intercept for zero-inflation
  real delta1;
  real delta2;
}

transformed parameters {
  vector[N] log_scale = mu + beta1 * X1 + beta2 * X2;
  vector<lower=0>[N] sigma = exp(log_scale);  // Weibull scale
  vector<lower=0, upper=1>[N] pi = inv_logit(gamma + delta1 * P1 + delta2 * P2);
}

model {
  // Priors
  alpha ~ normal(0.5, 1);         // Shape around 0.5, moderately flexible
  mu ~ normal(0, 0.5);
  beta1 ~ normal(0, 0.5);
  beta2 ~ normal(0, 0.5);
  
  gamma ~ normal(1, 1);
  delta1 ~ normal(0, 0.5);
  delta2 ~ normal(0, 0.5);
  
  // Likelihood
  for (n in 1:N) {
    if (fw_score_weighted[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]);
    } else {
      target += bernoulli_lpmf(0 | pi[n]) +
                weibull_lpdf(fw_score_weighted[n] | alpha, sigma[n]);
    }
  }
}

generated quantities {
  vector[N] fw_score_pred;

  for (n in 1:N) {
    if (bernoulli_rng(pi[n]) == 1) {
      fw_score_pred[n] = 0;
    } else {
      fw_score_pred[n] = weibull_rng(alpha, sigma[n]);
    }
  }
}
