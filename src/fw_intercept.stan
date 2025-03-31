data {
  int<lower=1> N;  // Number of grid cells
  real<lower=0> fw_score[N];  // Food waste scores
  real<lower=0, upper=1> p_zero;  // Probability of zero inflation (constant for all sites)
}

parameters {
  real<lower=0> alpha;  // Global intercept (shape parameter for Gamma)
  real<lower=0> beta;   // Rate parameter for Gamma
}

model {
  // Priors
  alpha ~ lognormal(log(2), 0.5);  // Ensures alpha > 0
  beta ~ lognormal(log(1), 0.5);   // Ensures beta > 0

  // Likelihood with zero-inflation
  for (n in 1:N) {
    if (fw_score[n] == 0) {
      target += bernoulli_logit_lpmf(1 | p_zero);
    } else {
      target += gamma_lpdf(fw_score[n] | alpha, beta);
    }
  }
}

generated quantities {
  real fw_score_pred[N];

  for (n in 1:N) {
    if (bernoulli_logit_rng(p_zero) == 1) {
      fw_score_pred[n] = 0;
    } else {
      fw_score_pred[n] = gamma_rng(alpha, beta);
    }
  }
}


