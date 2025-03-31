// Stan model for Zero-Inflated Gamma

data {
  int<lower=0> N;  // Total number of grid cells
  real<lower=0> X1[N];  // Predictor: Proportion of renters (or other continuous variable)
  real<lower=0> fw_score[N];  // Food waste scores (must be strictly positive for Gamma)
}

parameters {
  real alpha;  // Shape parameter (related to mean and variance)
  real beta;  // Coefficient for X1 (effect of predictor)
  real<lower=0> lambda;  // Rate parameter (related to mean)
  real<lower=0, upper=1> p_zero;  // Probability of observing a zero
}

transformed parameters {
  for (n in 1:N) {
    alpha[n] = exp(beta * X1[n]);
  }
}

model {
  // Priors for the parameters
  alpha ~ normal(0.5, 0.1);
  lambda ~ normal(1, 0.1);
  beta ~ normal(0, 0.1);
  p_zero ~ beta(1, 1);  // Prior for the probability of zero-inflation

  // Likelihood with zero-inflation
  for (n in 1:N) {
    if (fw_score[n] == 0) {
      // Zero-inflation: if fw_score is zero, it follows Bernoulli with p_zero
      target += bernoulli_logit_lpmf(1 | logit(p_zero));  // Zero-inflation model
    } else {
      // If the score is non-zero, follow Gamma distribution
      target += gamma_lpdf(fw_score[n] | alpha, 1 / lambda[n]);  // Corrected Gamma likelihood
    }
  }
}

generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution

  for (n in 1:N) {
    if (uniform_rng(0, 1) < p_zero) {
      fw_score_pred[n] = 0;
    } else {
      fw_score_pred[n] = gamma_rng(alpha, 1 / lambda[n]);
    }
  }
}
