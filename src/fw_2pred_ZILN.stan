// Stan model predicting food waste scores per grid cell with Zero Inflation

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real X1[N];  // Predictor: Proportion of renters (or other continuous variable)
  real X2[N];  // Predictor: Proportion of renters (or other continuous variable)
  real<lower=0> fw_score_weighted[N];  // Food waste scores (can include zeros)
}

// Parameters block
parameters {
  real mu;  // Mean parameter (log-scale)
  real beta1;  // Coefficient for the effect of X1 (proportion of renters or other predictor)
  real beta2;  // Coefficient for the effect of X1 (proportion of renters or other predictor)
  real<lower=0> sigma;  // Standard deviation parameter for Lognormal distribution
  real<lower=0,upper=1> pi;  // Probability of nonzero food waste (zero-inflation component)
}

// Transformed parameters block
transformed parameters {
  real log_mean[N];  // Log-scale mean
  for (n in 1:N) {
    log_mean[n] = mu + beta1 * X1[n] + beta2 * X2[n];  // Linear predictor on log scale
  }
}

// Model block
model {
  // Priors for the parameters
  mu ~ normal(0, 1);       // Prior for intercept (log mean)
  beta1 ~ normal(0, 1);     // Prior for the effect of X1 (predictor)
  beta2 ~ normal(0, 1);     // Prior for the effect of X1 (predictor)
  sigma ~ normal(0, 1);    // Weakly informative prior for standard deviation
  pi ~ beta(2, 8);         // Prior for zero-inflation probability (~85% zeros)

// Likelihood using zero-inflated Lognormal distribution
for (n in 1:N) {
  if (fw_score_weighted[n] == 0) {
    target += bernoulli_lpmf(1 | pi);  // Probability of being zero
  } else {
    target += bernoulli_lpmf(0 | pi) + lognormal_lpdf(fw_score_weighted[n] | log_mean[n], sigma);
  }
}
}

generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution

  for (n in 1:N) {
    if (bernoulli_rng(pi) == 0) {  // Inverted to match new definition of pi
      fw_score_pred[n] = lognormal_rng(log_mean[n], sigma);
    } else {
      fw_score_pred[n] = 0;
    }
  }
}


