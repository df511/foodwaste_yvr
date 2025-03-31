// Stan model predicting food waste scores per grid cell using a Gamma likelihood

// Data block
data {
  int<lower=1> N;  // Total number of grid cells
  real<lower=0> X1[N];  // Predictor: Proportion of renters (or other continuous variable)
  real<lower=0> fw_score[N];  // Food waste scores (must be strictly positive for Gamma)
}

// Parameters block
parameters {
  real alpha;  // Shape parameter (related to mean and variance)
  real<lower=0> theta;  // Rate parameter (related to mean)
  real<lower=0> phi;  // Precision parameter for Gamma distribution (could be used to control variance)
  real beta;  // Coefficient for the effect of X1 (proportion of renters or other predictor)
}

// Transformed parameters block
transformed parameters {
  real<lower=0> lambda[N];  // Mean (lambda) of Gamma distribution (must be positive)
  for (n in 1:N) {
    lambda[n] = alpha * theta * exp(beta * X1[n]);  // Ensure the mean follows alpha * theta * exp(beta * X1[n])
  }
}

// Model block
model {
  // Priors for the parameters
  alpha ~ normal(0.5, 0.1);  // Prior for intercept (shape parameter)
  theta ~ normal(1, 0.1);   // Prior for rate parameter
  beta ~ normal(0, 0.1);     // Prior for the effect of X1 (predictor)
  phi ~ gamma(2, 0.5);       // Weakly informative prior for precision parameter

  // Likelihood using Gamma distribution
  for (n in 1:N) {
    fw_score[n] ~ gamma(alpha, 1 / lambda[n]);  
    // Gamma(shape = alpha, rate = 1 / lambda) parameterization
  }
}

// Generated quantities block
generated quantities {
  real fw_score_pred[N];  // Posterior predictive distribution

  for (n in 1:N) {
    fw_score_pred[n] = gamma_rng(alpha, 1 / lambda[n]);  
    // Generate predicted food waste scores using the posterior predictive distribution
  }
}
