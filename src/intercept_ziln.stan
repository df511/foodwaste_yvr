//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> N;      // Number of observations
  vector[N] y;         // Response variable (fw_score)
}

parameters {
  real<lower=0> mu;    // Mean of lognormal
  real<lower=0> sigma; // Standard deviation of lognormal
  real<lower=0,upper=1> p; // Probability of zero
}

model {
  for (i in 1:N) {
    if (y[i] == 0) {
      target += log(p);  // Probability mass at zero
    } else {
      target += log1m(p) + lognormal_lpdf(y[i] | mu, sigma);
    }
  }
}

generated quantities {
  vector[N] y_rep;  // Simulated values for posterior predictive checks
  
  for (i in 1:N) {
    if (bernoulli_rng(p) == 1) {
      y_rep[i] = 0;
    } else {
      y_rep[i] = lognormal_rng(mu, sigma);
    }
  }
}


