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
  int<lower=0> N;         // Number of observations
  real<lower=0> S[N];     // Observations (lower bound at 0)
}

parameters {
  real mu;       // Constrain mu to be positive
  real<lower=0> sigma;    // Constrain sigma to be positive
}

model {
  for (n in 1:N)
    S[n] ~ normal(mu, sigma) T[0,];  // Truncated normal with positive mu
}
