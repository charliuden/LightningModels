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

// The input data is a vector 'S' of length 'N'.
data {
  int<lower=0> N;
  vector[N] S;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real<lower=0> mu;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  S ~ normal(mu, sigma);
}

// Generated quantities block for log-likelihood
generated quantities {
  vector[N] log_lik;         // Log-likelihood for each observation
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(S[n] | mu, sigma); // Calculate log-likelihood
  }
}
