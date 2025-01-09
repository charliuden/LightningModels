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
  real<lower=0> alpha;
  real<lower=0> beta;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  S ~ gamma(alpha, beta);
}

// Generated quantities block for log-likelihood
generated quantities {
  vector[N] log_lik;  // Log-likelihood for each observation
  
  // Calculate log-likelihood for each observation using the gamma_lpdf function
  for (n in 1:N) {
    log_lik[n] = gamma_lpdf(S[n] | alpha, beta);
  }
}
