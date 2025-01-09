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
// ZIG: Zero-inflated Gamma 

// The input data is a vector 'S' of length 'N'.
data {
  int<lower=0> N;         // Number of observations
  vector<lower=0>[N] S;   // Observations (non-negative values)
}

// The parameters accepted by the model. Our model
// now includes an additional parameter 'pi' for the zero-inflation probability.
parameters {
  real<lower=0> alpha;      // Shape parameter for the gamma distribution
  real<lower=0> beta;       // Rate parameter for the gamma distribution
  real<lower=0, upper=1> pi; // Probability of zero inflation
}

// The model to be estimated. Observations are modeled as coming from a
// mixture of a Bernoulli distribution (for zeros) and a Gamma distribution (for positive values).
model {
  for (n in 1:N) {
    if (S[n] == 0) {
      target += bernoulli_lpmf(1 | pi); // Log-probability for zero
    } else {
      target += log_mix(pi,
                        bernoulli_lpmf(0 | pi),               // Log-probability of zero in the Gamma
                        gamma_lpdf(S[n] | alpha, beta));     // Log-probability of Gamma
    }
  }
  alpha ~ normal(2, 1);  // Expecting shape parameter near 2
  beta ~ normal(1, 1);   // Expecting rate parameter near 1
  pi ~ beta(2, 2);       // Expecting pi to be moderate (not too close to 0 or 1)
}

// Generated quantities block for log-likelihood
generated quantities {
  vector[N] log_lik;  // Log-likelihood for each observation
  
  for (n in 1:N) {
    if (S[n] == 0) {
      log_lik[n] = bernoulli_lpmf(1 | pi); // Log-likelihood for zero
    } else {
      log_lik[n] = log_mix(pi,
                           bernoulli_lpmf(0 | pi),          // Log-probability of zero in the Gamma
                           gamma_lpdf(S[n] | alpha, beta));// Log-probability of Gamma
    }
  }
}