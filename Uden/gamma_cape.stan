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
  int<lower=0> N;           // Number of observations
  vector[N] S;              // Lightning strike rates (observations)
  vector[N] CAPE;           // Climate variable (e.g., CAPE)
}

parameters {
  real<lower=0> a_alpha;    // Intercept for alpha
  real<lower=0> b_alpha;    // Slope for alpha as a function of CAPE
  real<lower=0> a_beta;     // Intercept for beta
  real<lower=0> b_beta;     // Slope for beta as a function of CAPE
}

model {
  vector[N] alpha;         // Shape parameter (alpha) for each observation
  vector[N] beta;          // Rate parameter (beta) for each observation
  
  // Calculate alpha and beta as a function of CAPE
  alpha = a_alpha + b_alpha * CAPE;
  beta = a_beta + b_beta * CAPE;

  // Likelihood: S follows a Gamma distribution with alpha and beta
  S ~ gamma(alpha, beta);

  // Priors
  a_alpha ~ normal(0, 1);  // Prior for intercept of alpha
  b_alpha ~ normal(0, 1);  // Prior for slope of alpha
  a_beta ~ normal(0, 1);   // Prior for intercept of beta
  b_beta ~ normal(0, 1);   // Prior for slope of beta
}

// Generated quantities block for log-likelihood
generated quantities {
  vector[N] log_lik;  // Log-likelihood for each observation

  // Calculate log-likelihood for each observation using the gamma_lpdf function
  for (n in 1:N) {
    // Recalculate alpha and beta in the generated quantities block
    real alpha_n = a_alpha + b_alpha * CAPE[n];
    real beta_n = a_beta + b_beta * CAPE[n];
    
    // Calculate the log-likelihood for each observation
    log_lik[n] = gamma_lpdf(S[n] | alpha_n, beta_n);
  }
}


