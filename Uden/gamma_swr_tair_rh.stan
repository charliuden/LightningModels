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
  vector[N] SWR;            // Climate variable 1
  vector[N] T;              // Climate variable 2
  vector[N] RH;              // Climate variable 3
}

parameters {
  real<lower=0> a_alpha;                // Intercept for alpha
  real b_alpha;                // Coefficient for SWR in alpha
  real c_alpha;                // Coefficient for T in alpha
  real d_alpha;                // Coefficient for RH in alpha
  real<lower=0> a_beta;    // Intercept for beta
  real b_beta;    // Coefficient for SWR in beta
  real c_beta;    // Coefficient for T in beta
  real d_beta;    // Coefficient for RH in beta
}

model {
  vector[N] alpha;             // Mean for the normal distribution
  vector[N] beta;          // Standard deviation for the normal distribution

  // Define alpha and beta as functions of SWR and T
  alpha = a_alpha + b_alpha * SWR + c_alpha * T + d_alpha * RH;
  beta = a_beta + b_beta * SWR + c_beta * T + d_beta * RH;

  // Likelihood
  S ~ gamma(alpha, beta);

  // Priors
  a_alpha ~ normal(0, 1);     // Prior for intercept of alpha
  b_alpha ~ normal(0, 1);     // Prior for SWR coefficient in alpha
  c_alpha ~ normal(0, 1);     // Prior for T coefficient in alpha
  d_alpha ~ normal(0, 1);     // Prior for RH coefficient in alpha
  a_beta ~ normal(0, 1);   // Prior for intercept of beta
  b_beta ~ normal(0, 1);   // Prior for SWR coefficient in beta
  c_beta ~ normal(0, 1);   // Prior for T coefficient in beta
  d_beta ~ normal(0, 1);   // Prior for RH coefficient in beta
}

generated quantities {
  vector[N] log_lik;        // Log-likelihood for each observation

  for (n in 1:N) {
    real alpha_n = a_alpha + b_alpha * SWR[n] + c_alpha * T[n] + d_alpha * RH[n];
    real beta_n = a_beta + b_beta * SWR[n] + c_beta * T[n] + d_beta * RH[n];
    log_lik[n] = gamma_lpdf(S[n] | alpha_n, beta_n);
  }
}
