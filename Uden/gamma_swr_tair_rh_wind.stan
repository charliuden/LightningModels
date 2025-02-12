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
  vector[N] W;              // Climate variable 4
}

parameters {
  real<lower=0> a_alpha;                // Intercept for alpha
  real b_alpha;                // Coefficient for SWR in alpha
  real c_alpha;                // Coefficient for T in alpha
  real d_alpha;                // Coefficient for RH in alpha
  real e_alpha;                // Coefficient for W in alpha
  real<lower=0> a_beta;    // Intercept for beta
  real b_beta;    // Coefficient for SWR in beta
  real c_beta;    // Coefficient for T in beta
  real d_beta;    // Coefficient for RH in beta
  real e_beta;    // Coefficient for W in beta
}

model {
  vector[N] alpha;             //shape and scale
  vector[N] beta;          

  // Define alpha and beta as functions of climate vars
  alpha = a_alpha + b_alpha * SWR + c_alpha * T + d_alpha * RH + e_alpha * W;
  beta = a_beta + b_beta * SWR + c_beta * T + d_beta * RH + e_beta * W;

  // Likelihood
  S ~ gamma(alpha, beta);

  // Priors
  a_alpha ~ normal(0, 2);     // Prior for intercept of alpha
  b_alpha ~ normal(0, 2);     // Prior for SWR coefficient in alpha
  c_alpha ~ normal(0, 2);     // Prior for T coefficient in alpha
  d_alpha ~ normal(0, 2);     // Prior for RH coefficient in alpha
  e_alpha ~ normal(0, 2);     // Prior for W coefficient in alpha
  a_beta ~ normal(0, 2);   // Prior for intercept of beta
  b_beta ~ normal(0, 2);   // Prior for SWR coefficient in beta
  c_beta ~ normal(0, 2);   // Prior for T coefficient in beta
  d_beta ~ normal(0, 2);   // Prior for RH coefficient in beta
  e_beta ~ normal(0, 2);   // Prior for W coefficient in beta
}

generated quantities {
  vector[N] log_lik;        // Log-likelihood for each observation

  for (n in 1:N) {
    real alpha_n = a_alpha + b_alpha * SWR[n] + c_alpha * T[n] + d_alpha * RH[n] + e_alpha * W[n];
    real beta_n = a_beta + b_beta * SWR[n] + c_beta * T[n] + d_beta * RH[n] + e_beta * W[n];
    log_lik[n] = gamma_lpdf(S[n] | alpha_n, beta_n);
  }
}
