// ZIG: Zero-inflated Gamma 

// The input data includes 'S' (observed strikes), 'N' (number of observations), 
// and 'SWR' (climate variable as a predictor).
data {
  int<lower=0> N;           // Number of observations
  vector<lower=0>[N] S;     // Observations (non-negative values)
  vector[N] SWR;           // Climate variable
  vector[N] T;           // Climate variable
  vector[N] RH;           // Climate variable
  vector[N] W;           // Climate variable
  vector[N] P;           // Climate variable
  vector[N] SP;           // Climate variable
}

// The parameters for the model. 
// 'alpha', 'beta', and 'pi' are now modeled as functions of SWR.
parameters {
  real<lower=0> a_alpha;      
  real b_alpha;            
  real c_alpha;
  real d_alpha;
  real e_alpha;
  real f_alpha;
  real g_alpha;
  real<lower=0> a_beta; 
  real b_beta;            
  real c_beta;
  real d_beta;
  real e_beta;
  real f_beta;
  real g_beta;
  real<lower=0, upper=1> a_pi; 
  real b_pi; 
  real c_pi;
  real d_pi;
  real e_pi;
  real f_pi;
  real g_pi;
}

// Transformed parameters to calculate the actual alpha, beta, and pi for each observation
transformed parameters {
  vector[N] alpha = exp(a_alpha + b_alpha * SWR + c_alpha * T + d_alpha * RH + e_alpha * W + f_alpha * P + g_alpha * SP);  // Alpha must be positive
  vector[N] beta = exp(a_beta + b_beta * SWR + c_beta * T + d_beta * RH + e_beta * W + f_beta * P + g_beta * SP);    // Beta must be positive
  vector[N] pi = inv_logit(a_pi + b_pi * SWR + c_pi * T + d_pi * RH + e_pi * W + f_pi * P + g_pi * SP);    // Pi is constrained between 0 and 1
}

// The model block specifying the likelihood
model {
  // Prior distributions for intercepts and slopes
  a_alpha ~ exponential(1);
  b_alpha ~ normal(0,1);
  c_alpha ~ normal(0,1);
  d_alpha ~ normal(0,1);
  e_alpha ~ normal(0,1);
  f_alpha ~ normal(0,1);
  g_alpha ~ normal(0,1);
  a_beta ~ exponential(1);
  b_beta ~ normal(0,1);
  c_beta ~ normal(0,1);
  d_beta ~ normal(0,1);
  e_beta ~ normal(0,1);
  f_beta ~ normal(0,1);
  g_beta ~ normal(0,1);
  a_pi ~ exponential(1);
  b_pi ~ normal(0,1);
  c_pi ~ normal(0,1);
  d_pi ~ normal(0,1);
  e_pi ~ normal(0,1);
  f_pi ~ normal(0,1);
  g_pi ~ normal(0,1);

  // Likelihood for each observation
  for (n in 1:N) {
    if (S[n] == 0) {
      target += bernoulli_lpmf(1 | pi[n]); // Zero-inflation likelihood
    } else {
      target += log_mix(
        pi[n],
        bernoulli_lpmf(0 | pi[n]),            // Log-probability of zero in the Gamma
        gamma_lpdf(S[n] | alpha[n], beta[n]) // Gamma likelihood
      );
    }
  }
}

// Generated quantities block for posterior predictions and log-likelihood
generated quantities {
  vector[N] log_lik;  // Log-likelihood for each observation
  
  for (n in 1:N) {
    if (S[n] == 0) {
      log_lik[n] = bernoulli_lpmf(1 | pi[n]); // Log-likelihood for zero
    } else {
      log_lik[n] = log_mix(
        pi[n],
        bernoulli_lpmf(0 | pi[n]),           // Log-probability of zero in the Gamma
        gamma_lpdf(S[n] | alpha[n], beta[n]) // Gamma likelihood
      );
    }
  }
}

