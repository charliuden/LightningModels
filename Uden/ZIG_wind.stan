// ZIG: Zero-inflated Gamma 

// The input data includes 'S' (observed strikes), 'N' (number of observations), 
// and 'W' (climate variable as a predictor).
data {
  int<lower=0> N;           // Number of observations
  vector<lower=0>[N] S;     // Observations (non-negative values)
  vector[N] W;           // Climate variable
}

// The parameters for the model. 
// 'alpha', 'beta', and 'pi' are now modeled as functions of W.
parameters {
  real a_alpha;        // Intercept for alpha
  real b_alpha;            // Slope for alpha with W
  real<lower=0> a_beta; // Intercept for beta
  real b_beta;             // Slope for beta with W
  real<lower=0, upper=1> a_pi; // Intercept for pi
  real b_pi;               // Slope for pi with W
}

// Transformed parameters to calculate the actual alpha, beta, and pi for each observation
transformed parameters {
  vector[N] alpha = exp(a_alpha + b_alpha * W);  // Alpha must be positive
  vector[N] beta = exp(a_beta + b_beta * W);    // Beta must be positive
  vector[N] pi = inv_logit(a_pi + b_pi * W);    // Pi is constrained between 0 and 1
}

// The model block specifying the likelihood
model {
  // Prior distributions for intercepts and slopes
  a_alpha ~ normal(0, 1);
  b_alpha ~ normal(0, 1);
  a_beta ~ normal(0, 1);
  b_beta ~ normal(0, 1);
  a_pi ~ normal(0, 1);
  b_pi ~ normal(0, 1);

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