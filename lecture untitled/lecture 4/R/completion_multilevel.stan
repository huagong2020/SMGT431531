// Bayesian multilevel logistic regression for pass completion.
//
// Level one: pass attempt i with covariates X[i] and outcome y[i].
// Level two: crossed varying intercepts for the passer, the receiver, and the
// defense. Each set of intercepts has its own standard deviation, which is
// itself a parameter with a half-normal prior.
//
// The varying intercepts use the non-centered parameterization
//   u = tau * z,  z ~ normal(0, 1),
// which samples much more efficiently than u ~ normal(0, tau) when tau is
// small relative to the information in the data.

data {
  int<lower=1> N;                       // pass attempts
  int<lower=1> K;                       // covariates (no intercept column)
  int<lower=1> J_qb;                    // passers
  int<lower=1> J_rec;                   // receivers
  int<lower=1> J_def;                   // defenses
  matrix[N, K] X;
  array[N] int<lower=0, upper=1> y;
  array[N] int<lower=1, upper=J_qb> qb;
  array[N] int<lower=1, upper=J_rec> rec;
  array[N] int<lower=1, upper=J_def> def;
}

parameters {
  real alpha;
  vector[K] beta;
  vector[J_qb] z_qb;
  vector[J_rec] z_rec;
  vector[J_def] z_def;
  real<lower=0> tau_qb;
  real<lower=0> tau_rec;
  real<lower=0> tau_def;
}

transformed parameters {
  vector[J_qb] u_qb = tau_qb * z_qb;
  vector[J_rec] u_rec = tau_rec * z_rec;
  vector[J_def] u_def = tau_def * z_def;
}

model {
  // Priors
  alpha ~ normal(0, 2.5);
  beta ~ normal(0, 1);
  z_qb ~ std_normal();
  z_rec ~ std_normal();
  z_def ~ std_normal();
  tau_qb ~ normal(0, 1);
  tau_rec ~ normal(0, 1);
  tau_def ~ normal(0, 1);

  // Likelihood
  y ~ bernoulli_logit(alpha + X * beta + u_qb[qb] + u_rec[rec] + u_def[def]);
}
