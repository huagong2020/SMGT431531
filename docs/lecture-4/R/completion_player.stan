data {
  int<lower=1> J;                         // Quarterbacks in training data
  int<lower=1> G;                         // Training quarterback-game rows
  array[G] int<lower=1> n;                // Attempts per row
  array[G] int<lower=0> s;                // Completions per row
  array[G] int<lower=1, upper=J> qb;       // Quarterback index
  int<lower=1> G_test;                    // Held-out quarterback-game rows
  array[G_test] int<lower=1> n_test;
  array[G_test] int<lower=1, upper=J> qb_test;
}
parameters {
  real alpha;
  real<lower=0> tau;
  vector[J] z;
}
transformed parameters {
  vector[J] u = tau * z;
  vector[J] theta = inv_logit(alpha + u);
}
model {
  alpha ~ normal(0, 2.5);
  tau ~ exponential(1);
  z ~ std_normal();
  s ~ binomial_logit(n, alpha + u[qb]);
}
generated quantities {
  array[G] int s_rep;
  array[G_test] int s_test_rep;
  for (g in 1:G)
    s_rep[g] = binomial_rng(n[g], theta[qb[g]]);
  for (g in 1:G_test)
    s_test_rep[g] = binomial_rng(n_test[g], theta[qb_test[g]]);
}
