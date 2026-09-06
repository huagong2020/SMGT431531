// Dynamic team ratings: a random-walk state-space model for game margins
// in the spirit of Glickman and Stern (1998).
//
// theta[j, t] is the strength of team j in period t (one period per NFL week).
// Within a season the strengths follow a random walk; between seasons they
// are shrunk toward zero by a factor rho and receive a larger innovation.
//
// margin_i ~ normal(theta[home_i, t_i] - theta[away_i, t_i] + h * home_i, sigma_obs)

data {
  int<lower=1> N;                             // games
  int<lower=1> J;                             // teams
  int<lower=1> T;                             // periods (weeks across seasons)
  array[N] int<lower=1, upper=J> home;
  array[N] int<lower=1, upper=J> away;
  array[N] int<lower=1, upper=T> period;
  vector[N] margin;
  vector<lower=0, upper=1>[N] home_field;     // 0 at neutral sites
  array[T] int<lower=0, upper=1> season_start; // 1 if period t starts a new season
}

parameters {
  matrix[J, T] z;                              // standardized innovations
  real<lower=0> sigma_obs;
  real<lower=0> sigma_week;
  real<lower=0> sigma_season;
  real<lower=0> sigma_initial;
  real<lower=0, upper=1> rho;
  real h;
}

transformed parameters {
  matrix[J, T] theta;
  theta[, 1] = sigma_initial * z[, 1];
  for (t in 2:T) {
    if (season_start[t] == 1) {
      theta[, t] = rho * theta[, t - 1] + sigma_season * z[, t];
    } else {
      theta[, t] = theta[, t - 1] + sigma_week * z[, t];
    }
  }
}

model {
  to_vector(z) ~ std_normal();
  sigma_obs ~ normal(0, 20);
  sigma_week ~ normal(0, 3);
  sigma_season ~ normal(0, 6);
  sigma_initial ~ normal(0, 10);
  rho ~ beta(4, 2);
  h ~ normal(2, 3);

  {
    vector[N] mu;
    for (i in 1:N) {
      mu[i] = theta[home[i], period[i]] - theta[away[i], period[i]] + h * home_field[i];
    }
    margin ~ normal(mu, sigma_obs);
  }
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N) {
    real mu_i = theta[home[i], period[i]] - theta[away[i], period[i]] + h * home_field[i];
    log_lik[i] = normal_lpdf(margin[i] | mu_i, sigma_obs);
  }
}
