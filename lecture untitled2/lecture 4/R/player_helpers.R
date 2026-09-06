# Helper functions for Lecture 4: player performance evaluation.
#
# Everything here is base R plus dplyr. Model-fitting packages (lme4, rstanarm,
# cmdstanr) are loaded inside the tutorials that use them so that a tutorial
# which only reads precomputed results can render without them.

suppressPackageStartupMessages(library(dplyr))

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
load_passes <- function(path = file.path("data", "nfl_passes_2023.rds")) {
  passes <- readRDS(path)$passes
  passes %>%
    mutate(
      air_yards_z = (air_yards - mean(air_yards)) / sd(air_yards),
      deep = as.integer(air_yards >= 20),
      pass_location = factor(pass_location, levels = c("middle", "left", "right")),
      receiver_position = factor(receiver_position, levels = c("WR", "TE", "RB", "other")),
      down = factor(down, levels = 1:4),
      passer = factor(passer),
      receiver = factor(receiver),
      defteam = factor(defteam)
    )
}

load_wnba <- function(path = file.path("data", "wnba_2024.rds")) {
  readRDS(path)
}

# ---------------------------------------------------------------------------
# Per-player summaries
# ---------------------------------------------------------------------------
qb_summary <- function(passes) {
  passes %>%
    group_by(passer_id, passer, posteam) %>%
    summarize(
      attempts = n(),
      completions = sum(complete),
      completion_rate = mean(complete),
      epa_per_attempt = mean(qb_epa),
      epa_sd = sd(qb_epa),
      mean_air_yards = mean(air_yards),
      .groups = "drop"
    ) %>%
    arrange(desc(attempts))
}

# ---------------------------------------------------------------------------
# Shrinkage and conjugate updates
# ---------------------------------------------------------------------------

# Weight placed on a group's own mean under the normal-normal model when the
# group has n observations, between-group variance tau2, and within-group
# variance sigma2.
shrinkage_weight <- function(n, tau2, sigma2) {
  n * tau2 / (n * tau2 + sigma2)
}

# Posterior of a group mean theta given its sample mean ybar of n observations,
# a N(mu, tau2) prior, and known within-group variance sigma2.
normal_normal_posterior <- function(ybar, n, sigma2, mu, tau2) {
  precision <- 1 / tau2 + n / sigma2
  posterior_variance <- 1 / precision
  posterior_mean <- posterior_variance * (mu / tau2 + n * ybar / sigma2)
  data.frame(mean = posterior_mean, sd = sqrt(posterior_variance))
}

# Method-of-moments estimate of the between-group variance from group means.
# Var(ybar_j) = tau2 + sigma2 / n_j, so tau2 = Var(ybar_j) - mean(sigma2 / n_j).
estimate_tau2_moments <- function(ybar, n, sigma2) {
  max(var(ybar) - mean(sigma2 / n), 0)
}

# Beta-binomial conjugate update: prior Beta(a, b), observe s successes in t trials.
beta_binomial_update <- function(a, b, successes, trials) {
  c(a = a + successes, b = b + trials - successes)
}

beta_summary <- function(a, b, level = 0.95) {
  alpha <- (1 - level) / 2
  data.frame(
    mean = a / (a + b),
    sd = sqrt(a * b / ((a + b)^2 * (a + b + 1))),
    lower = qbeta(alpha, a, b),
    upper = qbeta(1 - alpha, a, b)
  )
}

# Marginal maximum likelihood for beta-binomial hyperparameters. Each row of the
# input is one player's successes and trials; the player-level rates are
# integrated out analytically.
fit_beta_binomial <- function(successes, trials) {
  negative_log_likelihood <- function(log_ab) {
    a <- exp(log_ab[[1]])
    b <- exp(log_ab[[2]])
    -sum(
      lchoose(trials, successes) +
        lbeta(successes + a, trials - successes + b) -
        lbeta(a, b)
    )
  }

  # Start from method-of-moments values and keep the search in a sensible
  # range; an unbounded quasi-Newton search can wander into a region where the
  # beta functions overflow.
  rates <- successes / trials
  m <- mean(rates)
  v <- max(var(rates) - mean(m * (1 - m) / trials), 1e-6)
  total <- max(m * (1 - m) / v - 1, 2)
  fit <- optim(
    c(log(m * total), log((1 - m) * total)),
    negative_log_likelihood,
    method = "L-BFGS-B",
    lower = log(0.01),
    upper = log(1e5)
  )

  c(a = exp(fit$par[[1]]), b = exp(fit$par[[2]]))
}

# ---------------------------------------------------------------------------
# Posterior computation for a small logistic regression
# ---------------------------------------------------------------------------

# Log posterior of a logistic regression with independent N(0, prior_sd^2)
# priors on every coefficient. X must include the intercept column.
logistic_log_posterior <- function(beta, X, y, prior_sd = 2.5) {
  eta <- as.numeric(X %*% beta)
  log_likelihood <- sum(y * eta - pmax(eta, 0) - log1p(exp(-abs(eta))))
  log_prior <- sum(dnorm(beta, 0, prior_sd, log = TRUE))
  log_likelihood + log_prior
}

# Random-walk Metropolis-Hastings. `log_target` takes a numeric vector and
# returns the log posterior up to a constant.
metropolis_hastings <- function(log_target, initial, iterations, proposal_sd, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  d <- length(initial)
  draws <- matrix(NA_real_, nrow = iterations, ncol = d)
  current <- initial
  current_log <- log_target(current)
  accepted <- 0

  for (i in seq_len(iterations)) {
    proposal <- current + rnorm(d, 0, proposal_sd)
    proposal_log <- log_target(proposal)
    log_ratio <- proposal_log - current_log
    if (log(runif(1)) < log_ratio) {
      current <- proposal
      current_log <- proposal_log
      accepted <- accepted + 1
    }
    draws[i, ] <- current
  }

  list(draws = draws, acceptance_rate = accepted / iterations)
}

# Laplace approximation: a normal centered at the posterior mode with covariance
# equal to the inverse of the negative Hessian at the mode.
laplace_approximation <- function(log_target, initial) {
  fit <- optim(
    initial,
    function(theta) -log_target(theta),
    method = "BFGS",
    hessian = TRUE
  )
  list(
    mode = fit$par,
    covariance = solve(fit$hessian)
  )
}

# Teaching-only single-chain ESS using truncation at the first negative
# autocorrelation. For inference use posterior::ess_bulk() and ess_tail().
effective_sample_size <- function(x) {
  n <- length(x)
  acf_values <- acf(x, lag.max = min(1000, n - 1), plot = FALSE)$acf[-1]
  rho_sum <- 0
  for (k in seq_along(acf_values)) {
    if (acf_values[[k]] < 0) break
    rho_sum <- rho_sum + acf_values[[k]]
  }
  n / (1 + 2 * rho_sum)
}

# Split R-hat for a matrix of draws with one column per chain.
split_rhat <- function(chains) {
  half <- floor(nrow(chains) / 2)
  pieces <- cbind(chains[seq_len(half), , drop = FALSE], chains[half + seq_len(half), , drop = FALSE])
  m <- ncol(pieces)
  n <- nrow(pieces)
  chain_means <- colMeans(pieces)
  chain_vars <- apply(pieces, 2, var)
  between <- n * var(chain_means)
  within <- mean(chain_vars)
  sqrt(((n - 1) / n * within + between / n) / within)
}

# ---------------------------------------------------------------------------
# Ranking with uncertainty
# ---------------------------------------------------------------------------

# Given a draws matrix (rows = posterior draws, columns = players), return the
# probability that each player holds each rank, ranked from best (1) down.
rank_probabilities <- function(draws, higher_is_better = TRUE) {
  ranks <- t(apply(draws, 1, function(row) {
    rank(if (higher_is_better) -row else row, ties.method = "first")
  }))
  colnames(ranks) <- colnames(draws)
  ranks
}

# ---------------------------------------------------------------------------
# Regularized adjusted plus-minus
# ---------------------------------------------------------------------------

# Simulate stints for an adjusted plus-minus example with known player values.
simulate_rapm <- function(players = 60, stints = 2500, true_sd = 2, noise_sd = 25,
                          seed = 431) {
  set.seed(seed)
  true_value <- rnorm(players, 0, true_sd)
  names(true_value) <- sprintf("P%02d", seq_len(players))

  # Split players into six teams of ten; each stint draws five from a home
  # team and five from a different away team.
  team <- rep(seq_len(6), each = players / 6)
  X <- matrix(0, nrow = stints, ncol = players, dimnames = list(NULL, names(true_value)))
  possessions <- numeric(stints)
  y <- numeric(stints)

  for (s in seq_len(stints)) {
    home_team <- sample(6, 1)
    away_team <- sample(setdiff(seq_len(6), home_team), 1)
    # Unequal playing time: earlier-indexed players within a team play more.
    home_weights <- exp(-0.35 * seq_len(10))
    home_players <- sample(which(team == home_team), 5, prob = home_weights)
    away_players <- sample(which(team == away_team), 5, prob = home_weights)
    X[s, home_players] <- 1
    X[s, away_players] <- -1
    possessions[[s]] <- sample(8:40, 1)
    signal <- sum(true_value[home_players]) - sum(true_value[away_players])
    y[[s]] <- signal + rnorm(1, 0, noise_sd / sqrt(possessions[[s]] / 100))
  }

  list(
    X = X,
    y = y,
    possessions = possessions,
    true_value = true_value,
    noise_sd = noise_sd,
    true_sd = true_sd
  )
}

# Weighted ridge regression without an intercept, solved in closed form.
ridge_closed_form <- function(X, y, lambda, weights = rep(1, length(y))) {
  W <- diag(weights)
  XtWX <- t(X) %*% W %*% X
  XtWy <- t(X) %*% W %*% y
  solve(XtWX + lambda * diag(ncol(X)), XtWy)[, 1]
}

# Raw plus-minus: each player's average margin over the stints they played.
raw_plus_minus <- function(X, y, possessions) {
  sapply(seq_len(ncol(X)), function(j) {
    on_court <- X[, j] != 0
    signed <- X[on_court, j] * y[on_court]
    weighted.mean(signed, possessions[on_court])
  })
}
