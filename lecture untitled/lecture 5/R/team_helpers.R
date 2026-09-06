# Helper functions for Lecture 5: team performance evaluation.
#
# Base R plus dplyr only. Stan-based packages are loaded inside the tutorials
# that use them so the remaining tutorials can render without them.

suppressPackageStartupMessages(library(dplyr))

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
load_nfl_games <- function(path = file.path("data", "nfl_games.rds")) {
  readRDS(path)$games
}

load_epl_matches <- function(path = file.path("data", "epl_matches.rds")) {
  readRDS(path)$matches
}

# ---------------------------------------------------------------------------
# Season summaries
# ---------------------------------------------------------------------------
team_season_table <- function(games) {
  home <- games %>%
    transmute(season, team = home_team, points_for = home_score,
              points_against = away_score, win = home_win, tie = tie)
  away <- games %>%
    transmute(season, team = away_team, points_for = away_score,
              points_against = home_score, win = 1L - home_win - tie, tie = tie)
  bind_rows(home, away) %>%
    group_by(season, team) %>%
    summarize(
      games = n(),
      wins = sum(win),
      ties = sum(tie),
      win_pct = (wins + 0.5 * ties) / games,
      points_for = sum(points_for),
      points_against = sum(points_against),
      point_diff = points_for - points_against,
      .groups = "drop"
    )
}

pythagorean <- function(points_for, points_against, exponent = 2.37) {
  points_for^exponent / (points_for^exponent + points_against^exponent)
}

# ---------------------------------------------------------------------------
# Design matrices and static ratings
# ---------------------------------------------------------------------------

# One row per game, one column per team: +1 for the home team, -1 for the away
# team, 0 otherwise.
build_design_matrix <- function(games, teams = NULL) {
  if (is.null(teams)) teams <- sort(unique(c(games$home_team, games$away_team)))
  X <- matrix(0, nrow = nrow(games), ncol = length(teams), dimnames = list(NULL, teams))
  X[cbind(seq_len(nrow(games)), match(games$home_team, teams))] <- 1
  X[cbind(seq_len(nrow(games)), match(games$away_team, teams))] <- -1
  X
}

# Least-squares ratings with a home-field intercept. The design matrix has rank
# (number of teams - 1) because every row sums to zero, so ratings are only
# identified up to an additive constant. Centering them at zero picks one.
massey_ratings <- function(games) {
  X <- build_design_matrix(games)
  home <- 1 - games$neutral_site
  fit <- lm.fit(cbind(home = home, X[, -1]), games$margin)
  ratings <- c(0, fit$coefficients[-1])
  names(ratings) <- colnames(X)
  list(
    ratings = ratings - mean(ratings),
    home_advantage = unname(fit$coefficients[["home"]])
  )
}

# Ridge ratings: penalize every team rating toward zero. The penalty is not
# applied to the home-advantage term.
ridge_ratings <- function(games, lambda) {
  X <- build_design_matrix(games)
  home <- 1 - games$neutral_site
  A <- cbind(home = home, X)
  penalty <- diag(c(0, rep(lambda, ncol(X))))
  coefficients <- solve(t(A) %*% A + penalty, t(A) %*% games$margin)[, 1]
  list(
    ratings = coefficients[-1],
    home_advantage = unname(coefficients[["home"]])
  )
}

# ---------------------------------------------------------------------------
# Elo ratings
# ---------------------------------------------------------------------------
elo_expected <- function(rating_difference) {
  1 / (1 + 10^(-rating_difference / 400))
}

# Run Elo through a data frame of games ordered in time. Returns the games with
# pregame ratings and expected home-win probabilities attached, plus the final
# ratings. Ties count as half a win. Between seasons every rating is pulled a
# fraction `season_regression` of the way back toward the initial value.
run_elo <- function(games, k = 20, home_advantage = 55, initial = 1500,
                    season_regression = 1 / 3, margin_multiplier = FALSE) {
  teams <- sort(unique(c(games$home_team, games$away_team)))
  rating <- setNames(rep(initial, length(teams)), teams)
  games <- games %>% arrange(season, week, gameday, game_id)
  out <- games
  out$home_elo <- NA_real_
  out$away_elo <- NA_real_
  out$home_prob <- NA_real_
  previous_season <- games$season[[1]]

  for (i in seq_len(nrow(games))) {
    if (games$season[[i]] != previous_season) {
      rating <- rating + season_regression * (initial - rating)
      previous_season <- games$season[[i]]
    }
    h <- games$home_team[[i]]
    a <- games$away_team[[i]]
    advantage <- if (games$neutral_site[[i]] == 1) 0 else home_advantage
    difference <- rating[[h]] + advantage - rating[[a]]
    expected <- elo_expected(difference)
    observed <- if (games$tie[[i]] == 1) 0.5 else games$home_win[[i]]

    multiplier <- 1
    if (margin_multiplier) {
      # FiveThirtyEight-style multiplier: larger margins move ratings more, with
      # a correction so that heavy favorites do not accumulate rating by
      # winning big against weak teams.
      margin <- abs(games$margin[[i]])
      winner_difference <- if (observed >= 0.5) difference else -difference
      multiplier <- log(margin + 1) * 2.2 / (winner_difference * 0.001 + 2.2)
    }

    update <- k * multiplier * (observed - expected)
    out$home_elo[[i]] <- rating[[h]]
    out$away_elo[[i]] <- rating[[a]]
    out$home_prob[[i]] <- expected
    rating[[h]] <- rating[[h]] + update
    rating[[a]] <- rating[[a]] - update
  }

  list(games = out, final_ratings = sort(rating, decreasing = TRUE))
}

# ---------------------------------------------------------------------------
# Kalman filter for a random-walk state-space rating model
# ---------------------------------------------------------------------------

# Observation: margin_i = theta_home - theta_away + h * (1 - neutral) + e,
#   e ~ N(0, sigma_obs^2).
# State: after each week, theta_j <- theta_j + w_j, w_j ~ N(0, sigma_week^2).
# Between seasons, theta_j <- rho * theta_j + v_j, v_j ~ N(0, sigma_season^2).
# The filter processes one week at a time and stores the filtered mean and
# standard deviation for every team before each week's games.
kalman_ratings <- function(games, sigma_obs = 13, sigma_week = 1.2,
                           sigma_season = 4, rho = 0.6, home_advantage = 2,
                           initial_sd = 6) {
  teams <- sort(unique(c(games$home_team, games$away_team)))
  J <- length(teams)
  m <- rep(0, J)
  P <- diag(initial_sd^2, J)
  names(m) <- teams

  games <- games %>% arrange(season, week, gameday, game_id)
  periods <- games %>% distinct(season, week) %>% arrange(season, week)
  filtered <- vector("list", nrow(periods))
  predictions <- games
  predictions$predicted_margin <- NA_real_
  predictions$predicted_sd <- NA_real_
  previous_season <- periods$season[[1]]

  for (p in seq_len(nrow(periods))) {
    if (periods$season[[p]] != previous_season) {
      m <- rho * m
      P <- rho^2 * P + diag(sigma_season^2, J)
      previous_season <- periods$season[[p]]
    } else if (p > 1) {
      P <- P + diag(sigma_week^2, J)
    }

    filtered[[p]] <- data.frame(
      season = periods$season[[p]], week = periods$week[[p]],
      team = teams, mean = m, sd = sqrt(diag(P))
    )

    rows <- which(games$season == periods$season[[p]] & games$week == periods$week[[p]])
    for (i in rows) {
      h <- match(games$home_team[[i]], teams)
      a <- match(games$away_team[[i]], teams)
      H <- rep(0, J)
      H[[h]] <- 1
      H[[a]] <- -1
      advantage <- home_advantage * (1 - games$neutral_site[[i]])
      predicted <- sum(H * m) + advantage
      S <- as.numeric(t(H) %*% P %*% H) + sigma_obs^2
      predictions$predicted_margin[[i]] <- predicted
      predictions$predicted_sd[[i]] <- sqrt(S)

      # Update using this game's observed margin
      K <- (P %*% H) / S
      m <- m + as.numeric(K) * (games$margin[[i]] - predicted)
      P <- P - K %*% t(H) %*% P
    }
  }

  list(
    filtered = bind_rows(filtered),
    predictions = predictions %>%
      mutate(home_prob = 1 - pnorm(0, predicted_margin, predicted_sd)),
    final = data.frame(team = teams, mean = m, sd = sqrt(diag(P))) %>%
      arrange(desc(mean))
  )
}

# ---------------------------------------------------------------------------
# Probabilities from odds, and forecast metrics
# ---------------------------------------------------------------------------
moneyline_to_probability <- function(moneyline) {
  ifelse(moneyline < 0, -moneyline / (-moneyline + 100), 100 / (moneyline + 100))
}

decimal_to_probability <- function(odds) 1 / odds

remove_vig <- function(...) {
  raw <- cbind(...)
  raw / rowSums(raw)
}

brier_score <- function(outcome, probability) mean((outcome - probability)^2)

log_loss <- function(outcome, probability) {
  probability <- pmin(pmax(probability, 1e-7), 1 - 1e-7)
  -mean(outcome * log(probability) + (1 - outcome) * log(1 - probability))
}

# Multiclass (home / draw / away) log loss for soccer.
multiclass_log_loss <- function(outcome_index, probability_matrix) {
  p <- probability_matrix[cbind(seq_along(outcome_index), outcome_index)]
  -mean(log(pmin(pmax(p, 1e-7), 1)))
}

# ---------------------------------------------------------------------------
# Poisson match probabilities
# ---------------------------------------------------------------------------
poisson_match_probabilities <- function(lambda_home, lambda_away, max_goals = 10) {
  home <- dpois(0:max_goals, lambda_home)
  away <- dpois(0:max_goals, lambda_away)
  grid <- outer(home, away)
  c(
    home = sum(grid[lower.tri(grid)]),
    draw = sum(diag(grid)),
    away = sum(grid[upper.tri(grid)])
  )
}
