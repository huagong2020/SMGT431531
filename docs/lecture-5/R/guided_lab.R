## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("R/team_helpers.R")
games <- load_nfl_games() %>% filter(season == 2023)
train <- games %>% filter(week <= 10)
test <- games %>% filter(week > 10)
teams <- sort(unique(c(train$home_team, train$away_team)))
X <- build_design_matrix(train, teams)
h <- 1 - train$neutral_site
y <- train$margin
stopifnot(!anyNA(X), !anyDuplicated(train$game_id),
  length(intersect(train$game_id, test$game_id)) == 0,
  all(c(test$home_team, test$away_team) %in% teams))
data.frame(training_games = nrow(train), test_games = nrow(test), teams = length(teams))


## -----------------------------------------------------------------------------
gibbs_teams <- function(X, h, y, seed, iterations = 4000, warmup = 1500) {
  set.seed(seed)
  A <- cbind(home = h, X)
  J <- ncol(X)
  AtA <- crossprod(A)
  Aty <- crossprod(A, y)
  tau2 <- runif(1, 10, 50)
  sigma2 <- runif(1, 100, 250)
  out <- matrix(NA_real_, iterations - warmup, J + 3)
  colnames(out) <- c("alpha", colnames(X), "tau", "sigma")
  for (s in seq_len(iterations)) {
    Q <- AtA / sigma2 + diag(c(1 / 9, rep(1 / tau2, J)))
    R <- chol(Q)
    m <- backsolve(R, forwardsolve(t(R), Aty / sigma2))
    b <- as.numeric(m) + backsolve(R, rnorm(J + 1))
    tau2 <- 1 / rgamma(1, 2 + J / 2, rate = 25 + sum(b[-1]^2) / 2)
    residual <- y - as.numeric(A %*% b)
    sigma2 <- 1 / rgamma(1, 2 + length(y) / 2, rate = 169 + sum(residual^2) / 2)
    if (s > warmup) out[s - warmup, ] <- c(b, sqrt(tau2), sqrt(sigma2))
  }
  out
}
chains <- lapply(541:544, function(seed) gibbs_teams(X, h, y, seed))
draws <- do.call(rbind, chains)
draw_array <- array(NA_real_, c(nrow(chains[[1]]), 4, ncol(draws)),
  dimnames = list(NULL, NULL, colnames(draws)))
for (c in 1:4) draw_array[, c, ] <- chains[[c]]
diagnostics <- posterior::summarise_draws(posterior::as_draws_array(draw_array))
diagnostics %>% filter(variable %in% c("alpha", "tau", "sigma")) %>%
  select(variable, mean, sd, rhat, ess_bulk, ess_tail) %>% knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
centered <- draws[, teams] - rowMeans(draws[, teams])
table_ratings <- data.frame(team = teams, mean = colMeans(centered),
  lower = apply(centered, 2, quantile, 0.025),
  upper = apply(centered, 2, quantile, 0.975)) %>% arrange(desc(mean))
table_ratings %>% slice_head(n = 10) %>% knitr::kable(digits = 2)

pair <- c("KC", "BUF")
delta <- draws[, pair[1]] - draws[, pair[2]]
data.frame(comparison = "KC minus BUF, neutral field",
  mean_difference = mean(delta), lower = quantile(delta, 0.025),
  upper = quantile(delta, 0.975), probability_stronger = mean(delta > 0)) %>%
  knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
set.seed(545)
keep <- sample(nrow(draws), 500)
A_train <- cbind(home = h, X)
eta_train <- draws[keep, c("alpha", teams)] %*% t(A_train)
yrep <- eta_train + matrix(rnorm(length(eta_train)), nrow = length(keep)) * draws[keep, "sigma"]
max_margin <- apply(abs(yrep), 1, max)
data.frame(observed = max(abs(y)), predictive_median = median(max_margin),
  lower = quantile(max_margin, 0.05), upper = quantile(max_margin, 0.95))


## -----------------------------------------------------------------------------
A_test <- cbind(home = 1 - test$neutral_site, build_design_matrix(test, teams))
eta <- draws[, c("alpha", teams)] %*% t(A_test)
p_home <- colMeans(pnorm(eta / draws[, "sigma"]))
set.seed(546)
future_margin <- eta + matrix(rnorm(length(eta)), nrow = nrow(draws)) * draws[, "sigma"]
forecasts <- test %>% mutate(
  predicted_margin = colMeans(eta), p_home = p_home,
  lower = apply(future_margin, 2, quantile, 0.025),
  upper = apply(future_margin, 2, quantile, 0.975))
forecasts %>% select(week, home_team, away_team, margin, predicted_margin,
  p_home, lower, upper) %>% slice_head(n = 8) %>% knitr::kable(digits = 2)


## -----------------------------------------------------------------------------
decisive <- forecasts %>% filter(tie == 0)
baseline <- mean(train$home_win[train$tie == 0])
data.frame(
  model = c("Training home-win rate", "Bayesian static margin model"),
  log_loss = c(log_loss(decisive$home_win, rep(baseline, nrow(decisive))),
               log_loss(decisive$home_win, decisive$p_home)),
  brier = c(brier_score(decisive$home_win, rep(baseline, nrow(decisive))),
            brier_score(decisive$home_win, decisive$p_home))
) %>% knitr::kable(digits = 4)
data.frame(rmse = sqrt(mean((forecasts$margin - forecasts$predicted_margin)^2)),
  coverage_95 = mean(forecasts$margin >= forecasts$lower & forecasts$margin <= forecasts$upper))


## -----------------------------------------------------------------------------
#| fig-cap: "Week-10 forecasts for later 2023 games. Vertical intervals represent future game variation and parameter uncertainty."
#| fig-alt: "Observed margins against predicted margins with predictive intervals and a diagonal reference line."
ggplot(forecasts, aes(predicted_margin, margin)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = 0.15) +
  geom_point(alpha = 0.6, color = "#0077a8") + geom_abline(slope = 1, intercept = 0) +
  labs(x = "Predicted home margin (points)", y = "Observed home margin (points)") + theme_minimal()

