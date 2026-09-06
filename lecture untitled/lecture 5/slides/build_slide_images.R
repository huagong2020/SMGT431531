# Generate the figures used by the Lecture 5 slides.
# Run from the Lecture 5 project root:
# Rscript slides/build_slide_images.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})
source("R/team_helpers.R")

dir.create("slides/images", showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 16))
save_plot <- function(plot, name, width = 10, height = 5.4) {
  ggsave(file.path("slides", "images", name), plot, width = width, height = height, dpi = 160, bg = "white")
}

nfl <- load_nfl_games()
epl <- load_epl_matches()
games_2023 <- nfl %>% filter(season == 2023)
team_results <- readRDS("data/team_results.rds")
poisson <- team_results$poisson
state_space <- team_results$state_space

# 1. Pythagorean -----------------------------------------------------------------
standings <- team_season_table(games_2023) %>%
  mutate(pythagorean = pythagorean(points_for, points_against, 2.37))
save_plot(
  ggplot(standings, aes(pythagorean, win_pct)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(color = "#03a9e6", size = 2.5) +
    ggrepel::geom_text_repel(aes(label = team), size = 3.4) +
    labs(x = "Pythagorean expectation", y = "Actual win percentage"),
  "pythagorean.png"
)

# 2. Ridge against least squares -------------------------------------------------
massey <- massey_ratings(games_2023)
ridge <- ridge_ratings(games_2023, lambda = 7.4)
comparison <- data.frame(team = names(massey$ratings), least_squares = massey$ratings, ridge = ridge$ratings)
save_plot(
  ggplot(comparison, aes(least_squares, ridge)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(color = "#03a9e6", size = 2.5) +
    ggrepel::geom_text_repel(aes(label = team), size = 3.2) +
    coord_equal() +
    labs(x = "Least-squares rating (points)", y = "Ridge rating (points)"),
  "ridge-vs-least-squares.png", width = 8, height = 6
)

# 3. Posterior intervals from rstanarm --------------------------------------------
suppressPackageStartupMessages(library(rstanarm))
X <- build_design_matrix(games_2023)
home <- 1 - games_2023$neutral_site
design <- data.frame(margin = games_2023$margin, home = home, X)
bayes_fit <- stan_glm(
  margin ~ 0 + ., data = design,
  prior = normal(0, c(3, rep(4.7, ncol(X)))), prior_aux = exponential(1 / 13),
  chains = 4, iter = 2000, seed = 431, refresh = 0
)
draws <- as.matrix(bayes_fit)[, colnames(X)]
draws <- draws - rowMeans(draws)
posterior_table <- data.frame(
  team = colnames(draws), mean = colMeans(draws),
  lower = apply(draws, 2, quantile, 0.05), upper = apply(draws, 2, quantile, 0.95)
)
save_plot(
  posterior_table %>%
    mutate(team = reorder(team, mean)) %>%
    ggplot(aes(mean, team)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, color = "gray60") +
    geom_point(color = "#03a9e6", size = 2) +
    labs(x = "Rating (points against an average opponent), 90% interval", y = NULL) +
    theme(axis.text.y = element_text(size = 9)),
  "posterior-ratings.png", width = 8, height = 6.2
)

# 4. Bradley-Terry against margin ratings ------------------------------------------
decided <- games_2023 %>% filter(tie == 0)
Xd <- build_design_matrix(decided)
bt_design <- data.frame(home_win = decided$home_win, home = 1 - decided$neutral_site, Xd[, -1])
bt_fit <- glm(home_win ~ 0 + ., data = bt_design, family = binomial())
bt <- c(0, coef(bt_fit)[-1])
names(bt) <- colnames(Xd)
bt <- bt - mean(bt)
both <- data.frame(team = names(bt), bradley_terry = bt, margin = massey$ratings[names(bt)])
save_plot(
  ggplot(both, aes(margin, bradley_terry)) +
    geom_smooth(method = "lm", se = FALSE, color = "gray60", linewidth = 0.6) +
    geom_point(color = "#03a9e6", size = 2.5) +
    ggrepel::geom_text_repel(aes(label = team), size = 3.2) +
    labs(x = "Margin rating (points)", y = "Bradley-Terry rating (log-odds)"),
  "bt-vs-margin.png"
)

# 5. Attack and defense effects ----------------------------------------------------
team_effects <- data.frame(
  team = colnames(poisson$attack_draws),
  attack = colMeans(poisson$attack_draws),
  defense = colMeans(poisson$defense_draws)
)
save_plot(
  ggplot(team_effects, aes(attack, defense)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
    geom_point(color = "#03a9e6", size = 2.5) +
    ggrepel::geom_text_repel(aes(label = team), size = 3.4) +
    labs(x = "Attack effect (log scale)", y = "Defense effect (log scale; lower is better)"),
  "attack-defense.png"
)

# 6. Posterior predictive check on goals -------------------------------------------
goals_long <- poisson$data
replications <- poisson$replications
goal_check <- bind_rows(lapply(0:7, function(g) {
  replicated <- rowMeans(replications == g)
  data.frame(goals = g, observed = mean(goals_long$goals == g), predicted = mean(replicated),
             lower = quantile(replicated, 0.05), upper = quantile(replicated, 0.95))
}))
save_plot(
  ggplot(goal_check, aes(goals)) +
    geom_col(aes(y = observed), fill = "#03a9e6", alpha = 0.6) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    geom_point(aes(y = predicted), size = 2.5) +
    labs(x = "Goals by one team in a match", y = "Proportion of team-matches", caption = "Bars: observed. Points and bars: posterior predictive 90% interval."),
  "ppc-goals.png"
)

# 7. Elo trajectories ----------------------------------------------------------------
elo <- run_elo(nfl, k = 20, home_advantage = 55, season_regression = 1 / 3)
selected <- c("KC", "SF", "DET", "CAR")
trajectories <- bind_rows(
  elo$games %>% transmute(gameday, team = home_team, elo = home_elo),
  elo$games %>% transmute(gameday, team = away_team, elo = away_elo)
) %>%
  filter(team %in% selected) %>%
  arrange(team, gameday)
save_plot(
  ggplot(trajectories, aes(gameday, elo, color = team)) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 1500, linetype = "dashed", color = "gray60") +
    labs(x = NULL, y = "Elo rating", color = NULL),
  "elo-trajectories.png"
)

# 8. Kalman filter bands ---------------------------------------------------------------
hyper <- state_space$hyper_summary
get <- function(name) hyper$mean[hyper$variable == name]
kalman <- kalman_ratings(
  nfl, sigma_obs = get("sigma_obs"), sigma_week = get("sigma_week"), sigma_season = get("sigma_season"),
  rho = get("rho"), home_advantage = get("h"), initial_sd = get("sigma_initial")
)
save_plot(
  kalman$filtered %>%
    filter(team %in% selected) %>%
    mutate(time = season + (week - 1) / 18) %>%
    ggplot(aes(time, mean, color = team, fill = team)) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
    labs(x = "Season", y = "Filtered strength (points)", color = NULL, fill = NULL),
  "kalman-bands.png"
)

# 9. Calibration of forecasts ------------------------------------------------------------
forecasts <- nfl %>%
  select(game_id, season, home_win, tie, home_moneyline, away_moneyline) %>%
  left_join(elo$games %>% select(game_id, elo_prob = home_prob), by = "game_id") %>%
  left_join(kalman$predictions %>% select(game_id, kalman_prob = home_prob), by = "game_id") %>%
  mutate(market_prob = remove_vig(moneyline_to_probability(home_moneyline), moneyline_to_probability(away_moneyline))[, 1]) %>%
  filter(season >= 2023, tie == 0, !is.na(market_prob))
calibration <- bind_rows(
  forecasts %>% transmute(model = "Elo", probability = elo_prob, home_win),
  forecasts %>% transmute(model = "Kalman filter", probability = kalman_prob, home_win),
  forecasts %>% transmute(model = "market", probability = market_prob, home_win)
) %>%
  mutate(bin = cut(probability, seq(0, 1, 0.1), include.lowest = TRUE)) %>%
  group_by(model, bin) %>%
  summarize(mean_forecast = mean(probability), observed = mean(home_win), games = n(), .groups = "drop") %>%
  filter(games >= 10)
save_plot(
  ggplot(calibration, aes(mean_forecast, observed, color = model)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = games)) +
    geom_line() +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    labs(x = "Mean forecast probability", y = "Observed home-win rate", color = NULL, size = "Games"),
  "calibration.png", width = 8, height = 6
)

# 10. Rank distributions -----------------------------------------------------------------
draws_final <- state_space$final_theta_draws
ranks <- t(apply(-draws_final, 1, rank, ties.method = "first"))
colnames(ranks) <- colnames(draws_final)
top <- names(sort(colMeans(draws_final), decreasing = TRUE))[1:6]
save_plot(
  as.data.frame(ranks[, top]) %>%
    tidyr::pivot_longer(everything(), names_to = "team", values_to = "rank") %>%
    mutate(team = factor(team, levels = top)) %>%
    ggplot(aes(rank)) +
    geom_bar(fill = "#03a9e6") +
    facet_wrap(~team, ncol = 3) +
    coord_cartesian(xlim = c(1, 12)) +
    labs(x = "Rank at the end of 2024", y = "Posterior draws"),
  "rank-distributions.png"
)

message("Lecture 5 slide images written to slides/images")
