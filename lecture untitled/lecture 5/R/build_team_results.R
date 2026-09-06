# Precompute the slow Bayesian fits for Lecture 5 and save reusable results.
# Run from the Lecture 5 project root:
# Rscript R/build_team_results.R
#
# Requires rstanarm, cmdstanr (with CmdStan installed), and posterior.

suppressPackageStartupMessages({
  library(dplyr)
  library(rstanarm)
  library(cmdstanr)
  library(posterior)
})
source("R/team_helpers.R")

set.seed(431)
started <- proc.time()[["elapsed"]]
nfl <- load_nfl_games()
epl <- load_epl_matches()

# ---------------------------------------------------------------------------
# 1. Bayesian multilevel Poisson model for Premier League goals, 2023-24
# ---------------------------------------------------------------------------
epl_2324 <- epl %>% filter(season == "2023-24")

goals_long <- bind_rows(
  epl_2324 %>%
    transmute(match_number, attack = home_team, defense = away_team,
              home = 1L, goals = home_goals),
  epl_2324 %>%
    transmute(match_number, attack = away_team, defense = home_team,
              home = 0L, goals = away_goals)
) %>%
  mutate(attack = factor(attack), defense = factor(defense, levels = levels(attack)))

poisson_started <- proc.time()[["elapsed"]]
poisson_fit <- stan_glmer(
  goals ~ home + (1 | attack) + (1 | defense),
  data = goals_long,
  family = poisson(),
  prior_intercept = normal(0, 1),
  prior = normal(0, 1),
  prior_covariance = decov(scale = 0.5),
  chains = 4,
  iter = 2000,
  seed = 431,
  refresh = 0
)
poisson_seconds <- proc.time()[["elapsed"]] - poisson_started

poisson_draws <- as.matrix(poisson_fit)
attack_columns <- grep("^b\\[\\(Intercept\\) attack:", colnames(poisson_draws))
defense_columns <- grep("^b\\[\\(Intercept\\) defense:", colnames(poisson_draws))
attack_draws <- poisson_draws[, attack_columns]
defense_draws <- poisson_draws[, defense_columns]
colnames(attack_draws) <- sub("^b\\[\\(Intercept\\) attack:(.*)\\]$", "\\1", colnames(attack_draws))
colnames(defense_draws) <- sub("^b\\[\\(Intercept\\) defense:(.*)\\]$", "\\1", colnames(defense_draws))
colnames(attack_draws) <- gsub("_", " ", colnames(attack_draws))
colnames(defense_draws) <- gsub("_", " ", colnames(defense_draws))

# Posterior predictive replications of the full season, used for checks.
posterior_replications <- posterior_predict(poisson_fit, draws = 500)

poisson_results <- list(
  data = goals_long,
  intercept_draws = poisson_draws[, "(Intercept)"],
  home_draws = poisson_draws[, "home"],
  attack_draws = attack_draws,
  defense_draws = defense_draws,
  sigma_draws = poisson_draws[, grep("^Sigma\\[", colnames(poisson_draws))],
  summary = as.data.frame(summary(poisson_fit, pars = c("(Intercept)", "home"), digits = 3)),
  replications = posterior_replications,
  prior_summary = capture.output(prior_summary(poisson_fit)),
  seconds = poisson_seconds
)

# ---------------------------------------------------------------------------
# 2. Stan state-space model for NFL margins, 2022-2024
# ---------------------------------------------------------------------------
teams <- sort(unique(c(nfl$home_team, nfl$away_team)))
periods <- nfl %>% distinct(season, week) %>% arrange(season, week) %>%
  mutate(period = row_number(), season_start = as.integer(week == min(week)))
nfl_indexed <- nfl %>% left_join(periods, by = c("season", "week"))

stan_data <- list(
  N = nrow(nfl_indexed),
  J = length(teams),
  T = nrow(periods),
  home = match(nfl_indexed$home_team, teams),
  away = match(nfl_indexed$away_team, teams),
  period = nfl_indexed$period,
  margin = nfl_indexed$margin,
  home_field = 1 - nfl_indexed$neutral_site,
  season_start = periods$season_start
)
stan_data$season_start[[1]] <- 0L

model <- cmdstan_model("R/dynamic_ratings.stan")
stan_started <- proc.time()[["elapsed"]]
fit <- model$sample(
  data = stan_data,
  seed = 431,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  refresh = 200
)
stan_seconds <- proc.time()[["elapsed"]] - stan_started

hyper_summary <- fit$summary(
  variables = c("sigma_obs", "sigma_week", "sigma_season", "sigma_initial", "rho", "h")
) %>% as.data.frame()

theta_summary <- fit$summary(variables = "theta", mean, sd,
                             ~quantile(.x, probs = c(0.05, 0.95))) %>%
  as.data.frame() %>%
  mutate(
    team_index = as.integer(sub("theta\\[(\\d+),(\\d+)\\]", "\\1", variable)),
    period = as.integer(sub("theta\\[(\\d+),(\\d+)\\]", "\\2", variable)),
    team = teams[team_index]
  ) %>%
  left_join(periods %>% select(period, season, week), by = "period") %>%
  select(team, season, week, period, mean, sd, lower = `5%`, upper = `95%`)

final_theta_draws <- as.matrix(fit$draws(
  variables = sprintf("theta[%d,%d]", seq_along(teams), nrow(periods)),
  format = "draws_matrix"
))
colnames(final_theta_draws) <- teams

sampler <- fit$sampler_diagnostics(format = "draws_matrix")
state_space_results <- list(
  teams = teams,
  periods = periods,
  hyper_summary = hyper_summary,
  theta_summary = theta_summary,
  final_theta_draws = final_theta_draws,
  hyper_draws = as.matrix(fit$draws(
    variables = c("sigma_obs", "sigma_week", "sigma_season", "rho", "h"),
    format = "draws_matrix"
  )),
  diagnostics = data.frame(
    divergent_transitions = sum(sampler[, "divergent__"]),
    max_treedepth_hits = sum(sampler[, "treedepth__"] >= 10),
    seconds = stan_seconds
  ),
  cmdstan_version = cmdstan_version()
)

team_results <- list(
  poisson = poisson_results,
  state_space = state_space_results,
  total_seconds = proc.time()[["elapsed"]] - started
)

saveRDS(team_results, "data/team_results.rds", compress = "xz")
print(poisson_results$summary)
print(hyper_summary)
print(state_space_results$diagnostics)
message(sprintf("Saved data/team_results.rds in %.0f seconds", team_results$total_seconds))
