# Generate the figures used by the Lecture 4 slides.
# Run from the Lecture 4 project root:
# Rscript slides/build_slide_images.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(lme4)
})
source("R/player_helpers.R")

dir.create("slides/images", showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 16))
save_plot <- function(plot, name, width = 10, height = 5.4) {
  ggsave(file.path("slides", "images", name), plot, width = width, height = height, dpi = 160, bg = "white")
}

passes <- load_passes()
qbs <- qb_summary(passes)
results <- readRDS("data/bayes_results.rds")
wnba <- load_wnba()

# 1. Funnel plot ---------------------------------------------------------------
league_rate <- mean(passes$complete)
funnel <- data.frame(attempts = 1:max(qbs$attempts)) %>%
  mutate(
    lower = league_rate - 2 * sqrt(league_rate * (1 - league_rate) / attempts),
    upper = league_rate + 2 * sqrt(league_rate * (1 - league_rate) / attempts)
  )
save_plot(
  ggplot(qbs, aes(attempts, completion_rate)) +
    geom_ribbon(data = funnel, aes(x = attempts, ymin = lower, ymax = upper), inherit.aes = FALSE, fill = "#03a9e6", alpha = 0.15) +
    geom_hline(yintercept = league_rate, linetype = "dashed", color = "gray40") +
    geom_point(alpha = 0.75, size = 2) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Pass attempts", y = "Completion rate"),
  "funnel-plot.png"
)

# 2. Shrinkage arrows ----------------------------------------------------------
epa_fit <- lmer(qb_epa ~ 1 + (1 | passer), data = passes)
vc <- as.data.frame(VarCorr(epa_fit))
tau2 <- vc$vcov[vc$grp == "passer"]
sigma2 <- vc$vcov[vc$grp == "Residual"]
beta0 <- fixef(epa_fit)[[1]]
comparison <- passes %>%
  group_by(passer) %>%
  summarize(attempts = n(), epa_mean = mean(qb_epa), .groups = "drop") %>%
  mutate(partial = beta0 + shrinkage_weight(attempts, tau2, sigma2) * (epa_mean - beta0))
save_plot(
  ggplot(comparison, aes(x = attempts)) +
    geom_hline(yintercept = beta0, linetype = "dashed", color = "gray40") +
    geom_segment(aes(xend = attempts, y = epa_mean, yend = partial), arrow = grid::arrow(length = grid::unit(0.05, "inches")), color = "gray60") +
    geom_point(aes(y = epa_mean), shape = 1, size = 2) +
    geom_point(aes(y = partial), color = "#03a9e6", size = 2) +
    scale_x_log10() +
    labs(x = "Attempts (log scale)", y = "EPA per attempt"),
  "shrinkage-arrows.png"
)

# 3. Passer-only against crossed effects ---------------------------------------
with_covariates <- glmer(
  complete ~ air_yards_z + qb_hit + pass_location + receiver_position + (1 | passer),
  data = passes, family = binomial()
)
crossed <- glmer(
  complete ~ air_yards_z + qb_hit + pass_location + receiver_position +
    (1 | passer) + (1 | receiver) + (1 | defteam),
  data = passes, family = binomial()
)
effects <- ranef(with_covariates)$passer %>%
  tibble::rownames_to_column("passer") %>%
  rename(passer_only = `(Intercept)`) %>%
  inner_join(ranef(crossed)$passer %>% tibble::rownames_to_column("passer") %>% rename(crossed = `(Intercept)`), by = "passer") %>%
  inner_join(passes %>% count(passer, name = "attempts") %>% mutate(passer = as.character(passer)), by = "passer") %>%
  filter(attempts >= 100)
save_plot(
  ggplot(effects, aes(passer_only, crossed)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = attempts), color = "#03a9e6", alpha = 0.8) +
    ggrepel::geom_text_repel(aes(label = passer), size = 3.2, max.overlaps = 10) +
    labs(x = "Passer effect, passer-only model", y = "Passer effect, crossed model", size = "Attempts"),
  "crossed-effects.png"
)

vc_crossed <- as.data.frame(VarCorr(crossed)) %>% select(grp, sdcor)
save_plot(
  ggplot(vc_crossed, aes(reorder(grp, sdcor), sdcor)) +
    geom_col(fill = "#03a9e6", width = 0.6) +
    coord_flip() +
    labs(x = NULL, y = "Standard deviation of effects (log-odds of completion)"),
  "variance-components.png", height = 4
)

# 4. Beta-binomial ---------------------------------------------------------------
league <- wnba$league_three %>% filter(athlete_display_name != "Caitlin Clark")
prior <- fit_beta_binomial(league$fg3m, league$fg3a)
clark <- wnba$clark_games
makes <- sum(clark$fg3m)
attempts <- sum(clark$fg3a)
posterior <- beta_binomial_update(prior[["a"]], prior[["b"]], makes, attempts)
theta <- seq(0.15, 0.55, by = 0.001)
likelihood <- dbinom(makes, attempts, theta)
curves <- bind_rows(
  data.frame(theta, density = dbeta(theta, prior[["a"]], prior[["b"]]), curve = "prior (league)"),
  data.frame(theta, density = likelihood / sum(likelihood) / 0.001, curve = "likelihood (scaled)"),
  data.frame(theta, density = dbeta(theta, posterior[["a"]], posterior[["b"]]), curve = "posterior")
)
save_plot(
  ggplot(curves, aes(theta, density, color = curve)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Three-point percentage", y = "Density", color = NULL) +
    theme(legend.position = "bottom"),
  "beta-binomial.png"
)

running <- clark %>%
  mutate(
    a = prior[["a"]] + cumsum(fg3m),
    b = prior[["b"]] + cumsum(fg3a) - cumsum(fg3m),
    mean = a / (a + b), lower = qbeta(0.025, a, b), upper = qbeta(0.975, a, b),
    raw = cumsum(fg3m) / cumsum(fg3a)
  )
save_plot(
  ggplot(running, aes(game)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#03a9e6", alpha = 0.2) +
    geom_line(aes(y = mean), color = "#03a9e6", linewidth = 1.2) +
    geom_line(aes(y = raw), color = "gray40", linetype = "dashed") +
    labs(x = "Game", y = "Three-point percentage", caption = "Dashed: raw cumulative rate"),
  "sequential-updating.png"
)

# 5. Metropolis-Hastings traces ------------------------------------------------
one <- passes %>% filter(passer == "Patrick Mahomes")
X <- cbind(1, one$air_yards_z)
y <- one$complete
log_post <- function(beta) logistic_log_posterior(beta, X, y, prior_sd = 2.5)
chains <- lapply(1:3, function(chain) metropolis_hastings(log_post, c(runif(1, -1, 2), runif(1, -2, 1)), 3000, 0.10, seed = 430 + chain))
traces <- bind_rows(lapply(seq_along(chains), function(c) data.frame(chain = factor(c), iteration = 1:3000, beta1 = chains[[c]]$draws[, 2])))
save_plot(
  ggplot(traces, aes(iteration, beta1, color = chain)) +
    geom_line(alpha = 0.75, linewidth = 0.35) +
    labs(x = "Iteration", y = "Air-yards slope", color = "Chain"),
  "mh-traces.png"
)

# 6. Stan against lme4 -----------------------------------------------------------
qb_posterior <- data.frame(
  passer = colnames(results$stan$u_qb_draws),
  posterior_mean = colMeans(results$stan$u_qb_draws)
) %>%
  left_join(results$lme4$completion_qb_effects %>% tibble::rownames_to_column("passer") %>% rename(lme4 = `(Intercept)`), by = "passer") %>%
  left_join(results$qb_attempts %>% mutate(passer = as.character(passer)), by = "passer")
save_plot(
  ggplot(qb_posterior, aes(lme4, posterior_mean)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(aes(size = attempts), color = "#03a9e6", alpha = 0.7) +
    labs(x = "lme4 estimate (empirical Bayes)", y = "Stan posterior mean (full Bayes)", size = "Attempts"),
  "stan-vs-lme4.png"
)

tau_chains <- results$stan$tau_qb_chains
save_plot(
  data.frame(iteration = rep(seq_len(nrow(tau_chains)), 4), chain = factor(rep(1:4, each = nrow(tau_chains))), tau = as.numeric(tau_chains)) %>%
    ggplot(aes(iteration, tau, color = chain)) +
    geom_line(alpha = 0.7, linewidth = 0.35) +
    labs(x = "Sampling iteration", y = "Passer standard deviation", color = "Chain"),
  "stan-trace.png"
)

# 7. RAPM estimators -------------------------------------------------------------
sim <- simulate_rapm(players = 60, stints = 2500, true_sd = 2, noise_sd = 25, seed = 431)
weights <- sim$possessions / 100
lambda_known <- sim$noise_sd^2 / sim$true_sd^2
estimates <- data.frame(
  truth = sim$true_value,
  raw_plus_minus = raw_plus_minus(sim$X, sim$y, sim$possessions),
  least_squares = ridge_closed_form(sim$X, sim$y, 1e-8, weights),
  ridge = ridge_closed_form(sim$X, sim$y, lambda_known, weights)
) %>%
  tidyr::pivot_longer(-truth, names_to = "estimator", values_to = "estimate") %>%
  mutate(estimator = factor(estimator, levels = c("raw_plus_minus", "least_squares", "ridge")))
save_plot(
  ggplot(estimates, aes(truth, estimate)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(color = "#03a9e6", alpha = 0.8, size = 2) +
    facet_wrap(~estimator, scales = "free_y") +
    labs(x = "True player value", y = "Estimate"),
  "rapm-estimators.png"
)

message("Lecture 4 slide images written to slides/images")
