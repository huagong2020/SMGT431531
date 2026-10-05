# Rebuild the data figures used in the revised Lecture 4 slides.
# Run from the Lecture 4 folder after R/fit_completion_stan.R.
suppressPackageStartupMessages(library(ggplot2))
source("R/bayesian_player_helpers.R")
results <- readRDS("data/completion_stan_results.rds")
train <- results$input$train
test <- results$input$test
post <- results$cmdstanr$draws
dir.create("slides/images", recursive = TRUE, showWarnings = FALSE)
slide_theme <- theme_minimal(base_size = 22) +
  theme(panel.grid.minor = element_blank(), plot.background = element_rect(fill = "#fbfdff", color = NA))
tau_chains <- results$cmdstanr$tau_chains
trace <- data.frame(iteration = rep(seq_len(nrow(tau_chains)), ncol(tau_chains)),
                     chain = factor(rep(seq_len(ncol(tau_chains)), each = nrow(tau_chains))),
                     tau = as.numeric(tau_chains))
p <- ggplot(trace, aes(iteration, tau)) + geom_line(color = "#0077a8", linewidth = 0.3) +
  facet_wrap(~chain, ncol = 2) + slide_theme +
  labs(x = "Retained iteration", y = "Player SD (log odds)")
ggsave("slides/images/player-stan-trace.png", p, width = 12, height = 5.6, dpi = 160)

row <- which(test$passer == "Patrick Mahomes")[1]
predicted <- post$s_test_rep[, row]
p <- ggplot(data.frame(completions = predicted), aes(completions)) +
  geom_histogram(binwidth = 1, boundary = -0.5, fill = "#03a9e6") +
  geom_vline(xintercept = test$completions[row], color = "#a12835", linewidth = 1.3) +
  slide_theme + labs(x = "Completions in the held-out game", y = "Predictive draws")
ggsave("slides/images/player-heldout-predictive.png", p, width = 12, height = 5.6, dpi = 160)

rows <- which(train$passer == "Patrick Mahomes")
observed_T <- game_dispersion(train$completions[rows], train$attempts[rows])
replicated_T <- game_dispersion(post$s_rep[, rows, drop = FALSE], train$attempts[rows])
p <- ggplot(data.frame(T = replicated_T), aes(T)) +
  geom_histogram(bins = 35, fill = "#03a9e6") +
  geom_vline(xintercept = observed_T, color = "#a12835", linewidth = 1.3) +
  slide_theme + labs(x = "Variance across training-game completion rates", y = "Replicated datasets")
ggsave("slides/images/player-bayesian-pvalue.png", p, width = 12, height = 5.6, dpi = 160)

print(test[row, ])
print(c(predictive_mean = mean(predicted), quantile(predicted, c(.025, .975)),
        observed = test$completions[row], heldout_upper_tail = mean(predicted >= test$completions[row])))
print(c(T_observed = observed_T, T_replicated_mean = mean(replicated_T),
        p_B = mean(replicated_T >= observed_T)))
print(data.frame(sample = c("train", "test"),
                  games = c(nrow(train), nrow(test)),
                  attempts = c(sum(train$attempts), sum(test$attempts)),
                  players = c(length(results$input$players), length(unique(test$passer)))))
cat("Largest R-hat:", max(results$cmdstanr$summary$rhat),
    max(results$rstanarm$summary$rhat), "\n")
