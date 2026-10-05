# Complete runnable workflow for Tutorial 4.5.
# Run from the Lecture 4 folder: Rscript R/fit_completion_stan.R
# Requires cmdstanr, an installed CmdStan, and a C++ toolchain.
suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(cmdstanr)
  library(rstanarm)
  library(posterior)
})
source("R/player_helpers.R")
source("R/bayesian_player_helpers.R")
options(mc.cores = min(4L, parallel::detectCores()))
input <- prepare_completion(load_passes())
train <- input$train
test <- input$test
stan_data <- completion_stan_data(input)

reference <- glmer(cbind(completions, attempts - completions) ~ 1 + (1 | passer),
                    data = train, family = binomial())
model <- cmdstan_model("R/completion_player.stan")
fit_stan <- model$sample(
  data = stan_data, chains = 4, parallel_chains = 4, seed = 431,
  iter_warmup = 1000, iter_sampling = 1000,
  adapt_delta = 0.95, max_treedepth = 12, refresh = 500)
fit_arm <- stan_glmer(
  cbind(completions, attempts - completions) ~ 1 + (1 | passer),
  data = train, family = binomial(link = "logit"),
  prior_intercept = normal(0, 2.5, autoscale = FALSE),
  prior_covariance = decov(shape = 1, scale = 1),
  chains = 4, iter = 2000, warmup = 1000, seed = 432,
  adapt_delta = 0.95, control = list(max_treedepth = 12), refresh = 500)

draw_matrix <- as.matrix(fit_stan$draws(
  variables = c("alpha", "tau", "theta", "s_rep", "s_test_rep"), format = "matrix"))
stan_draws <- completion_draws(draw_matrix, input$players, nrow(train), nrow(test))
stan_array <- fit_stan$draws(variables = c("alpha", "tau", "theta"), format = "array")
arm_matrix <- as.matrix(fit_arm)
arm_array <- as.array(fit_arm)
sigma_name <- grep("^Sigma\\[", colnames(arm_matrix), value = TRUE)
stopifnot(length(sigma_name) == 1L)
new_players <- data.frame(passer = factor(input$players, levels = input$players),
                          attempts = 1L, completions = 0L)
arm_theta <- posterior_epred(fit_arm, newdata = new_players, re.form = NULL)
colnames(arm_theta) <- input$players
arm_parameters <- cbind(alpha = arm_matrix[, "(Intercept)"],
                         tau = sqrt(arm_matrix[, sigma_name]), arm_theta)
colnames(arm_parameters) <- c("alpha", "tau",
                              paste0("theta[", seq_along(input$players), "]"))
arm_param_array <- array(arm_parameters,
                         dim = c(dim(arm_array)[1:2], ncol(arm_parameters)),
                         dimnames = list(NULL, NULL, colnames(arm_parameters)))
# Prediction uses only the held-out attempt counts and known player identities.
test_predictors <- test
test_predictors$completions <- 0L
arm_rep <- posterior_predict(fit_arm, seed = 433)
arm_test_rep <- posterior_predict(fit_arm, newdata = test_predictors, seed = 434)

results <- list(
  input = input, stan_data = stan_data,
  priors = list(alpha_sd = 2.5, tau_distribution = "Exponential(rate = 1)"),
  cmdstanr = list(draws = stan_draws, summary = summarize_chains(stan_array),
               diagnostics = sampler_checks(fit_stan), tau_chains = stan_array[, , "tau"]),
  rstanarm = list(draws = list(alpha = arm_parameters[, "alpha"],
                              tau = arm_parameters[, "tau"], theta = arm_theta,
                              s_rep = arm_rep, s_test_rep = arm_test_rep),
                  summary = summarize_chains(arm_param_array),
                  diagnostics = sampler_checks(fit_arm),
                  prior_summary = capture.output(prior_summary(fit_arm))),
  lme4 = list(alpha = unname(fixef(reference)[1]),
              tau = unname(attr(VarCorr(reference)$passer, "stddev")),
              theta = setNames(plogis(fixef(reference)[1] +
                                       ranef(reference)$passer[, 1]),
                                rownames(ranef(reference)$passer))),
  cmdstan_version = as.character(cmdstan_version()),
  seeds = c(cmdstanr = 431L, rstanarm = 432L, training_replications = 433L,
            test_replications = 434L),
  versions = sapply(c("cmdstanr", "rstanarm", "lme4", "posterior", "bayesplot"),
                     function(package) as.character(packageVersion(package))),
  session_info = capture.output(sessionInfo()))
saveRDS(results, "data/completion_stan_results.rds", compress = "xz")
print(results$cmdstanr$summary[1:2, ])
print(results$rstanarm$summary[1:2, ])
print(results$cmdstanr$diagnostics)
print(results$rstanarm$diagnostics)
cat(paste(results$rstanarm$prior_summary, collapse = "\n"), "\n")
