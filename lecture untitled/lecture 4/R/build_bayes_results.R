# Precompute the slow model fits for Lecture 4 and save reusable results.
# Run from the Lecture 4 project root:
# Rscript R/build_bayes_results.R
#
# Requires lme4, cmdstanr (with CmdStan installed), and posterior.

suppressPackageStartupMessages({
  library(dplyr)
  library(lme4)
  library(cmdstanr)
  library(posterior)
})
source("R/player_helpers.R")

set.seed(431)
passes <- load_passes()
started <- proc.time()[["elapsed"]]

# ---------------------------------------------------------------------------
# 1. lme4 reference fits
# ---------------------------------------------------------------------------
epa_crossed <- lmer(
  qb_epa ~ air_yards_z + qb_hit + pass_location + receiver_position +
    (1 | passer) + (1 | receiver) + (1 | defteam),
  data = passes,
  REML = TRUE
)

completion_crossed <- glmer(
  complete ~ air_yards_z + qb_hit + pass_location + receiver_position +
    (1 | passer) + (1 | receiver) + (1 | defteam),
  data = passes,
  family = binomial()
)

# ---------------------------------------------------------------------------
# 2. Parametric bootstrap for the EPA model (variance components and passer
#    effects). Each refit takes well under a second, so 200 replicates is
#    affordable.
# ---------------------------------------------------------------------------
bootstrap_statistic <- function(fit) {
  sds <- as.data.frame(VarCorr(fit))$sdcor
  names(sds) <- c(as.data.frame(VarCorr(fit))$grp)
  qb_effects <- ranef(fit)$passer[, 1]
  names(qb_effects) <- rownames(ranef(fit)$passer)
  c(sds, qb_effects)
}

boot_started <- proc.time()[["elapsed"]]
epa_bootstrap <- bootMer(
  epa_crossed,
  FUN = bootstrap_statistic,
  nsim = 200,
  seed = 431,
  type = "parametric",
  use.u = FALSE
)
boot_seconds <- proc.time()[["elapsed"]] - boot_started

# ---------------------------------------------------------------------------
# 3. Stan: fully Bayesian version of the completion model
# ---------------------------------------------------------------------------
X <- model.matrix(
  ~ air_yards_z + qb_hit + pass_location + receiver_position,
  data = passes
)[, -1]

stan_data <- list(
  N = nrow(passes),
  K = ncol(X),
  J_qb = nlevels(passes$passer),
  J_rec = nlevels(passes$receiver),
  J_def = nlevels(passes$defteam),
  X = X,
  y = passes$complete,
  qb = as.integer(passes$passer),
  rec = as.integer(passes$receiver),
  def = as.integer(passes$defteam)
)

model <- cmdstan_model("R/completion_multilevel.stan")

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

draws <- fit$draws(format = "draws_matrix")
hyper_summary <- fit$summary(
  variables = c("alpha", "beta", "tau_qb", "tau_rec", "tau_def")
) %>%
  as.data.frame()

u_qb_draws <- as.matrix(draws[, grep("^u_qb\\[", colnames(draws))])
colnames(u_qb_draws) <- levels(passes$passer)
u_def_draws <- as.matrix(draws[, grep("^u_def\\[", colnames(draws))])
colnames(u_def_draws) <- levels(passes$defteam)
tau_draws <- as.matrix(draws[, c("tau_qb", "tau_rec", "tau_def")])

# Chains kept separate for R-hat teaching examples.
tau_qb_chains <- matrix(
  as.numeric(fit$draws(variables = "tau_qb")),
  ncol = 4
)

sampler <- fit$sampler_diagnostics(format = "draws_matrix")
diagnostics <- data.frame(
  divergent_transitions = sum(sampler[, "divergent__"]),
  max_treedepth_hits = sum(sampler[, "treedepth__"] >= 10),
  seconds = stan_seconds,
  chains = 4,
  iterations_per_chain = 1000
)

bayes_results <- list(
  lme4 = list(
    epa_varcorr = as.data.frame(VarCorr(epa_crossed)),
    epa_fixef = fixef(epa_crossed),
    epa_qb_effects = ranef(epa_crossed, condVar = TRUE)$passer,
    completion_varcorr = as.data.frame(VarCorr(completion_crossed)),
    completion_fixef = fixef(completion_crossed),
    completion_qb_effects = ranef(completion_crossed, condVar = TRUE)$passer
  ),
  bootstrap = list(
    statistics = epa_bootstrap$t,
    observed = epa_bootstrap$t0,
    seconds = boot_seconds
  ),
  stan = list(
    hyper_summary = hyper_summary,
    u_qb_draws = u_qb_draws,
    u_def_draws = u_def_draws,
    tau_draws = tau_draws,
    tau_qb_chains = tau_qb_chains,
    diagnostics = diagnostics,
    cmdstan_version = cmdstan_version()
  ),
  qb_attempts = passes %>% count(passer, name = "attempts"),
  total_seconds = proc.time()[["elapsed"]] - started
)

saveRDS(bayes_results, "data/bayes_results.rds", compress = "xz")
print(hyper_summary)
print(diagnostics)
message(sprintf("Saved data/bayes_results.rds in %.0f seconds", bayes_results$total_seconds))
