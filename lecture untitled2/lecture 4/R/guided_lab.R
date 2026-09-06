## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("R/player_helpers.R")
passes <- load_passes()
train <- passes %>% filter(week <= 10)
test <- passes %>% filter(week > 10)
stats <- train %>% group_by(passer) %>% summarize(
  n = n(), sy = sum(qb_epa), sy2 = sum(qb_epa^2),
  raw = mean(qb_epa), .groups = "drop"
)
stopifnot(length(intersect(unique(train$game_id), unique(test$game_id))) == 0)
stats %>% arrange(desc(n)) %>% slice_head(n = 6) %>% knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
gibbs_players <- function(stats, seed, iterations = 4000, warmup = 1500,
                          tau_rate = 0.04) {
  set.seed(seed)
  J <- nrow(stats)
  theta <- stats$raw
  mu <- rnorm(1, 0, 0.3)
  tau2 <- runif(1, 0.02, 0.15)
  sigma2 <- runif(1, 2, 6)
  out <- matrix(NA_real_, iterations - warmup, J + 3)
  colnames(out) <- c(as.character(stats$passer), "mu", "tau", "sigma")
  for (s in seq_len(iterations)) {
    v <- 1 / (stats$n / sigma2 + 1 / tau2)
    m <- v * (stats$sy / sigma2 + mu / tau2)
    theta <- rnorm(J, m, sqrt(v))
    v_mu <- 1 / (1 + J / tau2)
    mu <- rnorm(1, v_mu * sum(theta) / tau2, sqrt(v_mu))
    tau2 <- 1 / rgamma(1, 2 + J / 2,
                       rate = tau_rate + sum((theta - mu)^2) / 2)
    sse <- sum(stats$sy2 - 2 * theta * stats$sy + stats$n * theta^2)
    sigma2 <- 1 / rgamma(1, 2 + sum(stats$n) / 2, rate = 4 + sse / 2)
    if (s > warmup) out[s - warmup, ] <- c(theta, mu, sqrt(tau2), sqrt(sigma2))
  }
  out
}
chains <- lapply(431:434, function(seed) gibbs_players(stats, seed))
draws <- do.call(rbind, chains)


## -----------------------------------------------------------------------------
draw_array <- array(NA_real_, c(nrow(chains[[1]]), 4, ncol(draws)),
  dimnames = list(NULL, NULL, colnames(draws)))
for (c in 1:4) draw_array[, c, ] <- chains[[c]]
diagnostics <- posterior::summarise_draws(posterior::as_draws_array(draw_array))
diagnostics %>% filter(variable %in% c("mu", "tau", "sigma")) %>%
  select(variable, mean, sd, rhat, ess_bulk, ess_tail) %>% knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
#| fig-cap: "Between-passer standard deviation across four Gibbs chains, after warmup."
#| fig-alt: "Four time series for tau used to check stationarity and agreement between chains."
trace <- bind_rows(lapply(1:4, function(c) data.frame(
  iteration = seq_len(nrow(chains[[c]])), tau = chains[[c]][, "tau"], chain = factor(c))))
ggplot(trace, aes(iteration, tau, color = chain)) + geom_line(alpha = 0.6) +
  facet_wrap(~chain, ncol = 2) + theme_minimal() + theme(legend.position = "none")


## -----------------------------------------------------------------------------
pair <- as.character(stats$passer[order(-stats$n)][1:2])
delta <- draws[, pair[1]] - draws[, pair[2]]
data.frame(
  comparison = paste(pair[1], "minus", pair[2]),
  mean_difference = mean(delta),
  lower_95 = quantile(delta, 0.025), upper_95 = quantile(delta, 0.975),
  probability_positive = mean(delta > 0),
  probability_above_0.05 = mean(delta > 0.05)
) %>% knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
set.seed(435)
theta_A <- draws[, pair[1]]
future_40 <- rnorm(nrow(draws), theta_A, draws[, "sigma"] / sqrt(40))
data.frame(
  quantity = c("Passer mean EPA", "Mean EPA over 40 future attempts"),
  lower_95 = c(quantile(theta_A, 0.025), quantile(future_40, 0.025)),
  upper_95 = c(quantile(theta_A, 0.975), quantile(future_40, 0.975))
) %>% knitr::kable(digits = 3)

heldout <- test %>% group_by(passer) %>% summarize(
  n_test = n(), observed = mean(qb_epa), .groups = "drop") %>%
  filter(passer %in% stats$passer, n_test >= 30)
checks <- bind_rows(lapply(seq_len(nrow(heldout)), function(j) {
  name <- as.character(heldout$passer[j])
  pred <- rnorm(nrow(draws), draws[, name], draws[, "sigma"] / sqrt(heldout$n_test[j]))
  data.frame(passer = name, observed = heldout$observed[j],
    predicted = mean(pred), lower = quantile(pred, 0.025), upper = quantile(pred, 0.975))
}))
checks %>% slice_head(n = 8) %>% knitr::kable(digits = 3)
mean(checks$observed >= checks$lower & checks$observed <= checks$upper)


## -----------------------------------------------------------------------------
sensitivity <- bind_rows(lapply(c(0.01, 0.04, 0.16), function(b) {
  d <- do.call(rbind, lapply(431:434, function(seed)
    gibbs_players(stats, seed, tau_rate = b)))
  difference <- d[, pair[1]] - d[, pair[2]]
  data.frame(prior_mean_tau2 = b, posterior_mean_tau = mean(d[, "tau"]),
    mean_difference = mean(difference), probability_positive = mean(difference > 0))
}))
sensitivity %>% knitr::kable(digits = 3)

