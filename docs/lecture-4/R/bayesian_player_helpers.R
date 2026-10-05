# Run from the Lecture 4 folder. Helpers for Tutorials 4.4 and 4.5.

gibbs_epa <- function(stats, seed, iterations = 4000, warmup = 1500) {
  stopifnot(iterations > warmup, all(stats$n > 0))
  set.seed(seed)
  J <- nrow(stats)
  theta <- stats$raw
  mu <- rnorm(1, 0, 0.3)
  tau2 <- runif(1, 0.02, 0.15)
  sigma2 <- runif(1, 2, 6)
  out <- matrix(NA_real_, iterations - warmup, J + 3,
                dimnames = list(NULL, c(as.character(stats$passer),
                                      "mu", "tau", "sigma")))
  for (iteration in seq_len(iterations)) {
    v <- 1 / (stats$n / sigma2 + 1 / tau2)
    m <- v * (stats$sy / sigma2 + mu / tau2)
    theta <- rnorm(J, m, sqrt(v))
    v_mu <- 1 / (1 + J / tau2)
    mu <- rnorm(1, v_mu * sum(theta) / tau2, sqrt(v_mu))
    tau2 <- 1 / rgamma(1, 2 + J / 2,
                       rate = 0.04 + sum((theta - mu)^2) / 2)
    sse <- sum(stats$sy2 - 2 * theta * stats$sy + stats$n * theta^2)
    sigma2 <- 1 / rgamma(1, 2 + sum(stats$n) / 2,
                         rate = 4 + max(sse, 0) / 2)
    if (iteration > warmup)
      out[iteration - warmup, ] <- c(theta, mu, sqrt(tau2), sqrt(sigma2))
  }
  out
}

prepare_completion <- function(passes, cutoff = 10L) {
  games <- passes |>
    dplyr::mutate(passer = as.character(passer)) |>
    dplyr::group_by(game_id, week, passer) |>
    dplyr::summarize(attempts = dplyr::n(), completions = sum(complete),
                     .groups = "drop") |>
    dplyr::arrange(week, game_id, passer)
  train <- dplyr::filter(games, week <= cutoff)
  players <- sort(unique(train$passer))
  train$passer <- factor(train$passer, levels = players)
  test <- dplyr::filter(games, week > cutoff, passer %in% players)
  test$passer <- factor(test$passer, levels = players)
  stopifnot(length(intersect(train$game_id, test$game_id)) == 0,
            all(train$completions <= train$attempts),
            all(test$completions <= test$attempts))
  list(train = train, test = test, players = players, cutoff = cutoff)
}

completion_stan_data <- function(input) {
  list(J = length(input$players), G = nrow(input$train),
       n = input$train$attempts, s = input$train$completions,
       qb = as.integer(input$train$passer), G_test = nrow(input$test),
       n_test = input$test$attempts, qb_test = as.integer(input$test$passer))
}

summarize_chains <- function(chains) {
  as.data.frame(posterior::summarise_draws(
    posterior::as_draws_array(chains), "mean", "sd", "mcse_mean",
    "rhat", "ess_bulk", "ess_tail"))
}

completion_draws <- function(draw_matrix, players, G, G_test) {
  # Explicit numeric indices preserve the player and game mapping.
  theta <- draw_matrix[, paste0("theta[", seq_along(players), "]"), drop = FALSE]
  colnames(theta) <- players
  list(alpha = draw_matrix[, "alpha"], tau = draw_matrix[, "tau"],
       theta = theta,
       s_rep = draw_matrix[, paste0("s_rep[", seq_len(G), "]"), drop = FALSE],
       s_test_rep = draw_matrix[, paste0("s_test_rep[", seq_len(G_test), "]"), drop = FALSE])
}

sampler_checks <- function(fit, max_depth = 12L) {
  # bayesplot supports both CmdStanMCMC and rstanarm stanreg objects.
  sampler <- bayesplot::nuts_params(fit)
  data.frame(divergences = sum(sampler$Value[sampler$Parameter == "divergent__"]),
             treedepth_hits = sum(sampler$Value[sampler$Parameter == "treedepth__"] >= max_depth))
}

game_dispersion <- function(counts, attempts) {
  # Variance across game completion rates. Replications retain attempt counts.
  if (is.null(dim(counts))) return(var(counts / attempts))
  apply(sweep(counts, 2, attempts, "/"), 1, var)
}
