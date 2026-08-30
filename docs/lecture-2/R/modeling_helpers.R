library(dplyr)

wp_predictors <- c(
  "game_seconds_remaining",
  "home_score_diff",
  "home_possession",
  "down",
  "ydstogo",
  "yardline_100",
  "home_timeouts",
  "away_timeouts"
)

load_wp_data <- function(path = file.path("data", "nfl_wp_states.rds")) {
  readRDS(path)
}

make_game_split <- function(data, validation_fraction = 0.20, seed = 431) {
  stopifnot(validation_fraction > 0, validation_fraction < 1)

  set.seed(seed)
  games <- unique(data$game_id)
  validation_games <- sample(
    games,
    size = ceiling(length(games) * validation_fraction)
  )

  list(
    analysis = data %>% filter(!game_id %in% validation_games),
    validation = data %>% filter(game_id %in% validation_games)
  )
}

clip_probability <- function(probability, epsilon = 1e-7) {
  pmin(pmax(probability, epsilon), 1 - epsilon)
}

brier_score <- function(truth, probability) {
  mean((truth - probability)^2)
}

log_loss <- function(truth, probability) {
  probability <- clip_probability(probability)
  -mean(
    truth * log(probability) +
      (1 - truth) * log(1 - probability)
  )
}

classification_accuracy <- function(truth, probability, threshold = 0.50) {
  mean((probability >= threshold) == truth)
}

probability_metrics <- function(truth, probability, model) {
  tibble(
    model = model,
    brier = brier_score(truth, probability),
    log_loss = log_loss(truth, probability),
    accuracy = classification_accuracy(truth, probability)
  )
}

# Expected score of *reporting* p when the true probability is q. These
# functions describe one forecast, not a data set, and are used in Tutorial 2.6
# to show which scoring rules are minimized by honest reporting.
expected_brier <- function(reported, truth_probability) {
  truth_probability * (1 - reported)^2 +
    (1 - truth_probability) * reported^2
}

expected_log_loss <- function(reported, truth_probability) {
  reported <- clip_probability(reported)
  -(truth_probability * log(reported) +
      (1 - truth_probability) * log(1 - reported))
}

expected_accuracy <- function(reported, truth_probability, threshold = 0.50) {
  ifelse(reported >= threshold, truth_probability, 1 - truth_probability)
}

calibration_table <- function(truth, probability, bins = 10) {
  tibble(truth = truth, probability = probability) %>%
    mutate(
      probability_bin = cut(
        probability,
        breaks = seq(0, 1, length.out = bins + 1),
        include.lowest = TRUE
      )
    ) %>%
    group_by(probability_bin) %>%
    summarize(
      states = n(),
      mean_prediction = mean(probability),
      observed_win_rate = mean(truth),
      .groups = "drop"
    )
}

model_matrix <- function(data) {
  model.matrix(
    ~ game_seconds_remaining + home_score_diff + home_possession +
      down + ydstogo + yardline_100 + home_timeouts + away_timeouts - 1,
    data = data
  )
}
