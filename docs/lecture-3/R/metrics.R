# Probability metrics for Lecture 3.
#
# These functions are deliberately free of any torch dependency so that
# Tutorial 3.6, which only reads precomputed results, can be rendered without
# LibTorch installed. `R/torch_helpers.R` sources this file.

binary_log_loss <- function(observed, probability) {
  probability <- pmin(pmax(probability, 1e-7), 1 - 1e-7)
  -mean(
    observed * log(probability) +
      (1 - observed) * log(1 - probability)
  )
}

brier_score <- function(observed, probability) {
  mean((observed - probability)^2)
}

roc_auc <- function(observed, probability) {
  positive <- observed == 1
  number_positive <- sum(positive)
  number_negative <- sum(!positive)
  probability_ranks <- rank(probability, ties.method = "average")

  (
    sum(probability_ranks[positive]) -
      number_positive * (number_positive + 1) / 2
  ) / (number_positive * number_negative)
}

evaluate_probabilities <- function(observed, probability) {
  data.frame(
    log_loss = binary_log_loss(observed, probability),
    brier = brier_score(observed, probability),
    auc = roc_auc(observed, probability)
  )
}

# Optional expected-score utilities retained for existing analysis scripts.
# These describe one forecast, not a data set; Lecture 2 develops propriety.
expected_brier <- function(reported, truth_probability) {
  truth_probability * (1 - reported)^2 +
    (1 - truth_probability) * reported^2
}

expected_log_loss <- function(reported, truth_probability) {
  reported <- pmin(pmax(reported, 1e-7), 1 - 1e-7)
  -(truth_probability * log(reported) +
      (1 - truth_probability) * log(1 - reported))
}

expected_accuracy <- function(reported, truth_probability, threshold = 0.50) {
  ifelse(reported >= threshold, truth_probability, 1 - truth_probability)
}

calibration_table <- function(observed, probability, bins = 5L) {
  breaks <- unique(
    quantile(
      probability,
      probs = seq(0, 1, length.out = bins + 1),
      na.rm = TRUE
    )
  )

  group <- cut(
    probability,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )

  aggregate(
    cbind(predicted = probability, observed = observed),
    by = list(bin = group),
    FUN = mean
  )
}
