# Train the small teaching models and save reusable results for the website.
# The benchmark settings are fixed in advance. Test diagnostics below describe
# these locked models and must not guide further tuning on this test set.
# Run from the Lecture 3 project root.

library(dplyr)
source("https://raw.githubusercontent.com/huagong2020/SMGT431531/refs/heads/main/docs/lecture-3/R/torch_helpers.R")

dir.create("data", showWarnings = FALSE, recursive = TRUE)
data_url <- "https://raw.githubusercontent.com/huagong2020/SMGT431531/main/docs/lecture-3/data/soccer_shots.rds"
temp <- tempfile() # create a tempfile
download.file(data_url, temp) # download to disk

teaching_data <- readRDS(temp) # load soccer_shots.rds
tensors <- make_tensors(teaching_data)
standardized_features <- standardize_tabular(teaching_data)

feature_data <- as.data.frame(standardized_features)
names(feature_data) <- teaching_data$feature_names
feature_data$goal <- teaching_data$target

logistic_started_at <- proc.time()[["elapsed"]]
logistic_fit <- glm(
  goal ~ .,
  data = feature_data[tensors$train, ],
  family = binomial()
)
logistic_probability <- predict(
  logistic_fit,
  newdata = feature_data[tensors$test, ],
  type = "response"
)
logistic_seconds <- proc.time()[["elapsed"]] - logistic_started_at

torch_manual_seed(431)
mlp_fit <- fit_binary_model(
  model = mlp_module(length(teaching_data$feature_names)),
  x = tensors$tabular,
  y = tensors$target,
  training_indices = tensors$train,
  validation_indices = tensors$validation,
  epochs = 30,
  batch_size = 64,
  learning_rate = 0.01,
  patience = 5,
  seed = 431
)
mlp_probability <- predict_probability(
  mlp_fit$model,
  tensors$tabular,
  tensors$test
)

torch_manual_seed(431)
cnn_fit <- fit_binary_model(
  model = cnn_module(length(teaching_data$channel_names)),
  x = tensors$spatial,
  y = tensors$target,
  training_indices = tensors$train,
  validation_indices = tensors$validation,
  epochs = 16,
  batch_size = 64,
  learning_rate = 0.003,
  patience = 4,
  seed = 431
)
cnn_probability <- predict_probability(
  cnn_fit$model,
  tensors$spatial,
  tensors$test
)

observed_test <- teaching_data$target[tensors$test]
predictions <- data.frame(
  row = tensors$test,
  observed = observed_test,
  logistic = as.numeric(logistic_probability),
  mlp = mlp_probability,
  cnn = cnn_probability
)

metrics <- bind_rows(
  evaluate_probabilities(observed_test, predictions$logistic) %>%
    mutate(model = "Logistic regression"),
  evaluate_probabilities(observed_test, predictions$mlp) %>%
    mutate(model = "Multilayer perceptron"),
  evaluate_probabilities(observed_test, predictions$cnn) %>%
    mutate(model = "Two-dimensional CNN")
) %>%
  select(model, everything())

# A deliberately small validation-only learning-rate comparison.
tuning_results <- lapply(c(0.001, 0.005, 0.01), function(rate) {
  torch_manual_seed(431)
  candidate <- fit_binary_model(
    model = mlp_module(length(teaching_data$feature_names)),
    x = tensors$tabular,
    y = tensors$target,
    training_indices = tensors$train,
    validation_indices = tensors$validation,
    epochs = 12,
    batch_size = 64,
    learning_rate = rate,
    patience = 3,
    seed = 431
  )

  data.frame(
    learning_rate = rate,
    best_epoch = candidate$best_epoch,
    validation_log_loss = candidate$best_validation_loss,
    elapsed_seconds = candidate$elapsed_seconds
  )
}) %>%
  bind_rows()

# Permutation importance: increase in test log loss after shuffling one feature.
set.seed(431)
baseline_mlp_loss <- binary_log_loss(observed_test, mlp_probability)
permutation_importance <- lapply(
  seq_along(teaching_data$feature_names),
  function(feature_number) {
    permuted_features <- standardized_features[tensors$test, , drop = FALSE]
    permuted_features[, feature_number] <- sample(
      permuted_features[, feature_number]
    )
    permuted_tensor <- torch_tensor(
      permuted_features,
      dtype = torch_float()
    )
    permuted_probability <- predict_probability(
      mlp_fit$model,
      permuted_tensor,
      seq_len(nrow(permuted_features))
    )

    data.frame(
      feature = teaching_data$feature_names[[feature_number]],
      increase_in_log_loss = binary_log_loss(
        observed_test,
        permuted_probability
      ) - baseline_mlp_loss
    )
  }
) %>%
  bind_rows() %>%
  arrange(desc(increase_in_log_loss))

# CNN occlusion map for the test shot receiving the largest CNN probability.
selected_test_position <- which.max(cnn_probability)
selected_row <- tensors$test[[selected_test_position]]
selected_grid <- teaching_data$spatial[selected_row, , , , drop = FALSE]
patch_rows <- 8L
patch_columns <- 12L
number_of_patches <- patch_rows * patch_columns
occluded_grids <- array(
  0,
  dim = c(number_of_patches, 4L, 16L, 24L)
)

for (patch_number in seq_len(number_of_patches)) {
  occluded_grids[patch_number, , , ] <- selected_grid[1, , , ]
}

patch_number <- 0L
for (patch_row in seq_len(patch_rows)) {
  for (patch_column in seq_len(patch_columns)) {
    patch_number <- patch_number + 1L
    grid_rows <- (2L * patch_row - 1L):(2L * patch_row)
    grid_columns <- (2L * patch_column - 1L):(2L * patch_column)
    occluded_grids[
      patch_number,
      ,
      grid_rows,
      grid_columns
    ] <- 0
  }
}

occluded_probability <- predict_probability(
  cnn_fit$model,
  torch_tensor(occluded_grids, dtype = torch_float()),
  seq_len(number_of_patches)
)
occlusion_map <- matrix(
  cnn_probability[[selected_test_position]] - occluded_probability,
  nrow = patch_rows,
  ncol = patch_columns,
  byrow = TRUE
)

results <- list(
  predictions = predictions,
  metrics = metrics,
  histories = list(
    mlp = mlp_fit$history,
    cnn = cnn_fit$history
  ),
  runtime = data.frame(
    model = c(
      "Logistic regression",
      "Multilayer perceptron",
      "Two-dimensional CNN"
    ),
    seconds = c(
      logistic_seconds,
      mlp_fit$elapsed_seconds,
      cnn_fit$elapsed_seconds
    ),
    parameters = c(
      length(coef(logistic_fit)),
      count_parameters(mlp_fit$model),
      count_parameters(cnn_fit$model)
    )
  ),
  tuning = tuning_results,
  permutation_importance = permutation_importance,
  calibration = list(
    logistic = calibration_table(observed_test, predictions$logistic),
    mlp = calibration_table(observed_test, predictions$mlp),
    cnn = calibration_table(observed_test, predictions$cnn)
  ),
  occlusion = list(
    row = selected_row,
    probability = cnn_probability[[selected_test_position]],
    map = occlusion_map
  ),
  best_epoch = c(
    mlp = mlp_fit$best_epoch,
    cnn = cnn_fit$best_epoch
  ),
  torch_version = as.character(packageVersion("torch"))
)

saveRDS(
  results,
  file = "data/model_results.rds",
  compress = "xz"
)

print(metrics)
print(results$runtime)
print(tuning_results)
message("Saved data/model_results.rds")
