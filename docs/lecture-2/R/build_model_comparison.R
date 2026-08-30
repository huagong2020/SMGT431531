library(dplyr)
library(rpart)
library(ipred)
library(ranger)
library(xgboost)
library(lightgbm)
library(catboost)

source(file.path("R", "modeling_helpers.R"))

set.seed(431)

nfl_states <- load_wp_data()
development_data <- nfl_states %>% filter(season == 2022)
test_data <- nfl_states %>% filter(season == 2023)

split <- make_game_split(development_data)
analysis_data <- split$analysis
validation_data <- split$validation

tree_formula <- home_win_factor ~
  game_seconds_remaining + home_score_diff + home_possession +
  down + ydstogo + yardline_100 + home_timeouts + away_timeouts

extract_win_probability <- function(prediction) {
  if (is.matrix(prediction) || is.data.frame(prediction)) {
    return(as.numeric(prediction[, "win"]))
  }
  as.numeric(prediction)
}

# Logistic regression ---------------------------------------------------------
# This is the specification developed in Tutorial 2.1, including the
# score-difference by clock interaction. The comparison must use the model the
# course actually teaches.
logistic_fit <- glm(
  home_win ~ home_score_diff * minutes_remaining + home_possession +
    down + ydstogo + yardline_100 + home_timeouts + away_timeouts,
  family = binomial(),
  data = development_data
)
logistic_probability <- predict(
  logistic_fit,
  newdata = test_data,
  type = "response"
)

# A pruned classification tree -----------------------------------------------
full_tree <- rpart(
  tree_formula,
  data = analysis_data,
  method = "class",
  control = rpart.control(
    cp = 0,
    minsplit = 50,
    minbucket = 25,
    maxdepth = 10
  )
)

candidate_cp <- unique(full_tree$cptable[, "CP"])
tree_validation_scores <- lapply(candidate_cp, function(cp_value) {
  candidate_tree <- prune(full_tree, cp = cp_value)
  candidate_probability <- predict(
    candidate_tree,
    newdata = validation_data,
    type = "prob"
  )[, "win"]
  tibble(
    cp = cp_value,
    brier = brier_score(validation_data$home_win, candidate_probability)
  )
}) %>%
  bind_rows()

best_cp <- tree_validation_scores %>%
  slice_min(brier, n = 1, with_ties = FALSE) %>%
  pull(cp)

# Refit on the full development season at the selected complexity, so that CART
# is trained on the same data as every other model in the comparison.
tree_fit <- prune(
  rpart(
    tree_formula,
    data = development_data,
    method = "class",
    control = rpart.control(
      cp = 0,
      minsplit = 50,
      minbucket = 25,
      maxdepth = 10
    )
  ),
  cp = best_cp
)
tree_probability <- predict(
  tree_fit,
  newdata = test_data,
  type = "prob"
)[, "win"]

# Bagging ---------------------------------------------------------------------
bagging_fit <- bagging(
  tree_formula,
  data = development_data,
  nbagg = 100,
  coob = TRUE,
  control = rpart.control(
    cp = 0,
    minsplit = 80,
    minbucket = 40,
    maxdepth = 10
  )
)

# `aggregation = "average"` is essential for a probability model. The ipred
# default, "majority", returns the *fraction of trees voting* for each class, so
# a unanimous ensemble reports exactly 0 or 1 and log loss lands on the clipping
# floor. Averaging the trees' leaf probabilities is what Tutorial 2.3 describes.
bagging_probability <- predict(
  bagging_fit,
  newdata = test_data,
  type = "prob",
  aggregation = "average"
) %>%
  extract_win_probability()

# Random forest ---------------------------------------------------------------
# `probability = TRUE` makes this a probability forest: it averages the trees'
# leaf probabilities rather than counting their votes. See Tutorial 2.3.
forest_fit <- ranger(
  tree_formula,
  data = development_data,
  probability = TRUE,
  num.trees = 400,
  mtry = 3,
  min.node.size = 50,
  seed = 431
)
forest_probability <- predict(
  forest_fit,
  data = test_data
)$predictions[, "win"]

# Numeric matrices for XGBoost and LightGBM ----------------------------------
analysis_matrix <- model_matrix(analysis_data)
validation_matrix <- model_matrix(validation_data)
development_matrix <- model_matrix(development_data)
test_matrix <- model_matrix(test_data)

xgb_analysis <- xgb.DMatrix(
  analysis_matrix,
  label = analysis_data$home_win
)
xgb_validation <- xgb.DMatrix(
  validation_matrix,
  label = validation_data$home_win
)

xgb_fit <- xgb.train(
  params = list(
    objective = "binary:logistic",
    eval_metric = "logloss",
    eta = 0.05,
    max_depth = 4,
    min_child_weight = 20,
    subsample = 0.80,
    colsample_bytree = 0.80,
    lambda = 1,
    alpha = 0
  ),
  data = xgb_analysis,
  nrounds = 500,
  watchlist = list(validation = xgb_validation),
  early_stopping_rounds = 30,
  verbose = 0
)

xgb_rounds <- xgb_fit$best_iteration
xgb_development <- xgb.DMatrix(
  development_matrix,
  label = development_data$home_win
)
xgb_final <- xgb.train(
  params = xgb_fit$params,
  data = xgb_development,
  nrounds = xgb_rounds,
  verbose = 0
)
xgb_probability <- predict(xgb_final, test_matrix)

# LightGBM --------------------------------------------------------------------
lgb_analysis <- lgb.Dataset(
  analysis_matrix,
  label = analysis_data$home_win
)
lgb_validation <- lgb.Dataset.create.valid(
  dataset = lgb_analysis,
  data = validation_matrix,
  label = validation_data$home_win
)

lgb_parameters <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.05,
  num_leaves = 15,
  max_depth = 5,
  min_data_in_leaf = 50,
  feature_fraction = 0.80,
  bagging_fraction = 0.80,
  bagging_freq = 1,
  lambda_l2 = 1,
  seed = 431,
  verbosity = -1
)

lgb_fit <- lgb.train(
  params = lgb_parameters,
  data = lgb_analysis,
  nrounds = 500,
  valids = list(validation = lgb_validation),
  early_stopping_rounds = 30,
  verbose = -1
)

lgb_rounds <- lgb_fit$best_iter
lgb_development <- lgb.Dataset(
  development_matrix,
  label = development_data$home_win
)
lgb_final <- lgb.train(
  params = lgb_parameters,
  data = lgb_development,
  nrounds = lgb_rounds,
  verbose = -1
)
lgb_probability <- predict(lgb_final, test_matrix)

# CatBoost with native categorical features ----------------------------------
catboost_features <- c(
  wp_predictors,
  "play_type"
)

catboost_data <- bind_rows(
  analysis_data %>% mutate(partition = "analysis"),
  validation_data %>% mutate(partition = "validation"),
  development_data %>% mutate(partition = "development"),
  test_data %>% mutate(partition = "test")
) %>%
  select(partition, all_of(catboost_features)) %>%
  mutate(across(where(is.character), as.factor))

make_catboost_frame <- function(partition_name) {
  catboost_data %>%
    filter(partition == partition_name) %>%
    select(-partition)
}

cat_analysis <- catboost.load_pool(
  make_catboost_frame("analysis"),
  label = analysis_data$home_win
)
cat_validation <- catboost.load_pool(
  make_catboost_frame("validation"),
  label = validation_data$home_win
)

cat_parameters <- list(
  loss_function = "Logloss",
  eval_metric = "Logloss",
  iterations = 400,
  depth = 4,
  learning_rate = 0.03,
  l2_leaf_reg = 10,
  random_seed = 431,
  od_type = "Iter",
  od_wait = 30,
  logging_level = "Silent"
)

cat_fit <- catboost.train(
  learn_pool = cat_analysis,
  test_pool = cat_validation,
  params = cat_parameters
)
cat_rounds <- cat_fit$tree_count

cat_development <- catboost.load_pool(
  make_catboost_frame("development"),
  label = development_data$home_win
)
cat_test <- catboost.load_pool(
  make_catboost_frame("test"),
  label = test_data$home_win
)

cat_parameters$iterations <- cat_rounds
cat_parameters$od_type <- NULL
cat_parameters$od_wait <- NULL
cat_final <- catboost.train(
  learn_pool = cat_development,
  params = cat_parameters
)
cat_probability <- catboost.predict(
  cat_final,
  cat_test,
  prediction_type = "Probability"
)

# Save one tidy prediction file for Tutorial 2.6 -----------------------------
model_predictions <- test_data %>%
  transmute(
    game_id,
    season,
    week,
    home_team,
    away_team,
    game_seconds_remaining,
    home_score_diff,
    home_win,
    nflverse = home_wp,
    logistic = logistic_probability,
    cart = tree_probability,
    bagging = bagging_probability,
    random_forest = forest_probability,
    xgboost = xgb_probability,
    lightgbm = lgb_probability,
    catboost = cat_probability
  )

saveRDS(
  model_predictions,
  file.path("data", "model_predictions.rds")
)
