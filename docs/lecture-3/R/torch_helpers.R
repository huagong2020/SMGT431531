library(torch)

# Probability metrics live in a torch-free file so that Tutorial 3.6 can be
# rendered without LibTorch installed.
source(file.path("R", "metrics.R"))

torch_set_num_threads(2)

# Reuse centers and scales estimated on TRAINING shots for every split.
# sweep(..., 2, ...) works column by column: (feature - mean) / SD.
standardize_tabular <- function(teaching_data) {
  sweep(
    sweep(
      teaching_data$tabular,
      2,
      teaching_data$feature_center,
      "-"
    ),
    2,
    teaching_data$feature_scale,
    "/"
  )
}

# Course helper: convert ordinary R data to the tensors torch layers accept.
# torch_float() selects 32-bit floating-point numbers, including 0/1 targets.
# Shapes: tabular N x 10; spatial N x 4 x 16 x 24; target N x 1.
# train/validation/test are R row indices, not tensors or copies of the data.
make_tensors <- function(teaching_data) {
  list(
    tabular = torch_tensor(
      standardize_tabular(teaching_data),
      dtype = torch_float()
    ),
    spatial = torch_tensor(
      teaching_data$spatial,
      dtype = torch_float()
    ),
    target = torch_tensor(
      matrix(teaching_data$target, ncol = 1),
      dtype = torch_float()
    ),
    train = which(teaching_data$metadata$split == "train"),
    validation = which(teaching_data$metadata$split == "validation"),
    test = which(teaching_data$metadata$split == "test")
  )
}

# nn_module() defines a reusable model constructor; mlp_module() creates a model.
# initialize runs once per new model. self refers to that model instance.
# Layers assigned to self are registered: their weights/biases automatically
# appear in model$parameters and receive gradients during backpropagation.
mlp_module <- nn_module(
  "ShotMLP",
  initialize = function(number_of_features = 10L) {
    # nn_sequential applies its layers in order. nn_linear(in, out) includes bias.
    self$network <- nn_sequential(
      nn_linear(number_of_features, 12),
      nn_relu(),
      nn_linear(12, 6),
      nn_relu(),
      nn_linear(6, 1) # One logit per shot; the training loss includes sigmoid.
    )
  },
  forward = function(x) {
    # model(x) calls forward: B x 10 -> B x 12 -> B x 6 -> B x 1.
    self$network(x)
  }
)

# Same module pattern as the MLP, now with local, shared filter weights.
# This classifier expects 16 x 24 grids; changing grid size also requires
# changing the flatten size and the first dense layer below.
cnn_module <- nn_module(
  "ShotCNN",
  initialize = function(number_of_channels = 4L) {
    # Each 3 x 3 filter spans ALL input channels. Padding 1 keeps height/width.
    # With its default stride 2, each 2 x 2 max pool halves height and width.
    self$features <- nn_sequential(
      nn_conv2d(number_of_channels, 8, kernel_size = 3, padding = 1),
      nn_relu(),
      nn_max_pool2d(kernel_size = 2),
      nn_conv2d(8, 12, kernel_size = 3, padding = 1),
      nn_relu(),
      nn_max_pool2d(kernel_size = 2)
    )

    # After both pools: B x 12 x 4 x 6. Flatten gives 288 values per shot.
    self$classifier <- nn_sequential(
      nn_linear(12 * 4 * 6, 16),
      nn_relu(),
      nn_dropout(p = 0.10), # Randomly zero 10% of activations in training only.
      nn_linear(16, 1)
    )
  },
  forward = function(x) {
    x <- self$features(x)
    # Preserve the batch dimension (size(1)); list the remaining values per shot.
    x <- x$reshape(c(x$size(1), 12 * 4 * 6))
    self$classifier(x)
  }
)

# Measure mean loss with fixed weights; indices select validation rows.
# eval() disables dropout; with_no_grad() separately disables gradient recording.
tensor_loss <- function(model, x, y, indices) {
  model$eval()
  with_no_grad({
    logits <- model(x[indices, ])
    nnf_binary_cross_entropy_with_logits(
      logits,
      y[indices, ]
    )$item() # Convert a one-value tensor into an ordinary R number.
  })
}

# Course helper, not a torch built-in: train either network with the same loop.
# x contains all input rows; y must be N x 1. Only training_indices update weights.
# validation_indices choose the checkpoint. Never supply test rows for tuning.
# epochs is an upper limit; patience counts epochs without improvement > 1e-5.
# Returns a list: fitted model, loss history, best epoch/loss, and elapsed seconds.
fit_binary_model <- function(
    model,
    x,
    y,
    training_indices,
    validation_indices,
    epochs = 20L,
    batch_size = 64L,
    learning_rate = 0.01,
    patience = 4L,
    seed = 431L) {
  # torch controls tensor randomness (e.g. dropout); R controls sample() below.
  # Seed before explicitly constructing a model to reproduce its initial weights.
  # If model is passed as a constructor call, R evaluates it lazily below.
  torch_manual_seed(seed)
  set.seed(seed)

  # Give Adam all registered weights and biases. lr is the overall step scale.
  optimizer <- optim_adam(
    model$parameters,
    lr = learning_rate
  )

  history <- data.frame()
  best_validation_loss <- Inf
  best_state <- NULL
  best_epoch <- NA_integer_
  epochs_without_improvement <- 0L
  started_at <- proc.time()[["elapsed"]]

  for (epoch in seq_len(epochs)) {
    model$train() # Restore training behavior after the previous validation pass.
    # Shuffle training rows, then divide them into batches (last may be smaller).
    shuffled_indices <- sample(training_indices)
    batches <- split(
      shuffled_indices,
      ceiling(seq_along(shuffled_indices) / batch_size)
    )

    batch_losses <- numeric(length(batches))

    for (batch_number in seq_along(batches)) {
      batch <- batches[[batch_number]]
      optimizer$zero_grad() # Clear stored gradients: backward() otherwise adds.
      logits <- model(x[batch, ]) # Forward pass; omitted dimensions keep all inputs.
      # Logits and outcomes both have shape B x 1. Default reduction is the mean.
      # Do not apply sigmoid first: this stable loss already incorporates it.
      loss <- nnf_binary_cross_entropy_with_logits(
        logits,
        y[batch, ]
      )
      loss$backward() # Chain rule: calculate gradients; weights are still unchanged.
      optimizer$step() # Adam uses those gradients to update every parameter.
      batch_losses[[batch_number]] <- loss$item() # Save the pre-update batch loss.
    }

    # Once per epoch, evaluate all validation rows without updating parameters.
    validation_loss <- tensor_loss(
      model,
      x,
      y,
      validation_indices
    )

    history <- rbind(
      history,
      data.frame(
        epoch = epoch,
        training_loss = mean(batch_losses), # Mean of batch means, at changing weights.
        validation_loss = validation_loss
      )
    )

    if (validation_loss < best_validation_loss - 1e-5) {
      best_validation_loss <- validation_loss
      best_epoch <- epoch
      # clone() saves independent tensors; later updates must not alter the checkpoint.
      best_state <- lapply(
        model$state_dict(),
        function(parameter) parameter$clone()
      )
      epochs_without_improvement <- 0L
    } else {
      epochs_without_improvement <- epochs_without_improvement + 1L
    }

    if (epochs_without_improvement >= patience) {
      break
    }
  }

  # Return the best validation checkpoint, which may precede the last epoch.
  model$load_state_dict(best_state)
  elapsed_seconds <- proc.time()[["elapsed"]] - started_at

  list(
    model = model,
    history = history,
    best_epoch = best_epoch,
    best_validation_loss = best_validation_loss,
    elapsed_seconds = elapsed_seconds
  )
}

# Course helper for fixed-model prediction: select rows, disable dropout/gradients,
# turn one logit per shot into a probability, and return an ordinary R vector.
predict_probability <- function(model, x, indices) {
  model$eval()
  with_no_grad({
    logits <- model(x[indices, ])
    as.numeric(as_array(torch_sigmoid(logits)))
  })
}

# numel() counts scalar entries in each registered weight/bias tensor.
count_parameters <- function(model) {
  sum(
    vapply(
      model$parameters,
      function(parameter) parameter$numel(),
      numeric(1)
    )
  )
}
