# Helper functions for Lecture 6: clustering methods for sport.
#
# Several algorithms are implemented from scratch in base R so that the
# tutorials can show the calculation, then compared with package versions
# (stats::kmeans, stats::hclust, cluster, mclust, dbscan, kernlab).

suppressPackageStartupMessages(library(dplyr))

# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------
load_nba <- function(path = file.path("data", "nba_2024.rds")) {
  readRDS(path)
}

# The per-36 and rate variables used to describe a player's style.
profile_features <- c(
  "pts_36", "fga_36", "fg3a_36", "fta_36", "oreb_36", "dreb_36",
  "ast_36", "stl_36", "blk_36", "tov_36", "three_rate", "ft_rate", "efg", "ast_to_tov"
)

feature_labels <- c(
  pts_36 = "Points per 36", fga_36 = "FGA per 36", fg3a_36 = "3PA per 36",
  fta_36 = "FTA per 36", oreb_36 = "Off. rebounds per 36", dreb_36 = "Def. rebounds per 36",
  ast_36 = "Assists per 36", stl_36 = "Steals per 36", blk_36 = "Blocks per 36",
  tov_36 = "Turnovers per 36", three_rate = "3PA / FGA", ft_rate = "FTA / FGA",
  efg = "Effective FG%", ast_to_tov = "Assist-to-turnover"
)

# Standardized feature matrix with player names as row names.
profile_matrix <- function(profiles, features = profile_features) {
  X <- as.matrix(profiles[, features])
  rownames(X) <- profiles$player
  scale(X)
}

# ---------------------------------------------------------------------------
# k-means from scratch
# ---------------------------------------------------------------------------

# k-means++ seeding: the first center is uniform, every later center is drawn
# with probability proportional to squared distance from the nearest center.
kmeans_plus_plus_init <- function(X, k) {
  n <- nrow(X)
  centers <- matrix(NA_real_, nrow = k, ncol = ncol(X))
  centers[1, ] <- X[sample(n, 1), ]
  if (k == 1) return(centers)
  for (j in 2:k) {
    d2 <- apply(X, 1, function(x) {
      min(colSums((t(centers[seq_len(j - 1), , drop = FALSE]) - x)^2))
    })
    centers[j, ] <- X[sample(n, 1, prob = d2 / sum(d2)), ]
  }
  centers
}

# Lloyd's algorithm. Returns cluster labels, centers, the within-cluster sum
# of squares after each iteration, and the number of iterations.
lloyd_kmeans <- function(X, k, max_iterations = 100, seed = NULL, init = c("plusplus", "random")) {
  init <- match.arg(init)
  if (!is.null(seed)) set.seed(seed)
  centers <- if (init == "plusplus") kmeans_plus_plus_init(X, k) else X[sample(nrow(X), k), , drop = FALSE]
  labels <- rep(0L, nrow(X))
  history <- numeric(0)

  for (iteration in seq_len(max_iterations)) {
    # Assignment step: nearest center for every observation
    distances <- sapply(seq_len(k), function(j) rowSums((X - matrix(centers[j, ], nrow(X), ncol(X), byrow = TRUE))^2))
    new_labels <- max.col(-distances)
    history <- c(history, sum(distances[cbind(seq_len(nrow(X)), new_labels)]))

    # Update step: recompute centers
    for (j in seq_len(k)) {
      members <- X[new_labels == j, , drop = FALSE]
      if (nrow(members) > 0) centers[j, ] <- colMeans(members)
    }

    if (identical(new_labels, labels)) break
    labels <- new_labels
  }

  list(cluster = labels, centers = centers, wcss = history, iterations = iteration)
}

within_cluster_ss <- function(X, cluster) {
  sum(sapply(unique(cluster), function(j) {
    members <- X[cluster == j, , drop = FALSE]
    sum(scale(members, scale = FALSE)^2)
  }))
}

# ---------------------------------------------------------------------------
# Internal validity indexes
# ---------------------------------------------------------------------------
calinski_harabasz <- function(X, cluster) {
  n <- nrow(X)
  k <- length(unique(cluster))
  overall <- colMeans(X)
  between <- sum(sapply(unique(cluster), function(j) {
    members <- X[cluster == j, , drop = FALSE]
    nrow(members) * sum((colMeans(members) - overall)^2)
  }))
  within <- within_cluster_ss(X, cluster)
  (between / (k - 1)) / (within / (n - k))
}

davies_bouldin <- function(X, cluster) {
  ids <- sort(unique(cluster))
  centers <- t(sapply(ids, function(j) colMeans(X[cluster == j, , drop = FALSE])))
  scatter <- sapply(ids, function(j) {
    members <- X[cluster == j, , drop = FALSE]
    mean(sqrt(rowSums((members - matrix(centers[match(j, ids), ], nrow(members), ncol(X), byrow = TRUE))^2)))
  })
  k <- length(ids)
  ratios <- sapply(seq_len(k), function(i) {
    max(sapply(setdiff(seq_len(k), i), function(j) {
      (scatter[[i]] + scatter[[j]]) / sqrt(sum((centers[i, ] - centers[j, ])^2))
    }))
  })
  mean(ratios)
}

# Dunn index from a distance matrix: smallest between-cluster distance divided
# by the largest within-cluster diameter.
dunn_index <- function(D, cluster) {
  D <- as.matrix(D)
  ids <- sort(unique(cluster))
  diameters <- sapply(ids, function(j) {
    members <- which(cluster == j)
    if (length(members) < 2) 0 else max(D[members, members])
  })
  separations <- c()
  for (i in seq_along(ids)) {
    for (j in seq_along(ids)) {
      if (j > i) {
        separations <- c(separations, min(D[cluster == ids[[i]], cluster == ids[[j]]]))
      }
    }
  }
  min(separations) / max(diameters)
}

# ---------------------------------------------------------------------------
# External agreement between two labelings
# ---------------------------------------------------------------------------
adjusted_rand_index <- function(a, b) {
  tab <- table(a, b)
  sum_cells <- sum(choose(tab, 2))
  sum_rows <- sum(choose(rowSums(tab), 2))
  sum_cols <- sum(choose(colSums(tab), 2))
  total <- choose(sum(tab), 2)
  expected <- sum_rows * sum_cols / total
  maximum <- (sum_rows + sum_cols) / 2
  (sum_cells - expected) / (maximum - expected)
}

normalized_mutual_information <- function(a, b) {
  tab <- table(a, b)
  joint <- tab / sum(tab)
  pa <- rowSums(joint)
  pb <- colSums(joint)
  entropy <- function(p) -sum(p[p > 0] * log(p[p > 0]))
  mutual <- sum(joint[joint > 0] * log(joint[joint > 0] / outer(pa, pb)[joint > 0]))
  mutual / sqrt(entropy(pa) * entropy(pb))
}

# Jaccard similarity between two sets of observation indices.
jaccard <- function(set_a, set_b) {
  length(intersect(set_a, set_b)) / length(union(set_a, set_b))
}

# Bootstrap stability of a clustering (Hennig, 2007). For each bootstrap sample,
# recluster and record, for every original cluster, the best Jaccard match
# among the new clusters (computed on the observations shared by both samples).
bootstrap_stability <- function(X, cluster_function, reference, B = 50, seed = 431) {
  set.seed(seed)
  n <- nrow(X)
  ids <- sort(unique(reference))
  scores <- matrix(NA_real_, nrow = B, ncol = length(ids))
  for (b in seq_len(B)) {
    rows <- sort(unique(sample(n, n, replace = TRUE)))
    new_labels <- cluster_function(X[rows, , drop = FALSE])
    for (i in seq_along(ids)) {
      original <- intersect(which(reference == ids[[i]]), rows)
      scores[b, i] <- max(sapply(unique(new_labels), function(j) {
        jaccard(original, rows[new_labels == j])
      }))
    }
  }
  colnames(scores) <- paste0("cluster_", ids)
  scores
}

# ---------------------------------------------------------------------------
# Gaussian mixture model by EM (full covariances) from scratch
# ---------------------------------------------------------------------------
mvnormal_density <- function(X, mean, covariance) {
  d <- ncol(X)
  centered <- sweep(X, 2, mean)
  precision <- solve(covariance)
  quadratic <- rowSums((centered %*% precision) * centered)
  exp(-0.5 * quadratic) / sqrt((2 * pi)^d * det(covariance))
}

em_gmm <- function(X, k, max_iterations = 200, tolerance = 1e-6, seed = 431) {
  set.seed(seed)
  n <- nrow(X)
  d <- ncol(X)
  # Initialize from k-means
  start <- kmeans(X, k, nstart = 5)
  weights <- as.numeric(table(start$cluster)) / n
  means <- start$centers
  covariances <- lapply(seq_len(k), function(j) cov(X[start$cluster == j, , drop = FALSE]) + diag(1e-6, d))
  log_likelihood <- numeric(0)

  for (iteration in seq_len(max_iterations)) {
    # E-step: responsibilities
    densities <- sapply(seq_len(k), function(j) weights[[j]] * mvnormal_density(X, means[j, ], covariances[[j]]))
    total <- rowSums(densities)
    responsibilities <- densities / total
    log_likelihood <- c(log_likelihood, sum(log(total)))

    # M-step: weights, means, covariances
    n_j <- colSums(responsibilities)
    weights <- n_j / n
    for (j in seq_len(k)) {
      means[j, ] <- colSums(responsibilities[, j] * X) / n_j[[j]]
      centered <- sweep(X, 2, means[j, ])
      covariances[[j]] <- (t(centered * responsibilities[, j]) %*% centered) / n_j[[j]] + diag(1e-6, d)
    }

    if (iteration > 1 && abs(diff(tail(log_likelihood, 2))) < tolerance) break
  }

  parameters <- k * d + k * d * (d + 1) / 2 + (k - 1)
  list(
    weights = weights,
    means = means,
    covariances = covariances,
    responsibilities = responsibilities,
    cluster = max.col(responsibilities),
    log_likelihood = log_likelihood,
    bic = -2 * tail(log_likelihood, 1) + parameters * log(n),
    iterations = iteration
  )
}

# ---------------------------------------------------------------------------
# Spectral clustering from scratch
# ---------------------------------------------------------------------------
spectral_clustering <- function(X, k, neighbors = 10, seed = 431) {
  n <- nrow(X)
  D <- as.matrix(dist(X))
  # Mutual k-nearest-neighbor affinity with a Gaussian kernel scaled locally
  sigma <- apply(D, 1, function(row) sort(row)[neighbors + 1])
  W <- exp(-D^2 / (outer(sigma, sigma)))
  knn <- t(apply(D, 1, function(row) rank(row) <= neighbors + 1))
  W <- W * (knn | t(knn))
  diag(W) <- 0
  degree <- rowSums(W)
  L_sym <- diag(n) - diag(1 / sqrt(degree)) %*% W %*% diag(1 / sqrt(degree))
  eig <- eigen(L_sym, symmetric = TRUE)
  U <- eig$vectors[, n:(n - k + 1)]
  U <- U / sqrt(rowSums(U^2))
  set.seed(seed)
  list(
    cluster = kmeans(U, k, nstart = 20)$cluster,
    eigenvalues = rev(eig$values),
    embedding = U
  )
}

# ---------------------------------------------------------------------------
# Non-negative matrix factorization by multiplicative updates (Lee and Seung)
# ---------------------------------------------------------------------------
nmf_multiplicative <- function(V, rank, iterations = 300, seed = 431) {
  set.seed(seed)
  n <- nrow(V)
  m <- ncol(V)
  W <- matrix(runif(n * rank), n, rank)
  H <- matrix(runif(rank * m), rank, m)
  loss <- numeric(iterations)
  eps <- 1e-9
  for (i in seq_len(iterations)) {
    H <- H * (t(W) %*% V) / (t(W) %*% W %*% H + eps)
    W <- W * (V %*% t(H)) / (W %*% H %*% t(H) + eps)
    loss[[i]] <- sum((V - W %*% H)^2)
  }
  # Normalize each basis (row of H) to sum to one and move the scale into W
  scale <- rowSums(H)
  H <- H / scale
  W <- W %*% diag(scale)
  list(W = W, H = H, loss = loss)
}

# ---------------------------------------------------------------------------
# Shot charts
# ---------------------------------------------------------------------------

# Count shots per player in a grid of square cells covering the half court.
shot_grid <- function(shots, cell = 2.5, x_range = c(-25, 25), y_range = c(-5, 35),
                      minimum_shots = 200) {
  x_breaks <- seq(x_range[[1]], x_range[[2]], by = cell)
  y_breaks <- seq(y_range[[1]], y_range[[2]], by = cell)
  eligible <- shots %>%
    filter(court_x >= x_range[[1]], court_x < x_range[[2]], court_y >= y_range[[1]], court_y < y_range[[2]]) %>%
    group_by(athlete_id) %>%
    filter(n() >= minimum_shots) %>%
    ungroup() %>%
    mutate(
      column = findInterval(court_x, x_breaks),
      row = findInterval(court_y, y_breaks),
      cell_id = (row - 1) * (length(x_breaks) - 1) + column
    )
  counts <- eligible %>% dplyr::count(athlete_id, shooter, cell_id)
  players <- counts %>% distinct(athlete_id, shooter)
  V <- matrix(0, nrow = nrow(players), ncol = (length(x_breaks) - 1) * (length(y_breaks) - 1),
              dimnames = list(players$shooter, NULL))
  V[cbind(match(counts$athlete_id, players$athlete_id), counts$cell_id)] <- counts$n
  cells <- expand.grid(column = seq_len(length(x_breaks) - 1), row = seq_len(length(y_breaks) - 1)) %>%
    mutate(
      cell_id = row_number(),
      x = x_breaks[column] + cell / 2,
      y = y_breaks[row] + cell / 2
    )
  list(V = V, cells = cells, players = players)
}

# Court markings for ggplot: hoop, backboard, paint, and the three-point arc
# (NBA dimensions in feet, basket at the origin, court_y pointing to midcourt).
court_layers <- function(color = "gray45") {
  arc <- data.frame(theta = seq(acos(22 / 23.75), pi - acos(22 / 23.75), length.out = 100)) %>%
    transmute(x = 23.75 * cos(theta), y = 23.75 * sin(theta))
  hoop <- data.frame(theta = seq(0, 2 * pi, length.out = 60)) %>%
    transmute(x = 0.75 * cos(theta), y = 0.75 * sin(theta))
  list(
    ggplot2::geom_path(data = hoop, ggplot2::aes(x, y), color = color, inherit.aes = FALSE),
    ggplot2::annotate("segment", x = -3, xend = 3, y = -1.25, yend = -1.25, color = color),
    ggplot2::annotate("rect", xmin = -8, xmax = 8, ymin = -5.25, ymax = 13.75, fill = NA, color = color),
    ggplot2::annotate("segment", x = -22, xend = -22, y = -5.25, yend = 23.75 * sin(acos(22 / 23.75)), color = color),
    ggplot2::annotate("segment", x = 22, xend = 22, y = -5.25, yend = 23.75 * sin(acos(22 / 23.75)), color = color),
    ggplot2::geom_path(data = arc, ggplot2::aes(x, y), color = color, inherit.aes = FALSE),
    ggplot2::coord_fixed(xlim = c(-25, 25), ylim = c(-5.25, 35))
  )
}
