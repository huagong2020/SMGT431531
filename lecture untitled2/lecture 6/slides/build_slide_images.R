# Generate the figures used by the Lecture 6 slides.
# Run from the Lecture 6 project root:
# Rscript slides/build_slide_images.R

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(cluster)
  library(mclust)
  library(dbscan)
})
source("R/cluster_helpers.R")

dir.create("slides/images", showWarnings = FALSE, recursive = TRUE)
theme_set(theme_minimal(base_size = 16))
save_plot <- function(plot, name, width = 10, height = 5.4) {
  ggsave(file.path("slides", "images", name), plot, width = width, height = height, dpi = 160, bg = "white")
}

nba <- load_nba()
profiles <- nba$profiles
shots <- nba$shots
X <- profile_matrix(profiles)
D <- dist(X)
set.seed(431)

# 1. PCA with positions --------------------------------------------------------------
pca <- prcomp(X)
variance <- pca$sdev^2 / sum(pca$sdev^2)
scores <- data.frame(pca$x[, 1:2], player = profiles$player, position_group = profiles$position_group, minutes = profiles$minutes)
save_plot(
  ggplot(scores, aes(PC1, PC2, color = position_group)) +
    geom_point(alpha = 0.7, size = 2.2) +
    ggrepel::geom_text_repel(data = scores %>% arrange(desc(minutes)) %>% slice_head(n = 10), aes(label = player), size = 3.2, color = "black") +
    labs(x = paste0("PC1 (", round(100 * variance[1]), "%)"), y = paste0("PC2 (", round(100 * variance[2]), "%)"), color = NULL) +
    theme(legend.position = "bottom"),
  "pca-positions.png"
)

# 2. Criteria for k ----------------------------------------------------------------------
within <- sapply(1:10, function(k) kmeans(X, k, nstart = 25, iter.max = 100)$tot.withinss)
silhouettes <- sapply(2:10, function(k) mean(silhouette(kmeans(X, k, nstart = 25, iter.max = 100)$cluster, D)[, "sil_width"]))
gap <- clusGap(X, FUN = function(x, k) kmeans(x, k, nstart = 10, iter.max = 100), K.max = 10, B = 30)
criteria <- bind_rows(
  data.frame(k = 1:10, value = within, criterion = "within-cluster SS"),
  data.frame(k = 2:10, value = silhouettes, criterion = "average silhouette"),
  data.frame(k = 1:10, value = gap$Tab[, "gap"], criterion = "gap statistic")
)
save_plot(
  ggplot(criteria, aes(k, value)) +
    geom_line(color = "#03a9e6", linewidth = 1) +
    geom_point(color = "#03a9e6", size = 2.5) +
    facet_wrap(~criterion, scales = "free_y") +
    scale_x_continuous(breaks = 1:10) +
    labs(x = "Number of clusters", y = NULL),
  "kmeans-criteria.png"
)

# 3. Cluster heat map ---------------------------------------------------------------------
fit <- kmeans(X, 6, nstart = 50, iter.max = 100)
centers <- as.data.frame(fit$centers) %>%
  mutate(cluster = factor(seq_len(6))) %>%
  tidyr::pivot_longer(-cluster, names_to = "feature", values_to = "z") %>%
  mutate(feature = factor(feature_labels[feature], levels = rev(feature_labels[profile_features])))
save_plot(
  ggplot(centers, aes(cluster, feature, fill = z)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.1f", z)), size = 3.4) +
    scale_fill_gradient2(low = "#4575b4", mid = "white", high = "#d73027", midpoint = 0) +
    labs(x = "k-means cluster", y = NULL, fill = "z"),
  "cluster-heatmap.png", width = 9, height = 6
)

# 4. Dendrograms ---------------------------------------------------------------------------
png(file.path("slides", "images", "dendrograms.png"), width = 1600, height = 860, res = 150, bg = "white")
par(mfrow = c(1, 3), mar = c(1, 4, 3, 1))
for (method in c("single", "average", "ward.D2")) {
  plot(hclust(D, method = method), labels = FALSE, main = method, xlab = "", sub = "", hang = -1)
}
dev.off()

# 5. GMM on shot locations -----------------------------------------------------------------
shot_sample <- shots %>% filter(court_y >= -2) %>% slice_sample(n = 6000)
Y <- as.matrix(shot_sample[, c("court_x", "court_y")])
fit5 <- em_gmm(Y, 5, seed = 431)
ellipse <- function(mean, covariance, points = 80) {
  angles <- seq(0, 2 * pi, length.out = points)
  circle <- cbind(cos(angles), sin(angles))
  eig <- eigen(covariance)
  transformed <- circle %*% diag(sqrt(eig$values)) %*% t(eig$vectors)
  data.frame(x = transformed[, 1] + mean[[1]], y = transformed[, 2] + mean[[2]])
}
ellipses <- bind_rows(lapply(1:5, function(k) ellipse(fit5$means[k, ], fit5$covariances[[k]]) %>% mutate(component = factor(k))))
save_plot(
  ggplot() +
    geom_point(data = shot_sample %>% mutate(component = factor(fit5$cluster)), aes(court_x, court_y, color = component), alpha = 0.3, size = 0.8) +
    geom_path(data = ellipses, aes(x, y, group = component), color = "black", linewidth = 0.8) +
    court_layers() +
    labs(x = NULL, y = NULL, color = "Component") +
    theme(legend.position = "right"),
  "gmm-shots.png", width = 8, height = 6
)

# 6. BIC curves ------------------------------------------------------------------------------
mixture <- Mclust(X, G = 2:9, modelNames = c("EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "VEE"), verbose = FALSE)
png(file.path("slides", "images", "bic-curves.png"), width = 1500, height = 860, res = 150, bg = "white")
plot(mixture, what = "BIC")
dev.off()

# 7. HDBSCAN on one player's shots -------------------------------------------------------------
player_shots <- shots %>% filter(shooter == "Stephen Curry", court_y >= -2)
S <- as.matrix(player_shots[, c("court_x", "court_y")])
hdb <- hdbscan(S, minPts = 15)
save_plot(
  player_shots %>%
    mutate(cluster = factor(hdb$cluster)) %>%
    ggplot(aes(court_x, court_y, color = cluster)) +
    geom_point(alpha = 0.6, size = 1.2) +
    scale_color_manual(values = c("0" = "gray70", scales::hue_pal()(max(hdb$cluster)))) +
    court_layers() +
    labs(x = NULL, y = NULL, color = "Cluster"),
  "hdbscan-shots.png", width = 8, height = 6
)

# 8. Half-moons ---------------------------------------------------------------------------------
make_moons <- function(n, noise = 0.08) {
  t <- runif(n, 0, pi)
  moons <- rbind(data.frame(x = cos(t), y = sin(t), truth = 1), data.frame(x = 1 - cos(t), y = 0.5 - sin(t), truth = 2))
  moons$x <- moons$x + rnorm(2 * n, 0, noise)
  moons$y <- moons$y + rnorm(2 * n, 0, noise)
  moons
}
moons <- make_moons(200)
M <- as.matrix(moons[, c("x", "y")])
moons$`k-means` <- kmeans(M, 2, nstart = 20)$cluster
moons$spectral <- spectral_clustering(M, k = 2, neighbors = 10)$cluster
save_plot(
  moons %>%
    tidyr::pivot_longer(c(`k-means`, spectral), names_to = "method", values_to = "cluster") %>%
    ggplot(aes(x, y, color = factor(cluster))) +
    geom_point(alpha = 0.7, size = 1.8) +
    facet_wrap(~method) +
    coord_equal() +
    labs(color = "Cluster"),
  "moons.png", width = 10, height = 4.6
)

# 9. NMF bases -------------------------------------------------------------------------------------
grid <- shot_grid(shots, cell = 2.5, minimum_shots = 200)
V <- grid$V / rowSums(grid$V)
nmf <- nmf_multiplicative(V, rank = 6, iterations = 400, seed = 431)
bases <- bind_rows(lapply(1:6, function(r) grid$cells %>% mutate(intensity = nmf$H[r, ], basis = paste("Basis", r))))
save_plot(
  ggplot(bases, aes(x, y, fill = intensity)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#a12835") +
    court_layers(color = "gray40") +
    facet_wrap(~basis, ncol = 3) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none", axis.text = element_blank()),
  "nmf-bases.png", width = 10, height = 6.4
)

# 10. Bootstrap stability ---------------------------------------------------------------------------
kmeans_6 <- fit$cluster
ward_6 <- cutree(hclust(D, method = "ward.D2"), k = 6)
stability_kmeans <- bootstrap_stability(X, function(Z) kmeans(Z, 6, nstart = 20, iter.max = 100)$cluster, reference = kmeans_6, B = 50)
stability_ward <- bootstrap_stability(X, function(Z) cutree(hclust(dist(Z), method = "ward.D2"), k = 6), reference = ward_6, B = 50)
stability <- bind_rows(
  data.frame(method = "k-means", cluster = factor(1:6), jaccard = colMeans(stability_kmeans)),
  data.frame(method = "Ward", cluster = factor(1:6), jaccard = colMeans(stability_ward))
)
save_plot(
  ggplot(stability, aes(cluster, jaccard, fill = method)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = c(0.5, 0.75), linetype = "dashed", color = "gray40") +
    labs(x = "Cluster", y = "Mean bootstrap Jaccard similarity", fill = NULL),
  "stability.png"
)

message("Lecture 6 slide images written to slides/images")
