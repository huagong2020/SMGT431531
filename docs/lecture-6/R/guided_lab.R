## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
source("R/cluster_helpers.R")
nba <- load_nba()
zone_data <- nba$shots %>% mutate(zone = case_when(
  three == 1 ~ "three", distance <= 4 ~ "rim", TRUE ~ "other_two"))
zone_data$zone <- factor(zone_data$zone, levels = c("rim", "other_two", "three"))
counts <- unclass(xtabs(~ athlete_id + zone, data = zone_data))
counts <- counts[rowSums(counts) >= 200, , drop = FALSE]
player_key <- zone_data %>% distinct(athlete_id, shooter) %>%
  distinct(athlete_id, .keep_all = TRUE)
player_names <- player_key$shooter[match(rownames(counts), as.character(player_key$athlete_id))]
stopifnot(!anyNA(player_names), all(rowSums(counts) >= 200))
head(data.frame(player = player_names, attempts = rowSums(counts), counts))


## -----------------------------------------------------------------------------
alpha <- rep(0.5, ncol(counts))
posterior_shape <- sweep(counts, 2, alpha, "+")
P <- posterior_shape / rowSums(posterior_shape)
clr <- function(P) log(P) - rowMeans(log(P))
Z <- clr(P)
stopifnot(max(abs(rowSums(Z))) < 1e-10)
projection <- prcomp(Z, center = TRUE, scale. = FALSE)
coordinates <- projection$x[, 1:2]
set.seed(631)
hard <- kmeans(Z, centers = 4, nstart = 50)


## -----------------------------------------------------------------------------
set.seed(632)
fuzzy_fits <- lapply(1:20, function(b)
  e1071::cmeans(Z, centers = 4, m = 2, iter.max = 200))
fuzzy <- fuzzy_fits[[which.min(vapply(fuzzy_fits, function(f) f$withinerror, numeric(1)))]]
U <- fuzzy$membership
stopifnot(max(abs(rowSums(U) - 1)) < 1e-8)
membership_table <- data.frame(player = player_names, attempts = rowSums(counts),
  maximum_membership = apply(U, 1, max), U)
membership_table %>% arrange(maximum_membership) %>% slice_head(n = 8) %>%
  knitr::kable(digits = 3, row.names = FALSE)


## -----------------------------------------------------------------------------
#| fig-cap: "Shot-selection profiles in two CLR principal coordinates. Transparency indicates maximum fuzzy membership."
#| fig-alt: "Player points colored by their largest fuzzy membership with uncertain boundary points shown more transparently."
display <- data.frame(player = player_names, PC1 = coordinates[, 1], PC2 = coordinates[, 2],
  cluster = factor(max.col(U)), certainty = apply(U, 1, max))
ggplot(display, aes(PC1, PC2, color = cluster, alpha = certainty)) +
  geom_point(size = 2) + scale_alpha_continuous(range = c(0.25, 1)) +
  coord_equal() + theme_minimal() + labs(alpha = "Largest fuzzy weight")


## -----------------------------------------------------------------------------
center_shares <- exp(fuzzy$centers) / rowSums(exp(fuzzy$centers))
round(center_shares, 3) %>% knitr::kable()


## -----------------------------------------------------------------------------
draw_dirichlet_rows <- function(shape) {
  G <- matrix(rgamma(length(shape), shape = as.vector(shape)),
              nrow = nrow(shape), ncol = ncol(shape))
  G / rowSums(G)
}
set.seed(633)
B <- 100
C <- matrix(0, nrow(counts), nrow(counts))
for (b in seq_len(B)) {
  P_draw <- draw_dirichlet_rows(posterior_shape)
  labels <- kmeans(clr(P_draw), centers = 4, nstart = 20, iter.max = 100)$cluster
  C <- C + outer(labels, labels, "==") / B
}
stopifnot(max(abs(diag(C) - 1)) < 1e-10, max(abs(C - t(C))) < 1e-10)
consensus <- cutree(hclust(as.dist(1 - C), method = "average"), k = 4)


## -----------------------------------------------------------------------------
#| fig-cap: "Co-clustering frequencies from uncertain shot-selection profiles, ordered by consensus cluster."
#| fig-alt: "A square heat map with strong blocks for stable groups and lighter boundaries for uncertain pairs."
ordering <- order(consensus)
image(seq_len(nrow(C)), seq_len(nrow(C)), C[ordering, ordering],
  col = hcl.colors(30, "Blues"), zlim = c(0, 1),
  xlab = "Players ordered by consensus cluster", ylab = "Players in the same order",
  main = "Co-clustering frequency (0 to 1)")


## -----------------------------------------------------------------------------
set.seed(634)
fuzziness <- bind_rows(lapply(c(1.3, 1.7, 2), function(m) {
  candidates <- lapply(1:10, function(b) e1071::cmeans(Z, centers = 4, m = m, iter.max = 200))
  f <- candidates[[which.min(vapply(candidates, function(f) f$withinerror, numeric(1)))]]
  data.frame(m = m, mean_maximum_membership = mean(apply(f$membership, 1, max)),
    ARI_vs_kmeans = mclust::adjustedRandIndex(max.col(f$membership), hard$cluster))
}))
fuzziness %>% knitr::kable(digits = 3)

smoothing <- bind_rows(lapply(c(0.1, 0.5, 2), function(a) {
  smoothed <- (counts + a) / (rowSums(counts) + ncol(counts) * a)
  fit <- kmeans(clr(smoothed), centers = 4, nstart = 50)
  data.frame(alpha_per_zone = a,
    ARI_vs_baseline = mclust::adjustedRandIndex(hard$cluster, fit$cluster))
}))
smoothing %>% knitr::kable(digits = 3)


## -----------------------------------------------------------------------------
nearest_center <- function(Z, centers) {
  distances <- sapply(seq_len(nrow(centers)), function(k)
    rowSums(sweep(Z, 2, centers[k, ], "-")^2))
  max.col(-distances, ties.method = "first")
}
set.seed(635)
resample_ari <- replicate(50, {
  rows <- sample(nrow(Z), floor(0.8 * nrow(Z)))
  fit <- kmeans(Z[rows, ], centers = 4, nstart = 20, iter.max = 100)
  assigned <- nearest_center(Z, fit$centers)
  mclust::adjustedRandIndex(hard$cluster, assigned)
})
quantile(resample_ari, c(0.1, 0.5, 0.9))

