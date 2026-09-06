# SGMT 431 Lecture 6

This Quarto website teaches clustering methods for sports analytics in R, from k-means and hierarchical clustering through Gaussian mixtures, density-based and spectral methods, and non-negative matrix factorization.

## Render

1. Open the `lecture 6` folder as the project folder.
2. Install `dplyr`, `ggplot2`, `tidyr`, `ggrepel`, `cluster`, `mclust`, `dbscan`, and `kernlab` if needed.
3. Run `quarto render`.
4. Open `_site/index.html`.

The included `data/nba_2024.rds` file keeps rendering fast and offline. Run `R/prepare_nba_data.R` only when you want to rebuild the data from ESPN through hoopR. No model fits are precomputed; every tutorial runs in well under a minute.

## Tutorials

| File | Topic |
|---|---|
| `6.1-unsupervised-learning-features-distances.qmd` | Archetypes versus comparables, five decisions in a dissimilarity, distance choices, principal components |
| `6.2-kmeans-and-kmedoids.qmd` | Objective, Lloyd's algorithm, k-means++, elbow, silhouette, gap statistic, PAM |
| `6.3-hierarchical-clustering.qmd` | Linkage rules, Lance-Williams, dendrograms, cophenetic correlation, cutting trees, DIANA |
| `6.4-gaussian-mixture-models.qmd` | Mixture likelihood, EM derivation and implementation, covariance families, BIC, Bayesian mixtures |
| `6.5-density-graphs-matrix-factorization.qmd` | DBSCAN, HDBSCAN, spectral clustering, Gower distance, NMF for shot charts |
| `6.6-validating-stabilizing-communicating.qmd` | Internal indexes, bootstrap stability, ARI and NMI, sensitivity, reporting |

The slides live in `slides/` and are rendered separately with `quarto render` from that folder.
