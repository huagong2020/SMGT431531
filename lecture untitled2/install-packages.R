# Run once from R. Viewing the rendered HTML requires no R installation.
packages <- c("dplyr", "ggplot2", "tidyr", "ggrepel", "lme4", "rstanarm",
              "posterior", "knitr", "rmarkdown", "cluster", "mclust", "dbscan",
              "kernlab", "e1071", "scales")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
message("Tutorial dependencies are available.")
# Data rebuilding additionally uses nflreadr, wehoop, and hoopR.
# Refitting CmdStan examples additionally requires cmdstanr and CmdStan.
