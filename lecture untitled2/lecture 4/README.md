# SGMT 431 Lecture 4

This Quarto website teaches player performance evaluation with multilevel models and Bayesian inference in R.

## Render

1. Open the `lecture 4` folder as the project folder.
2. Install `dplyr`, `ggplot2`, `tidyr`, `ggrepel`, `lme4`, `rstanarm`, and `posterior` if needed.
3. Run `quarto render`.
4. Open `_site/index.html`.

The included `data/nfl_passes_2023.rds`, `data/wnba_2024.rds`, and `data/bayes_results.rds` files keep rendering fast and offline. Run `R/prepare_player_data.R` only when you want to rebuild the data from nflverse and wehoop. Run `R/build_bayes_results.R` after changing the Stan model or the bootstrap settings; it requires `cmdstanr` with CmdStan installed and takes about ten minutes on a laptop.

## Tutorials

| File | Topic |
|---|---|
| `4.1-player-evaluation-small-samples.qmd` | Signal plus noise, reliability, regression to the mean, CPOE |
| `4.2-multilevel-varying-intercepts.qmd` | Two-level models, variance components, shrinkage, BLUPs, GLMMs |
| `4.3-covariates-slopes-crossed-effects.qmd` | Level-one and level-two covariates, varying slopes, crossed effects, bootstrap |
| `4.4-bayesian-thinking-multilevel-connection.qmd` | Bayes' rule, beta-binomial, normal-normal, empirical Bayes versus full Bayes |
| `4.5-computing-posteriors.qmd` | Grid, Laplace, Metropolis-Hastings, HMC, Stan, diagnostics |
| `4.6-regularization-priors-plus-minus.qmd` | Ridge as a Gaussian prior, RAPM, ranking with uncertainty |

The slides live in `slides/` and are rendered separately with `quarto render` from that folder.


## Additional guided lab

`4.7-guided-lab-player-decisions.qmd` adds guided lab: player comparisons. The extracted script `R/guided_lab.R` runs from this lecture folder. Install `posterior` for Lectures 4 and 5, and `e1071` for Lecture 6. The new Gibbs examples run in R without Stan.
