# SGMT 431 Lecture 5

This Quarto website teaches team performance evaluation with regression, multilevel, and Bayesian rating models in R.

## Render

1. Open the `lecture 5` folder as the project folder.
2. Install `dplyr`, `ggplot2`, `tidyr`, `ggrepel`, `rstanarm`, `posterior`, and `MASS` if needed.
3. Run `quarto render`.
4. Open `_site/index.html`.

The included `data/nfl_games.rds`, `data/epl_matches.rds`, and `data/team_results.rds` files keep rendering fast and offline. Run `R/prepare_team_data.R` only when you want to rebuild the data from nflverse and football-data.co.uk. Run `R/build_team_results.R` after changing the Poisson model or the Stan state-space model; it requires `cmdstanr` with CmdStan installed and takes a few minutes.

## Tutorials

| File | Topic |
|---|---|
| `5.1-team-evaluation-outcomes-luck-schedules.qmd` | Records versus margins, Pythagorean expectation, talent versus luck, schedule adjustment |
| `5.2-ratings-from-margins.qmd` | The $\pm1$ design, least squares, ridge as a Gaussian prior, marginal likelihood, `rstanarm` |
| `5.3-bradley-terry-win-loss-ratings.qmd` | Bradley-Terry, separation, Bayesian version, Elo connection, ordered outcomes |
| `5.4-bayesian-poisson-goal-model.qmd` | Multilevel Poisson goals model, posterior predictive checks, match probabilities, season replay |
| `5.5-dynamic-ratings-elo-state-space.qmd` | Elo from scratch, state-space model, Kalman filter, Stan fit |
| `5.6-evaluating-communicating-ratings.qmd` | Sequential forecast evaluation, market comparison, betting math, rank uncertainty |

The slides live in `slides/` and are rendered separately with `quarto render` from that folder.
