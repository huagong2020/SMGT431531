# NFL offensive-style data dictionary

Source: [nflverse play-by-play](https://nflreadr.nflverse.com/reference/load_pbp.html), with [official field definitions](https://nflreadr.nflverse.com/articles/dictionary_pbp.html). Downloaded 2026-10-25; seasons 2022–2024. Data construction is fully specified in `../R/prepare_nfl_style_data.R`.

## Unit and eligible plays

Each row is a team-season. There are 96 rows, 32 per season, and eight clustering features. The sample keeps `season_type == "REG"`, `play_type` equal to `pass` or `run`, `qtr` in 1–3, absolute pre-play `score_differential <= 7`, and `half_seconds_remaining > 120`. It requires a valid offensive team and zero flags for `penalty`, `qb_kneel`, `qb_spike`, and `two_point_attempt`. Missing required filter values are excluded. Sacks and scrambles remain eligible when coded as run/pass plays. No efficiency or win variables enter the feature matrix.

## Features

All shares are proportions from 0 to 1, not percentages. Missing field values are excluded from the relevant feature denominator; known-value denominator counts are supplied. In this saved dataset the eight features are complete, and the known dropback/formation denominators equal their corresponding eligible-play counts.

| Feature | Exact construction within eligible team-season plays |
|---|---|
| `dropback_rate` | Mean of `qb_dropback` among nonmissing values; includes pass attempts, sacks, and scrambles flagged as dropbacks |
| `early_down_dropback_rate` | Same mean restricted to `down` 1 or 2 |
| `shotgun_rate` | Mean of the nonmissing `shotgun` indicator |
| `no_huddle_rate` | Mean of the nonmissing `no_huddle` indicator; formation/communication behavior, not seconds per snap |
| `mean_air_yards` | Mean `air_yards` among `play_type == "pass"` plays with finite air yards; excludes sacks and passes without recorded depth |
| `deep_target_rate` | Fraction with `air_yards >= 20` among that same recorded-depth pass sample |
| `middle_target_rate` | Fraction with `pass_location == "middle"` among recorded-depth passes whose locations are left/middle/right |
| `outside_run_rate` | Fraction with left/right `run_location` among `play_type == "run"`, `qb_scramble == 0` plays with known left/middle/right locations |

## Identifiers and denominators: exclude from PCA and clustering

- `season`, `team`, `label`: year, nflverse abbreviation, and unique team-year label.
- `n_plays`, `n_early`: eligible plays and first-/second-down eligible plays.
- `n_targets`, `n_target_locations`: recorded-depth targets and the subset with known target location.
- `n_located_runs`: nonscramble runs with a recorded direction.
- `n_dropback_known`, `n_early_dropback_known`, `n_shotgun_known`, `n_no_huddle_known`: nonmissing denominator counts for the corresponding rates.

## Limits

This is a close-score, first-three-quarter sample, not a complete estimate of a team's playbook or a situation-adjusted measure of intent. Target depth and location describe recorded attempts. Run direction does not identify blocking scheme. No-huddle does not measure elapsed time. Teams can change coaches and quarterbacks; repeated franchise observations are not independent. Standardization and PCA are pooled over the three years, so league-year differences can influence the profiles. PCA reduces redundancy but does not remove the implicit weighting caused by related measures.
