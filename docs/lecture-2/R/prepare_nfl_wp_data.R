library(dplyr)
library(nflreadr)

set.seed(431)

seasons <- c(2022, 2023)

pbp <- nflreadr::load_pbp(seasons = seasons)

nfl_wp_states <- pbp %>%
  filter(
    season_type == "REG",
    qtr <= 4,
    !is.na(posteam),
    !is.na(down),
    !is.na(game_seconds_remaining),
    !is.na(score_differential),
    !is.na(yardline_100),
    !is.na(ydstogo),
    !is.na(result),
    result != 0
  ) %>%
  mutate(
    home_win = as.integer(result > 0),
    home_win_factor = factor(
      if_else(home_win == 1, "win", "loss"),
      levels = c("loss", "win")
    ),
    home_possession = as.integer(posteam == home_team),
    home_score_diff = if_else(
      home_possession == 1,
      score_differential,
      -score_differential
    ),
    home_timeouts = if_else(
      home_possession == 1,
      posteam_timeouts_remaining,
      defteam_timeouts_remaining
    ),
    away_timeouts = if_else(
      home_possession == 1,
      defteam_timeouts_remaining,
      posteam_timeouts_remaining
    ),
    minutes_remaining = game_seconds_remaining / 60,
    two_minute_block = floor(game_seconds_remaining / 120),
    down = factor(down, levels = 1:4),
    play_type = factor(if_else(is.na(play_type), "other", play_type)),
    final_margin = result
  ) %>%
  arrange(game_id, desc(game_seconds_remaining), play_id) %>%
  group_by(game_id, two_minute_block) %>%
  slice(1) %>%
  ungroup() %>%
  select(
    game_id,
    season,
    week,
    home_team,
    away_team,
    home_win,
    home_win_factor,
    final_margin,
    qtr,
    game_seconds_remaining,
    minutes_remaining,
    home_score_diff,
    home_possession,
    down,
    ydstogo,
    yardline_100,
    home_timeouts,
    away_timeouts,
    play_type,
    home_wp
  ) %>%
  filter(
    complete.cases(
      game_seconds_remaining,
      home_score_diff,
      home_possession,
      down,
      ydstogo,
      yardline_100,
      home_timeouts,
      away_timeouts
    )
  )

output_path <- file.path("data", "nfl_wp_states.rds")
saveRDS(nfl_wp_states, output_path, compress = "xz")

message(
  "Saved ", format(nrow(nfl_wp_states), big.mark = ","),
  " game states from ", n_distinct(nfl_wp_states$game_id),
  " games to ", output_path
)
