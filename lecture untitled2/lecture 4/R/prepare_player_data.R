# Prepare the teaching data for Lecture 4 (player performance evaluation).
# Run from the Lecture 4 project root:
# Rscript R/prepare_player_data.R
#
# Two public sources are used:
#   1. NFL 2023 regular-season play-by-play data distributed through nflreadr.
#   2. WNBA 2024 regular-season box scores distributed through wehoop.
# Both packages download from the nflverse / sportsdataverse GitHub releases.

suppressPackageStartupMessages({
  library(dplyr)
  library(nflreadr)
  library(wehoop)
})

options(timeout = 300)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
retrieved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# ---------------------------------------------------------------------------
# 1. NFL pass attempts
# ---------------------------------------------------------------------------
pbp <- as.data.frame(nflreadr::load_pbp(2023))
rosters <- as.data.frame(nflreadr::load_rosters(2023)) %>%
  filter(!is.na(gsis_id)) %>%
  distinct(gsis_id, .keep_all = TRUE) %>%
  select(gsis_id, full_name, position)

passes <- pbp %>%
  filter(
    season_type == "REG",
    play_type == "pass",
    sack == 0,
    qb_spike == 0,
    qb_scramble == 0,
    two_point_attempt == 0,
    !is.na(air_yards),
    !is.na(pass_location),
    !is.na(passer_player_id),
    !is.na(receiver_player_id),
    !is.na(complete_pass),
    !is.na(qb_epa)
  ) %>%
  arrange(game_id, play_id) %>%
  transmute(
    game_id,
    play_id,
    week,
    posteam,
    defteam,
    home = as.integer(posteam == home_team),
    passer_id = passer_player_id,
    receiver_id = receiver_player_id,
    complete = as.integer(complete_pass),
    air_yards,
    pass_location,
    qb_hit = as.integer(qb_hit),
    shotgun = as.integer(shotgun),
    no_huddle = as.integer(no_huddle),
    down = as.integer(down),
    ydstogo,
    yardline_100,
    score_differential,
    half_seconds_remaining,
    qb_epa,
    epa,
    interception = as.integer(interception),
    yards_after_catch,
    passing_yards,
    nflverse_cp = cp,
    nflverse_cpoe = cpoe,
    roof
  ) %>%
  left_join(
    rosters %>% rename(passer_id = gsis_id, passer = full_name, passer_position = position),
    by = "passer_id"
  ) %>%
  left_join(
    rosters %>% rename(receiver_id = gsis_id, receiver = full_name, receiver_position = position),
    by = "receiver_id"
  ) %>%
  mutate(
    passer = if_else(is.na(passer), passer_id, passer),
    receiver = if_else(is.na(receiver), receiver_id, receiver),
    receiver_position = case_when(
      receiver_position %in% c("WR", "TE", "RB") ~ receiver_position,
      receiver_position %in% c("FB", "HB") ~ "RB",
      TRUE ~ "other"
    )
  )

nfl_passes <- list(
  passes = passes,
  provenance = list(
    provider = "nflverse play-by-play via nflreadr",
    season = 2023L,
    season_type = "regular season",
    filters = c(
      "play_type == pass", "no sacks", "no spikes", "no scrambles",
      "no two-point attempts", "air yards, pass location, passer, and receiver recorded"
    ),
    retrieved = retrieved_at,
    nflreadr_version = as.character(packageVersion("nflreadr"))
  )
)

saveRDS(nfl_passes, "data/nfl_passes_2023.rds", compress = "xz")
message(sprintf(
  "Saved %d pass attempts, %d passers, %d receivers, %d defenses",
  nrow(passes), n_distinct(passes$passer_id),
  n_distinct(passes$receiver_id), n_distinct(passes$defteam)
))

# ---------------------------------------------------------------------------
# 2. WNBA 2024 shooting: Caitlin Clark's game log and the league distribution
# ---------------------------------------------------------------------------
# The regular-season file also contains the All-Star Game (Team WNBA vs Team
# USA), which is dropped here.
wnba_box <- as.data.frame(wehoop::load_wnba_player_box(seasons = 2024)) %>%
  filter(
    season_type == 2,
    !is.na(minutes),
    minutes > 0,
    !team_abbreviation %in% c("USA", "WNBA")
  )

game_teams <- wnba_box %>%
  distinct(game_id, team_abbreviation)

clark_games <- wnba_box %>%
  filter(athlete_display_name == "Caitlin Clark") %>%
  left_join(
    game_teams %>% rename(opponent = team_abbreviation),
    by = "game_id",
    relationship = "many-to-many"
  ) %>%
  filter(opponent != team_abbreviation) %>%
  arrange(game_date) %>%
  transmute(
    game = row_number(),
    game_date = as.Date(game_date),
    opponent,
    minutes,
    fg3m = three_point_field_goals_made,
    fg3a = three_point_field_goals_attempted,
    fgm = field_goals_made,
    fga = field_goals_attempted,
    ftm = free_throws_made,
    fta = free_throws_attempted,
    points
  )

league_three <- wnba_box %>%
  group_by(athlete_id, athlete_display_name) %>%
  summarize(
    fg3m = sum(three_point_field_goals_made),
    fg3a = sum(three_point_field_goals_attempted),
    .groups = "drop"
  ) %>%
  filter(fg3a >= 40) %>%
  mutate(fg3_pct = fg3m / fg3a) %>%
  arrange(desc(fg3a))

wnba_2024 <- list(
  clark_games = clark_games,
  league_three = league_three,
  provenance = list(
    provider = "ESPN box scores via wehoop",
    season = 2024L,
    season_type = "regular season",
    retrieved = retrieved_at,
    wehoop_version = as.character(packageVersion("wehoop"))
  )
)

saveRDS(wnba_2024, "data/wnba_2024.rds", compress = "xz")
message(sprintf(
  "Saved %d Caitlin Clark games and %d league shooters with at least 40 three-point attempts",
  nrow(clark_games), nrow(league_three)
))
