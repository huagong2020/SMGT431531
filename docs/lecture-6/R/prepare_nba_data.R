# Prepare the teaching data for Lecture 6 (clustering methods for sport).
# Run from the Lecture 6 project root:
# Rscript R/prepare_nba_data.R
#
# Source: ESPN box scores and play-by-play for the 2023-24 NBA regular season,
# distributed through the hoopR package (sportsdataverse).

suppressPackageStartupMessages({
  library(dplyr)
  library(hoopR)
})

options(timeout = 300)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
retrieved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
minimum_minutes <- 500

# ---------------------------------------------------------------------------
# 1. Player-season profiles
# ---------------------------------------------------------------------------
box <- as.data.frame(hoopR::load_nba_player_box(seasons = 2024)) %>%
  filter(season_type == 2, !is.na(minutes), minutes > 0)

profiles <- box %>%
  group_by(athlete_id, player = athlete_display_name) %>%
  summarize(
    position = names(which.max(table(athlete_position_abbreviation))),
    team = names(which.max(table(team_short_display_name))),
    games = n(),
    minutes = sum(minutes),
    points = sum(points),
    fgm = sum(field_goals_made),
    fga = sum(field_goals_attempted),
    fg3m = sum(three_point_field_goals_made),
    fg3a = sum(three_point_field_goals_attempted),
    ftm = sum(free_throws_made),
    fta = sum(free_throws_attempted),
    oreb = sum(offensive_rebounds),
    dreb = sum(defensive_rebounds),
    ast = sum(assists),
    stl = sum(steals),
    blk = sum(blocks),
    tov = sum(turnovers),
    pf = sum(fouls),
    .groups = "drop"
  ) %>%
  filter(minutes >= minimum_minutes) %>%
  mutate(
    position_group = case_when(
      position %in% c("PG", "SG", "G") ~ "Guard",
      position %in% c("SF", "PF", "F") ~ "Forward",
      position == "C" ~ "Center",
      TRUE ~ "Unknown"
    ),
    per36 = 36 / minutes,
    pts_36 = points * per36,
    fga_36 = fga * per36,
    fg3a_36 = fg3a * per36,
    fta_36 = fta * per36,
    oreb_36 = oreb * per36,
    dreb_36 = dreb * per36,
    ast_36 = ast * per36,
    stl_36 = stl * per36,
    blk_36 = blk * per36,
    tov_36 = tov * per36,
    pf_36 = pf * per36,
    three_rate = fg3a / fga,
    ft_rate = fta / fga,
    efg = (fgm + 0.5 * fg3m) / fga,
    ts = points / (2 * (fga + 0.44 * fta)),
    ast_to_tov = ast / pmax(tov, 1)
  ) %>%
  select(-per36) %>%
  arrange(desc(minutes))

# ---------------------------------------------------------------------------
# 2. Field-goal attempts with half-court coordinates
# ---------------------------------------------------------------------------
pbp <- as.data.frame(hoopR::load_nba_pbp(seasons = 2024)) %>%
  filter(season_type == 2)

# ESPN records the full court with the two baskets at x = -41.75 and x = 41.75
# and the sideline-to-sideline axis as y in [-25, 25]. Fold both ends onto one
# half court: court_y is the distance from the basket toward midcourt and
# court_x is the left-right offset when facing the basket.
shots <- pbp %>%
  filter(
    shooting_play,
    !grepl("Free Throw", type_text),
    !is.na(coordinate_x),
    !is.na(coordinate_y),
    !is.na(athlete_id_1)
  ) %>%
  transmute(
    game_id,
    athlete_id = athlete_id_1,
    shooter = athlete_name_1,
    shot_type = type_text,
    made = as.integer(scoring_play),
    points = score_value,
    court_x = coordinate_y * sign(coordinate_x),
    court_y = 41.75 - abs(coordinate_x),
    distance = sqrt(court_x^2 + court_y^2),
    three = as.integer(grepl("three point", text, ignore.case = TRUE))
  ) %>%
  filter(court_y >= -5, court_y <= 40, abs(court_x) <= 25)

shooter_counts <- shots %>% count(athlete_id, name = "shots")

nba_2024 <- list(
  profiles = profiles,
  shots = shots,
  shooter_counts = shooter_counts,
  provenance = list(
    provider = "ESPN box scores and play-by-play via hoopR",
    season = "2023-24",
    season_type = "regular season",
    minimum_minutes = minimum_minutes,
    retrieved = retrieved_at,
    hoopR_version = as.character(packageVersion("hoopR")),
    notes = "Coordinates are folded onto one half court: court_y is the distance from the basket toward midcourt, court_x is the left-right offset. Free throws are excluded."
  )
)

saveRDS(nba_2024, "data/nba_2024.rds", compress = "xz")
message(sprintf(
  "Saved %d player profiles (>= %d minutes) and %d field-goal attempts by %d shooters",
  nrow(profiles), minimum_minutes, nrow(shots), nrow(shooter_counts)
))
