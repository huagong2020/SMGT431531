# Prepare the teaching data for Lecture 5 (team performance evaluation).
# Run from the Lecture 5 project root:
# Rscript R/prepare_team_data.R
#
# Two public sources are used:
#   1. NFL regular-season schedules and results distributed through nflreadr,
#      including closing betting lines.
#   2. English Premier League results from football-data.co.uk.

suppressPackageStartupMessages({
  library(dplyr)
  library(nflreadr)
})

options(timeout = 300)
dir.create("data", showWarnings = FALSE, recursive = TRUE)
retrieved_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

# ---------------------------------------------------------------------------
# 1. NFL games, 2022 to 2024 regular seasons
# ---------------------------------------------------------------------------
nfl_seasons <- 2022:2024

games <- as.data.frame(nflreadr::load_schedules(nfl_seasons)) %>%
  filter(game_type == "REG", !is.na(home_score), !is.na(away_score)) %>%
  arrange(season, week, gameday, game_id) %>%
  transmute(
    game_id,
    season,
    week,
    gameday = as.Date(gameday),
    home_team,
    away_team,
    home_score,
    away_score,
    margin = home_score - away_score,
    home_win = as.integer(margin > 0),
    tie = as.integer(margin == 0),
    neutral_site = as.integer(location == "Neutral"),
    overtime = as.integer(overtime == 1),
    div_game = as.integer(div_game == 1),
    spread_line,
    total_line,
    home_moneyline,
    away_moneyline,
    roof
  )

# Every team appears under one abbreviation; standardize relocations if needed.
teams <- sort(unique(c(games$home_team, games$away_team)))

nfl_games <- list(
  games = games,
  teams = teams,
  provenance = list(
    provider = "nflverse schedules via nflreadr",
    seasons = nfl_seasons,
    season_type = "regular season",
    retrieved = retrieved_at,
    nflreadr_version = as.character(packageVersion("nflreadr")),
    notes = "spread_line is the closing home-team spread (positive means the home team is favored); moneylines are American odds."
  )
)

saveRDS(nfl_games, "data/nfl_games.rds", compress = "xz")
message(sprintf("Saved %d NFL games across %d teams", nrow(games), length(teams)))

# ---------------------------------------------------------------------------
# 2. Premier League matches, 2021-22 to 2023-24
# ---------------------------------------------------------------------------
epl_codes <- c("2122", "2223", "2324")
epl_labels <- c("2021-22", "2022-23", "2023-24")

epl <- lapply(seq_along(epl_codes), function(i) {
  url <- sprintf("https://www.football-data.co.uk/mmz4281/%s/E0.csv", epl_codes[[i]])
  raw <- read.csv(url, stringsAsFactors = FALSE)
  raw %>%
    filter(!is.na(FTHG), !is.na(FTAG), HomeTeam != "") %>%
    transmute(
      season = epl_labels[[i]],
      date = as.Date(Date, format = "%d/%m/%Y"),
      home_team = HomeTeam,
      away_team = AwayTeam,
      home_goals = as.integer(FTHG),
      away_goals = as.integer(FTAG),
      result = FTR,
      home_shots = HS,
      away_shots = AS,
      home_shots_on_target = HST,
      away_shots_on_target = AST,
      odds_home = B365H,
      odds_draw = B365D,
      odds_away = B365A
    )
}) %>%
  bind_rows() %>%
  arrange(season, date) %>%
  group_by(season) %>%
  mutate(match_number = row_number()) %>%
  ungroup()

epl_matches <- list(
  matches = epl,
  provenance = list(
    provider = "football-data.co.uk (E0 files)",
    seasons = epl_labels,
    retrieved = retrieved_at,
    notes = "Odds columns are Bet365 decimal odds recorded by the provider."
  )
)

saveRDS(epl_matches, "data/epl_matches.rds", compress = "xz")
message(sprintf(
  "Saved %d Premier League matches across %d seasons",
  nrow(epl), length(epl_labels)
))
