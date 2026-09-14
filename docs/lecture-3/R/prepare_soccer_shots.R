# Prepare a compact soccer shot data set for Lecture 3.
# Run from the Lecture 3 project root:
# Rscript R/prepare_soccer_shots.R

library(dplyr)
library(jsonlite)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

dir.create("data", showWarnings = FALSE, recursive = TRUE)

competition_id <- 43L
season_id <- 106L
grid_height <- 16L
grid_width <- 24L
field_length <- 120
field_width <- 80

matches_url <- sprintf(
  paste0(
    "https://raw.githubusercontent.com/statsbomb/open-data/",
    "master/data/matches/%s/%s.json"
  ),
  competition_id,
  season_id
)

matches_raw <- fromJSON(matches_url, simplifyVector = FALSE)

match_metadata <- lapply(matches_raw, function(match) {
  data.frame(
    match_id = as.integer(match$match_id),
    match_date = as.Date(match$match_date),
    home_team = match$home_team$home_team_name,
    away_team = match$away_team$away_team_name,
    stringsAsFactors = FALSE
  )
}) %>%
  bind_rows() %>%
  arrange(match_date, match_id)

match_ids <- match_metadata$match_id
n_matches <- length(match_ids)
n_train_matches <- floor(0.70 * n_matches)
n_validation_matches <- floor(0.15 * n_matches)

match_metadata <- match_metadata %>%
  mutate(
    split = case_when(
      row_number() <= n_train_matches ~ "train",
      row_number() <= n_train_matches + n_validation_matches ~ "validation",
      TRUE ~ "test"
    )
  )

grid_cell <- function(location) {
  x <- max(0, min(field_length - 1e-8, as.numeric(location[[1]])))
  y <- max(0, min(field_width - 1e-8, as.numeric(location[[2]])))

  c(
    row = floor(y / (field_width / grid_height)) + 1L,
    column = floor(x / (field_length / grid_width)) + 1L
  )
}

put_player <- function(grid, channel, location) {
  cell <- grid_cell(location)
  grid[channel, cell[["row"]], cell[["column"]]] <- 1L
  grid
}

goal_angle <- function(x, y) {
  angle <- abs(
    atan2(44 - y, 120 - x) -
      atan2(36 - y, 120 - x)
  )

  ifelse(angle > pi, 2 * pi - angle, angle)
}

shot_rows <- list()
spatial_grids <- list()
shot_index <- 0L

for (match_number in seq_along(match_ids)) {
  current_match_id <- match_ids[[match_number]]
  message(
    sprintf(
      "Reading match %d of %d: %s",
      match_number,
      n_matches,
      current_match_id
    )
  )

  events_url <- sprintf(
    paste0(
      "https://raw.githubusercontent.com/statsbomb/open-data/",
      "master/data/events/%s.json"
    ),
    current_match_id
  )

  events <- tryCatch(
    fromJSON(events_url, simplifyVector = FALSE),
    error = function(error) {
      warning("Skipping match ", current_match_id, ": ", conditionMessage(error))
      NULL
    }
  )

  if (is.null(events)) {
    next
  }

  match_info <- match_metadata %>%
    filter(.data$match_id == .env$current_match_id)

  for (event in events) {
    if (is.null(event$type$name) || event$type$name != "Shot") {
      next
    }

    shot <- event$shot
    shot_type <- shot$type$name %||% NA_character_

    if (is.na(shot_type) || shot_type == "Penalty") {
      next
    }

    if (is.null(shot$freeze_frame) || length(shot$freeze_frame) == 0) {
      next
    }

    if (is.null(event$location) || length(event$location) < 2) {
      next
    }

    shot_x <- as.numeric(event$location[[1]])
    shot_y <- as.numeric(event$location[[2]])

    # Channels: shooter, attacking teammates, outfield defenders,
    # and defending goalkeeper.
    grid <- array(
      0L,
      dim = c(4L, grid_height, grid_width)
    )

    grid <- put_player(grid, 1L, event$location)

    visible_attackers <- 0L
    visible_defenders <- 0L

    for (player in shot$freeze_frame) {
      if (is.null(player$location) || length(player$location) < 2) {
        next
      }

      is_teammate <- isTRUE(player$teammate)
      position_name <- player$position$name %||% ""
      is_goalkeeper <- identical(position_name, "Goalkeeper")

      if (is_teammate) {
        grid <- put_player(grid, 2L, player$location)
        visible_attackers <- visible_attackers + 1L
      } else if (is_goalkeeper) {
        grid <- put_player(grid, 4L, player$location)
        visible_defenders <- visible_defenders + 1L
      } else {
        grid <- put_player(grid, 3L, player$location)
        visible_defenders <- visible_defenders + 1L
      }
    }

    shot_index <- shot_index + 1L
    spatial_grids[[shot_index]] <- grid

    shot_rows[[shot_index]] <- data.frame(
      event_id = event$id,
      match_id = current_match_id,
      match_date = match_info$match_date,
      split = match_info$split,
      team = event$team$name %||% NA_character_,
      player = event$player$name %||% NA_character_,
      minute = as.numeric(event$minute %||% NA_real_),
      shot_x = shot_x,
      shot_y = shot_y,
      distance = sqrt((120 - shot_x)^2 + (40 - shot_y)^2),
      angle = goal_angle(shot_x, shot_y),
      header = as.integer(identical(shot$body_part$name, "Head")),
      set_piece = as.integer(!identical(shot_type, "Open Play")),
      under_pressure = as.integer(isTRUE(event$under_pressure)),
      first_time = as.integer(isTRUE(shot$first_time)),
      visible_attackers = visible_attackers,
      visible_defenders = visible_defenders,
      goal = as.integer(identical(shot$outcome$name, "Goal")),
      outcome = shot$outcome$name %||% NA_character_,
      stringsAsFactors = FALSE
    )
  }
}

if (shot_index == 0L) {
  stop("No eligible shots were found.")
}

shot_data <- bind_rows(shot_rows)

spatial_array <- array(
  0L,
  dim = c(
    shot_index,
    4L,
    grid_height,
    grid_width
  )
)

for (i in seq_len(shot_index)) {
  spatial_array[i, , , ] <- spatial_grids[[i]]
}

feature_names <- c(
  "shot_x",
  "shot_y",
  "distance",
  "angle",
  "header",
  "set_piece",
  "under_pressure",
  "first_time",
  "visible_attackers",
  "visible_defenders"
)

tabular_matrix <- as.matrix(shot_data[, feature_names])
storage.mode(tabular_matrix) <- "double"

training_rows <- shot_data$split == "train"
feature_center <- colMeans(tabular_matrix[training_rows, , drop = FALSE])
feature_scale <- apply(
  tabular_matrix[training_rows, , drop = FALSE],
  2,
  sd
)
feature_scale[feature_scale == 0 | is.na(feature_scale)] <- 1

teaching_data <- list(
  spatial = spatial_array,
  tabular = tabular_matrix,
  target = shot_data$goal,
  metadata = shot_data,
  feature_names = feature_names,
  feature_center = feature_center,
  feature_scale = feature_scale,
  channel_names = c(
    "shooter",
    "attacking_teammates",
    "outfield_defenders",
    "defending_goalkeeper"
  ),
  grid = list(
    height = grid_height,
    width = grid_width,
    field_length = field_length,
    field_width = field_width
  ),
  provenance = list(
    provider = "StatsBomb Open Data",
    competition = "FIFA World Cup",
    season = "2022",
    competition_id = competition_id,
    season_id = season_id,
    matches_url = matches_url,
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    exclusions = c(
      "penalty shots",
      "shots without an event freeze frame"
    )
  )
)

saveRDS(
  teaching_data,
  file = "data/soccer_shots.rds",
  compress = "xz"
)

summary_table <- shot_data %>%
  count(split) %>%
  left_join(
    shot_data %>%
      group_by(split) %>%
      summarize(
        matches = n_distinct(match_id),
        goals = sum(goal),
        goal_rate = mean(goal),
        .groups = "drop"
      ),
    by = "split"
  )

print(summary_table)
message("Saved data/soccer_shots.rds")
