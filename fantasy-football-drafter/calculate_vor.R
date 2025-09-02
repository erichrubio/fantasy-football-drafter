library(dplyr)
library(stringr)

# --- Reusable functions ---

# Computes replacement points per position using the same approach as the original
# calculate_vor script, but parameterized by a named vector of starters per position.
# - projections: data.frame with columns `position` and metric column (e.g., `points`)
# - starters: named integer vector with counts per position (e.g., c(QB=1, RB=2, ...))
#   If a position has 0 remaining slots, replacement is set to Inf (so VOR becomes negative).
compute_replacement_points_by_position <- function(projections, starters, col = "points") {
  stopifnot(all(c("position", col) %in% names(projections)))
  pos_levels <- unique(projections$position)
  rp <- setNames(numeric(length(pos_levels)), pos_levels)
  for (p in pos_levels) {
    k <- as.integer(ifelse(is.na(starters[p]), 0L, starters[p]))
    pts <- sort(projections[[col]][projections$position == p], decreasing = TRUE)
    if (k <= 0) {
      rp[p] <- Inf
    } else if (length(pts) >= k) {
      rp[p] <- pts[k]
    } else if (length(pts) > 0) {
      rp[p] <- pts[length(pts)]
    } else {
      rp[p] <- -Inf
    }
  }
  rp
}

# Computes VORP per row given projections and starters.
# Returns a numeric vector of the same length as nrow(projections).
compute_vorp <- function(projections, starters, col = "points") {
  rp <- compute_replacement_points_by_position(projections, starters, col)
  rp_vec <- rp[projections$position]
  projections[[col]] - rp_vec
}

# Optional: End-to-end pipeline similar to the original script for CSV -> updated CSV
run_vor_pipeline <- function(csv_dir,
                             csv_name,
                             num_of_teams = 12,
                             per_team_starters = c(QB = 1, RB = 2, WR = 2, TE = 1, K = 1, DST = 1)) {
  csv_path <- paste0(csv_dir, csv_name)
  projections <- read.csv(csv_path)
  starters <- per_team_starters * num_of_teams

  # Replacement points per position (based on points only)
  rp_points <- compute_replacement_points_by_position(projections, starters, "points")

  projections <- projections %>%
    mutate(
      replacement_points = rp_points[position],
      vorp = points - replacement_points,
      f_vor = if ("floor" %in% names(.)) ifelse(is.na(floor), points, floor) - replacement_points else NA_real_,
      c_vor = if ("ceiling" %in% names(.)) ifelse(is.na(ceiling), points, ceiling) - replacement_points else NA_real_
    ) %>%
    mutate(rank = min_rank(-vorp)) %>%
    group_by(position) %>%
    mutate(position_rank = min_rank(-vorp)) %>%
    ungroup() %>%
    as.data.frame()

  # Flex VORP (RB/WR/TE)
  flex_starters <- starters[['RB']] + starters[['WR']] + num_of_teams
  flex_replacement_points <- projections %>%
    filter(position %in% c("RB", "WR", "TE")) %>%
    arrange(desc(points)) %>%
    slice(flex_starters) %>%
    summarize(flex_replacement_points = min(points)) %>%
    pull(flex_replacement_points)

  projections <- projections %>%
    mutate(flex_vorp = ifelse(
      position %in% c("RB", "WR", "TE"),
      points - flex_replacement_points,
      -9999
    ))

  top_players <- projections %>% arrange(desc(vorp))
  write.csv(top_players, paste0(str_sub(csv_path, 0, -5), '_updated.csv'), na = '')
  invisible(top_players)
}

run_vor_pipeline(
  csv_dir='/Users/erichrubio/Documents/fantasy-football/fantasy-football-drafter/',
  csv_name='2025_bad_hombres_projections.csv',
  num_of_teams=10,
  per_team_starters = c(QB = 2, RB = 2, WR = 2, TE = 1, K = 1, DST = 1)
)

