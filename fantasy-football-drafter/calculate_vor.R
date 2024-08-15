library(dplyr)
library(stringr)

csv_dir <- (
  '/Users/erichrubio/Documents/fantasy-football/fantasy-football-drafter/'
)
csv_name <- '2024_jelly_projections_2024_wk0.csv'
csv_path <- paste0(csv_dir, csv_name)

projections <- read.csv(csv_path)

team_size <- 12

# Defines the number of starters per position in the league
starters <- c(QB = 1, RB = 2, WR = 2, TE = 1) * team_size

# Calculates the replacement level points for each position
replacement_points <- projections %>%
  group_by(position) %>%
  arrange(desc(points)) %>%
  slice(starters[position]) %>%
  summarize(replacement_points = min(points))

# Merges the replacement level points back to the original data
projections <- projections %>%
  left_join(replacement_points, by = "position") %>%
  mutate(vorp = points - replacement_points) %>%
  mutate(rank = min_rank(-vorp)) %>%
  group_by(position) %>%
  mutate(position_rank = min_rank(-vorp)) %>%
  ungroup() %>%
  as.data.frame()

# Prints the top players by VORP
top_players <- projections %>%
  arrange(desc(vorp))
print(top_players %>% head(20))
print(top_players %>%
        select(player, position, points, vorp, rank, position_rank) %>%
        head(20))

flex_starters <- starters[['RB']] + starters[['WR']] + team_size

flex_replacement_points <- projections %>%
  filter(position %in% c("RB", "WR", "TE")) %>%
  arrange(desc(points)) %>%
  slice(flex_starters) %>%
  summarize(flex_replacement_points = min(points)) %>%
  pull(flex_replacement_points)

# Calculate flex VORP for each RB, WR, and TE
projections <- projections %>%
  mutate(flex_vorp = ifelse(
    position %in% c("RB", "WR", "TE"),
    points - flex_replacement_points,
    NA
  ))

# Prints the top players by VORP
top_players <- projections %>%
  arrange(desc(vorp))
print(top_players %>%
        select(player, position, points, vorp, flex_vorp) %>%
        head(20))

write.csv(top_players, paste0(str_sub(csv_path, 0, -5), '_updated.csv'), na='')

