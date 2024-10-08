library(shiny)
library(data.table)
library(DT)

shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

g.vec.positions <- c('QB', 'RB', 'WR', 'TE', 'K', 'DST')

g.initial.players.to.drop <- c()

g.n_teams <- 12

csv_dir <- (
  '/Users/erichrubio/Documents/fantasy-football/fantasy-football-drafter/'
)

csv_name <- '2024_jelly_projections_2024_wk0_updated.csv'
# csv_name <- '2024_bad_hombres_projections_2024_wk0_updated.csv'

g.dt.pros <- fread(paste0(csv_dir, csv_name))
g.dt.pros[, id := .I]
g.dt.pros[, `:=` (
  tier = NULL
  # ecr = NULL,
  # sd_ecr = NULL,
  # salary = NULL,
  # ppd = NULL
)]
g.dt.pros

g.r.dt.pros <- copy(g.dt.pros)

# Formats columns.
g.r.dt.pros <- g.r.dt.pros[, .(
  Rank = rank,
  Player = player,
  Pos = position,
  Team = team,
  Points = round(points),
  # VoR = round(points_vor),
  VoR = round(vorp),
  SD_Pts = round(sd_pts),
  Floor = round(floor),
  Ceiling = round(ceiling),
  Pos_Rank = position_rank,
  Flex_VoR = round(flex_vorp),
  F_VoR = round(floor_vor),
  C_VoR = round(ceiling_vor),
  ADP = round(adp, 1),
  Uncertainty = uncertainty,
  id
)]

# Adds 'Drop' button.
g.r.dt.pros[, Drop := shinyInput(
  actionButton,
  NROW(g.dt.pros),
  'button_',
  label = "Drop",
  onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
),]

n.col <- NCOL(g.r.dt.pros)
setcolorder(g.r.dt.pros, c(n.col, 1:(n.col-1)))
