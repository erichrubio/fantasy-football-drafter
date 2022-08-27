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

g.initial.players.to.drop = c()

g.dt.pros <- fread(paste0(getwd(), "/projections_2022_season.csv"))
g.dt.pros[, id := .I]
g.dt.pros[, `:=` (
  tier = NULL,
  ecr = NULL,
  sd_ecr = NULL,
  salary = NULL,
  ppd = NULL
)]
g.dt.pros

g.r.dt.pros <- copy(g.dt.pros)

# Formats columns.
g.r.dt.pros <- g.r.dt.pros[, .(
  Rank = rank, Player = player, Pos = position, Team = team,
  Points = round(points),
  VoR = round(points_vor),
  SD_Pts = round(sd_pts), Floor = round(floor), Ceiling = round(ceiling),
  Pos_Rank = position_rank,
  F_VoR = floor_vor, C_VoR = ceiling_vor,
  ADP = adp, Uncertainty = uncertainty,
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
