fluidPage(
  fluidRow(
    column(
      2,
      # Adds selector for positions.
      selectizeInput(
        inputId = 'input_position',
        label = 'Positions',
        choices = g.vec.positions,
        selected = g.vec.positions,
        multiple = TRUE
        )
    ),
    column(
      10,
      # Adds search to drop players.
      selectizeInput(
        inputId = 'input_drop_players',
        label = 'Dropped Players',
        choices = g.r.dt.pros$Player,
        selected = NULL,
        multiple = TRUE,
        width = '100%'
      )
    )
  ),
  h3("Players"),
  DT::dataTableOutput(outputId = 'dto_players')
)