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
      ),
      h4("Expected Draft Position"),
      div(
        style = 'display:flex; align-items:center; gap:8px; flex-wrap:wrap;',
        textOutput(outputId='expected_draft_position'),
        selectInput(
          inputId = 'input_order_column',
          label = 'Order by',
          choices = setdiff(names(g.r.dt.pros), 'id'),
          selected = 'Rank',
          width = '180px'
        ),
        selectInput(
          inputId = 'input_order_dir',
          label = 'Direction',
          choices = c('Ascending' = 'asc', 'Descending' = 'desc'),
          selected = 'asc',
          width = '160px'
        )
      ),
      h4("Drafted by position"),
      textOutput(outputId='drafted_positions')
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