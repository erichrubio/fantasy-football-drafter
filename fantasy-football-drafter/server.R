function(input, output, session) {

  # Reactive dropped player IDs.
  r.v.button.dropped <- reactiveValues(id = c())

  # Reactive player projections.
  r.dt.pros <- reactiveValues(data = g.r.dt.pros)

  # Player projections data table output.
  output$dto_players <- DT::renderDataTable(
    r.dt.pros$data[
      (!id %in% r.v.button.dropped$id) &
      (!Player %in% input$input_drop_players) &
      (Pos %in% input$input_position)
    ][order(Rank)][, !"id"],
    server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE,
    options = list(pageLength = 50)
  )

  # Listens for "Drop" button click.
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    r.v.button.dropped$id <- unique(
      append(
        r.v.button.dropped$id,
        r.dt.pros$data[selectedRow, "id"]
      )
    )
    updateSelectizeInput(
      session,
      inputId = 'input_drop_players',
      selected = g.r.dt.pros[
        (id %in% r.v.button.dropped$id) |
        (Player %in% input$input_drop_players)
      ]$Player
    )
  })

}