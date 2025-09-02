function(input, output, session) {

  # Reactive dropped player IDs.
  r.v.button.dropped <- reactiveValues(id = c())

  # Reactive player projections.
  r.dt.pros <- reactiveValues(data = g.r.dt.pros)

  output$drafted_positions <- renderText({
    position_counts <- g.r.dt.pros[
      Player %in% input$input_drop_players,
      .N,
      by = Pos
    ]
    allowed <- c("QB", "RB", "WR", "TE")
    position_counts <- position_counts[Pos %in% allowed]
    paste(position_counts$Pos, position_counts$N, sep = ":", collapse = " ")
  })

  output$expected_draft_position <- renderText({
    picks <- length(input$input_drop_players)
    rnd <- floor(picks / g.n_teams)
    pick <- picks %% g.n_teams + 1
    paste0("Round: ", rnd, ";  Pick: ", pick)
  })

  output$dto_players <- DT::renderDataTable(
    {
      dt <- r.dt.pros$data[
        (!id %in% r.v.button.dropped$id) &
          (!Player %in% input$input_drop_players) &
          (Pos %in% input$input_position)
      ]
      # Hide id column from display
      dt <- dt[, !"id"]
      # Determine ordering column (default to Rank)
      cols <- colnames(dt)
      ord_idx <- match(input$input_order_column, cols)
      if (is.na(ord_idx)) ord_idx <- match("Rank", cols)
      # Determine ordering direction (default asc)
      dir <- input$input_order_dir
      if (is.null(dir) || !dir %in% c('asc','desc')) dir <- 'asc'
      DT::datatable(
        as.data.frame(dt, stringsAsFactors = FALSE),
        escape = FALSE,
        selection = "none",
        rownames = FALSE,
        options = list(
          pageLength = 50,
          order = list(list(ord_idx - 1, dir))
        )
      )
    },
    server = FALSE
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
      inputId = "input_drop_players",
      selected = g.r.dt.pros[
        (id %in% r.v.button.dropped$id) |
        (Player %in% input$input_drop_players)
      ]$Player
    )
  })

}