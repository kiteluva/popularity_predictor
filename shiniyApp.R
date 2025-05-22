library(shiny)
library(DT)
library(xgboost)

# --- Model Loading with Error Handling ---
xgb_model <- tryCatch({
  readRDS("xgb_model.rds")
}, error = function(e) {
  message(paste("Error loading model: ", e$message,
                "\nPlease ensure 'xgb_model.rds' is in the same directory as app.R"))
  NULL
})

# Function to predict popularity
predict_popularity <- function(new_song_features) {
  if (!is.null(xgb_model)) {
    predict(xgb_model, newdata = as.matrix(new_song_features))
  } else {
    warning("Model not loaded. Cannot make predictions.")
    rep(NA, nrow(new_song_features))
  }
}

# Initialize the results table with empty columns for all features + prediction
initial_table <- data.frame(
  daily_rank = numeric(),
  days_out = numeric(),
  market_count = numeric(),
  artist_count = numeric(),
  duration_min = numeric(),
  daily_movement = numeric(),
  weekly_movement = numeric(),
  mode = numeric(),
  is_explicit = numeric(),
  danceability = numeric(),
  energy = numeric(),
  loudness = numeric(),
  speechiness = numeric(),
  acousticness = numeric(),
  instrumentalness = numeric(),
  liveness = numeric(),
  valence = numeric(),
  time_signature = numeric(),
  key = numeric(),
  tempo = numeric(),
  Predicted_Popularity = numeric()
)

ui <- fluidPage(
  titlePanel("Spotify Song Popularity Predictor"),

  # Link to custom CSS and JavaScript files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js") # This links your JavaScript file
  ),

  sidebarLayout(
    sidebarPanel(
      h3("Enter Song Features"),

      # Input fields for song features
      numericInput("daily_rank", "Daily Rank (1-200):", value = 1, min = 1, max = 200),
      numericInput("days_out", "Days out (1-3000):", value = 1, min = 1, max = 3000),
      numericInput("market_count", "Market Counts (1-72):", value = 1, min = 1, max = 72),
      numericInput("artist_count", "No. of Artists (1-10):", value = 1, min = 1, max = 10),
      numericInput("duration_min", "Song Duration (minutes, 1-20):", value = 3, min = 1, max = 20),
      numericInput("daily_movement", "Daily Movement (-100 to 100):", value = 0, min = -100, max = 100),
      numericInput("weekly_movement", "Weekly Movement (-300 to 300):", value = 0, min = -300, max = 300),

      # Binary inputs
      numericInput("mode", "Mode (0=Minor, 1=Major):", value = 1, min = 0, max = 1),
      numericInput("is_explicit", "Is Explicit? (0=No, 1=Yes):", value = 0, min = 0, max = 1),

      # Audio feature inputs (0-1 range typically)
      numericInput("danceability", "Danceability (0-1):", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("energy", "Energy (0-1):", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("loudness", "Loudness (dB, -60 to 0):", value = -10, min = -60, max = 0, step = 0.1),
      numericInput("speechiness", "Speechiness (0-1):", value = 0.1, min = 0, max = 1, step = 0.01),
      numericInput("acousticness", "Acousticness (0-1):", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("instrumentalness", "Instrumentalness (0-1):", value = 0.0, min = 0, max = 1, step = 0.01),
      numericInput("liveness", "Liveness (0-1):", value = 0.1, min = 0, max = 1, step = 0.01),
      numericInput("valence", "Valence (0-1):", value = 0.5, min = 0, max = 1, step = 0.01),

      # Other numerical inputs
      numericInput("time_signature", "Time Signature (1-5):", value = 4, min = 1, max = 5),
      numericInput("key", "Key (0-11):", value = 0, min = 0, max = 11),
      numericInput("tempo", "Tempo (BPM, 40-220):", value = 120, min = 40, max = 220),

      # Action buttons
      actionButton("predict", "Predict Popularity", class = "btn-primary"),
      actionButton("reset_input", "Reset Inputs"),
      actionButton("reset_table", "Clear Results")
    ),

    mainPanel(
      h3("Prediction Results"),
      p("Enter song features in the sidebar and click 'Predict Popularity'. Your predictions will appear below."),
      DT::dataTableOutput("results_table"),
      br(), # Add a line break for spacing
      actionButton("copy_table", "Copy Table to Clipboard", class = "btn-info") # Added Copy button
    )
  )
)

server <- function(input, output, session) {
  results_table <- reactiveVal(initial_table)

  # Observe event for 'Reset Inputs' button
  observeEvent(input$reset_input, {
    # Reset all numeric inputs to their default values
    updateNumericInput(session, "daily_rank", value = 1)
    updateNumericInput(session, "days_out", value = 1)
    updateNumericInput(session, "market_count", value = 1)
    updateNumericInput(session, "artist_count", value = 1)
    updateNumericInput(session, "duration_min", value = 3)
    updateNumericInput(session, "daily_movement", value = 0)
    updateNumericInput(session, "weekly_movement", value = 0)
    updateNumericInput(session, "mode", value = 1)
    updateNumericInput(session, "is_explicit", value = 0)
    updateNumericInput(session, "danceability", value = 0.5)
    updateNumericInput(session, "energy", value = 0.5)
    updateNumericInput(session, "loudness", value = -10)
    updateNumericInput(session, "speechiness", value = 0.1)
    updateNumericInput(session, "acousticness", value = 0.5)
    updateNumericInput(session, "instrumentalness", value = 0.0)
    updateNumericInput(session, "liveness", value = 0.1)
    updateNumericInput(session, "valence", value = 0.5)
    updateNumericInput(session, "time_signature", value = 4)
    updateNumericInput(session, "key", value = 0)
    updateNumericInput(session, "tempo", value = 120)
  })

  # Observe event for 'Clear Results' button
  observeEvent(input$reset_table, {
    results_table(initial_table)
  })

  # Observe event for 'Predict Popularity' button
  observeEvent(input$predict, {
    new_song_features_df <- data.frame(
      daily_rank = input$daily_rank,
      days_out = input$days_out,
      market_count = input$market_count,
      artist_count = input$artist_count,
      duration_min = input$duration_min,
      daily_movement = input$daily_movement,
      weekly_movement = input$weekly_movement,
      mode = input$mode,
      is_explicit = input$is_explicit,
      danceability = input$danceability,
      energy = input$energy,
      loudness = input$loudness,
      speechiness = input$speechiness,
      acousticness = input$acousticness,
      instrumentalness = input$instrumentalness,
      liveness = input$liveness,
      valence = input$valence,
      time_signature = input$time_signature,
      key = input$key,
      tempo = input$tempo
    )

    # 2. Perform prediction (potentially with a progress indicator)
    prediction_result <- NULL
    withProgress(message = 'Predicting popularity...', value = 0.5, {
      prediction_result <- predict_popularity(new_song_features_df)
    })

    # 3. Add the predicted popularity to the collected features
    new_song_features_df$Predicted_Popularity <- prediction_result

    # 4. Update the main results table
    current_table <- results_table()
    updated_table <- rbind(current_table, new_song_features_df)
    results_table(updated_table)

    # Send a custom message to trigger scrolling after table update
    session$sendCustomMessage(type = "scroll_to_results", message = list(id = "results_table"))
  })

  # Render the data table using the reactive results_table
  output$results_table <- DT::renderDataTable({
    results_table()
  }, options = list(
    pageLength = 10,
    scrollX = TRUE,
    searching = FALSE,
    info = FALSE
  ))
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
