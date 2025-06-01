# app.R

# --- 1. Load Libraries ---
library(shiny)
library(DT)
library(xgboost)
library(shinydisconnect)
library(caret) # <--- ADD THIS LIBRARY! This is essential if your model is from caret::train

# --- 2. Load Your Machine Learning Model (Load once, globally) ---
# Ensure 'xgb_model.rds' is in the same directory as app.R when deploying!
xgb_model <- tryCatch({
    readRDS("xgb_model.rds")
}, error = function(e) {
    message(paste("Error loading model: ", e$message,
                  "\nPlease ensure 'xgb_model.rds' is in the same directory as app.R"))
    stop("Failed to load prediction model. Please check 'xgb_model.rds'.")
})

# Function to predict popularity - UPDATED!
predict_popularity <- function(new_song_features) {
    # Caret's predict method for 'train' objects often expects:
    # 1. A data.frame for newdata (not necessarily a matrix for most models trained with caret)
    # 2. type = "raw" for actual predictions, or "prob" for probabilities if classification
    
    # Ensure input_df has correct column names and order as expected by the model
    # The original new_song_features_df created in observeEvent is already a data.frame,
    # so no need for as.matrix() here if your model expects a data.frame.
    
    # If your caret model expects specific factor levels for categorical variables,
    # ensure new_song_features_df has those levels. (Though your inputs are numeric).
    
    # Assuming your caret model output is a continuous prediction (popularity score)
    # and that it expects a data.frame input.
    predict(xgb_model, newdata = new_song_features) # <--- Removed as.matrix() if caret model
}

# Initialize the results table structure (remains the same)
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

# --- UI Definition (remains the same) ---
ui <- fluidPage(
    shinydisconnect::disconnectMessage(
        text = "It looks like our connection was lost. Please refresh the page.",
        refresh = "Refresh Now",
        top = 50,
        size = 22,
        background = "#F8F8F8",
        colour = "#333333",
        overlayColour = "#000000",
        overlayOpacity = 0.7,
        refreshColour = "#007bff"
    ),
    
    titlePanel("Spotify Song Popularity Predictor"),
    
    tags$head(
        tags$link(rel = "icon", type = "image/png", href = "icona.png"),
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
        tags$script(src = "script.js")
    ),
    
    div(class = "input-section-container",
        h3("Enter Song Features"),
        fluidRow(
            column(4, numericInput("daily_rank", "Daily Rank (1-200):", value = 1, min = 1, max = 200)),
            column(4, numericInput("days_out", "Days out (1-3000):", value = 1, min = 1, max = 3000)),
            column(4, numericInput("market_count", "Market Counts (1-72):", value = 1, min = 1, max = 72))
        ),
        fluidRow(
            column(4, numericInput("artist_count", "No. of Artists (1-10):", value = 1, min = 1, max = 10)),
            column(4, numericInput("duration_min", "Song Duration (minutes, 1-20):", value = 3, min = 1, max = 20)),
            column(4, numericInput("daily_movement", "Daily Movement (-100 to 100):", value = 0, min = -100, max = 100)),
            column(4, numericInput("weekly_movement", "Weekly Movement (-300 to 300):", value = 0, min = -300, max = 300))
        ),
        fluidRow(
            column(4, numericInput("mode", "Mode (0=Minor, 1=Major):", value = 1, min = 0, max = 1)),
            column(4, numericInput("is_explicit", "Is Explicit? (0=No, 1=Yes):", value = 0, min = 0, max = 1)),
            column(4, numericInput("danceability", "Danceability (0-1):", value = 0.5, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("energy", "Energy (0-1):", value = 0.5, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("loudness", "Loudness (dB, -60 to 0):", value = -10, min = -60, max = 0, step = 0.1)),
            column(4, numericInput("speechiness", "Speechiness (0-1):", value = 0.1, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("acousticness", "Acousticness (0-1):", value = 0.5, min = 0, max = 1, step = 0.01))
        ),
        fluidRow(
            column(4, numericInput("instrumentalness", "Instrumentalness (0-1):", value = 0.0, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("liveness", "Liveness (0-1):", value = 0.1, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("valence", "Valence (0-1):", value = 0.5, min = 0, max = 1, step = 0.01)),
            column(4, numericInput("time_signature", "Time Signature (1-5):", value = 4, min = 1, max = 5)),
            column(4, numericInput("key", "Key (0-11):", value = 0, min = 0, max = 11)),
            column(4, numericInput("tempo", "Tempo (BPM, 40-220):", value = 120, min = 40, max = 220))
        )
    ),
    
    hr(),
    
    fluidRow(
        column(12, align = "center",
               actionButton("predict", "Predict Popularity", class = "btn-primary"),
               actionButton("reset", "Reset Inputs", class = "btn-reset"),
               actionButton("clear", "Clear Results", class = "btn-clear")
        )
    ),
    
    hr(),
    
    h3("Prediction Results"),
    p("Enter song features above and click 'Predict Popularity'. Your predictions will appear below."),
    fluidRow(
        column(12,
               DT::dataTableOutput("results_table"),
               br(),
               actionButton("copy_table", "Copy Table to Clipboard", class = "btn-info")
        )
    ),
    textOutput("keepAlive", container = tags$span)
)

# --- Server Logic (minor adjustment for `predict_popularity` call) ---
server <- function(input, output, session) {
    results_table_reactive <- reactiveVal(initial_table)
    
    output$keepAlive <- renderText({
        invalidateLater(30000, session)
        "Keep alive"
    })
    
    # Your existing server-side file existence and JS status checks (keeping for now)
    output$file_exist_status <- renderUI({
        css_path <- file.path("www", "style.css")
        js_path <- file.path("www", "script.js")
        css_exists <- file.exists(css_path)
        js_exists <- file.exists(js_path)
        if (css_exists && js_exists) {
            HTML(paste0("<p style='color: green;'>&#10004; CSS and JS files found on server (<code>",
                        css_path, "</code>, <code>", js_path, "</code>)</p>"))
        } else {
            HTML(paste0("<p style='color: red;'>&#10008; Missing files on server: ",
                        ifelse(!css_exists, paste0("<code>", css_path, "</code>"), ""),
                        ifelse(!js_exists, paste0("<code>", js_path, "</code>"), ""), "</p>"))
        }
    })
    js_loaded <- reactiveVal(FALSE)
    observeEvent(input$js_loaded_message, {
        js_loaded(TRUE)
        output$js_status <- renderUI({
            HTML("<p style='color: green;'>&#10004; <code>script.js</code> appears to be loading and executing.</p>")
        })
    }, once = TRUE)
    output$css_status <- renderUI({
        if (js_loaded()) {
            HTML("<p style='color: orange;'>&#9888; CSS file loaded but appearance not confirmed. <br>Check browser dev tools for styles (Elements tab) and console for errors.</p>")
        } else {
            HTML("<p style='color: gray;'>&#9679; CSS status pending JS execution.</p>")
        }
    })
    
    observeEvent(input$reset, {
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
    
    observeEvent(input$clear, {
        results_table_reactive(initial_table)
    })
    
    observeEvent(input$predict, {
        req(
            input$daily_rank, input$days_out, input$market_count, input$artist_count,
            input$duration_min, input$daily_movement, input$weekly_movement,
            input$mode, input$is_explicit, input$danceability, input$energy,
            input$loudness, input$speechiness, input$acousticness, input$instrumentalness,
            input$liveness, input$valence, input$time_signature, input$key, input$tempo
        )
        
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
        
        prediction_result <- NULL
        
        withProgress(message = 'Predicting popularity...', value = 0, {
            incProgress(0.2, detail = "Gathering features...")
            
            # Ensure column names match and order matches your model's training data.
            # `caret` is generally robust if names match, even if order differs slightly,
            # but it's best practice to keep them consistent.
            
            incProgress(0.5, detail = "Applying model...")
            
            prediction_result <- tryCatch({
                predict_popularity(new_song_features_df) # <--- This function call is now using the updated predict_popularity
            }, error = function(e) {
                warning(paste("Error during prediction:", e$message))
                showNotification(
                    paste("Prediction failed:", e$message),
                    type = "error", duration = 10
                )
                return(NA)
            })
            
            incProgress(0.9, detail = "Updating results...")
        })
        
        if (!is.null(prediction_result) && !is.na(prediction_result)) {
            new_song_features_df$Predicted_Popularity <- prediction_result
            
            current_table <- results_table_reactive()
            updated_table <- rbind(current_table, new_song_features_df)
            results_table_reactive(updated_table)
            
            session$sendCustomMessage(type = "scroll_to_results", message = list(id = "results_table"))
        }
    })
    
    output$results_table <- DT::renderDataTable({
        results_table_reactive()
    }, options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE,
        info = FALSE
    ))
    
    observeEvent(input$copy_table, {
        session$sendCustomMessage(type = "copy_table", message = list(id = "results_table"))
        showNotification("Table copied to clipboard!", type = "message", duration = 3)
    })
}

shinyApp(ui = ui, server = server)