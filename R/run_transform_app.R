#' Runs app to upload .wav file and transform into csv
#'
#' @import tuneR
#' @import seewave
#' @import shinyjs
#'
#' @export

run_transform_app <- function() {

  # Increase file size limit to 400MB
  options(shiny.maxRequestSize = 400 * 1024^2)

  ui <- fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        #whale-progress {
          height: 80px;
          margin: 20px 0 5px 0;  /* Added bottom margin for disclaimer */
          position: relative;
          border: 1px solid #ddd;
          border-radius: 5px;
          overflow: hidden;
          background-color: #f5f5f5;
        }
        .disclaimer {
          font-size: 11px;
          color: #777;
          text-align: center;
          margin-top: 0;
          font-style: italic;
        }
        #whale-container {
          width: 100%;
          height: 100%;
          position: relative;
        }
        #whale {
          position: absolute;
          height: 60px;
          bottom: 10px;
          left: 0;
          transition: left 1s ease-out;
        }
        .processing-text {
          position: absolute;
          top: 5px;
          left: 0;
          width: 100%;
          text-align: center;
          font-weight: bold;
          color: #333;
        }
      "))
    ),
    titlePanel("WAV File Feature Extractor"),
    sidebarLayout(
      sidebarPanel(
        fileInput("wav_file", "Choose WAV File", accept = c(".wav", ".wave")),
        actionButton("process", "Process File", class = "btn-primary"),
        br(), br(),
        downloadButton("download", "Download CSV", class = "btn-success"),
        helpText("Note: Processing large files may take several minutes."),
        br(), br(), br(), br(),
        helpText("Instructions"),
        br(),
        helpText("1. Upload a wav file by clicking Browse"),
        helpText("2. Wait for File Details to show up"),
        helpText("3. Click Process File to start the transformation once it turns blue"),
        helpText("4. After Data Preview shows up, click Download CSV to store the file")
      ),

      mainPanel(
        h4("File Details"),
        verbatimTextOutput("file_datetime"),
        h4("Processing Status"),
        div(id = "whale-progress",
            div(id = "whale-container",
                div(class = "processing-text", textOutput("process_text")),
                img(id = "whale", src = "https://www.emoji.co.uk/files/phantom-open-emojis/animals-nature-phantom/12439-whale.png")
            )
        ),
        p(class = "disclaimer", "Note: Whale animation is not time-accurate. Time can vary based on computing power"),
        h4("Data Preview"),
        verbatimTextOutput("data_preview")
      )
    )
  )

  server <- function(input, output, session) {
    # Reactive values
    processed_data <- reactiveVal(NULL)
    valid_file <- reactiveVal(FALSE)
    file_info <- reactiveVal(NULL)

    is_wav_file <- function(filepath) {
      tryCatch({
        # Read first 12 bytes to check WAV header
        con <- file(filepath, "rb")
        header <- readBin(con, "raw", n = 12)
        close(con)

        # Check for RIFF header and WAVE format
        riff_check <- rawToChar(header[1:4]) == "RIFF"
        wave_check <- rawToChar(header[9:12]) == "WAVE"

        return(riff_check && wave_check)
      }, error = function(e) {
        return(FALSE)
      })
    }

    # Disable buttons initially
    observe({
      shinyjs::disable("process")
      shinyjs::disable("download")
      shinyjs::hide("whale-progress")
    })

    # Observe file input changes
    observeEvent(input$wav_file, {
      req(input$wav_file)

      filename <- input$wav_file$name
      filepath <- input$wav_file$datapath

      # First check if it's a valid WAV file
      if(!is_wav_file(filepath)) {
        valid_file(FALSE)
        output$file_datetime <- renderPrint({
          cat("Error: File is not a valid WAV file\n")
          cat("Please select a valid WAV audio file\n")
        })
        shinyjs::disable("process")
        return()
      }

      # File is valid WAV, now try to extract timestamp from filename
      timestamp_str <- str_extract(filename, "(?<=\\.)\\d{12}(?=\\.wav$)")

      # Store file info in reactive value for later use
      file_info(list(
        name = filename,
        path = filepath,
        size = input$wav_file$size,
        timestamp = timestamp_str
      ))

      # Always enable process button for valid WAV files
      valid_file(TRUE)
      shinyjs::enable("process")

      output$file_datetime <- renderPrint({
        if(!is.na(timestamp_str)) {
          # Valid timestamp found - parse and display
          month_num <- as.integer(substr(timestamp_str, 3, 4))
          month_name <- month.name[month_num]
          cat("month:", month_name, ", ")
          cat("day:", substr(timestamp_str, 5, 6), ", ")
          year_full <- 2000 + as.integer(substr(timestamp_str, 1, 2))
          cat("year:", year_full, "\n")
          cat("hour:", substr(timestamp_str, 7, 8), ", ")
          cat("minute:", substr(timestamp_str, 9, 10), "\n")
        } else {
          # No valid timestamp in filename
          cat("Name information cannot be extracted\n")
          cat("Filename does not contain expected timestamp format\n")
        }

        # Always show file size for valid WAV files
        cat("File size:", round(input$wav_file$size/1024/1024, 2), "MB\n")
      })
    })

    observeEvent(input$process, {
      req(valid_file())
      req(file_info())

      shinyjs::disable("process")
      shinyjs::disable("download")
      shinyjs::show("whale-progress")

      # Initialize
      output$process_text <- renderText("Starting processing...")
      shinyjs::runjs("$('#whale').css({'transition': 'left 1s ease-out', 'left': '0%'})")

      # Custom progress updater with variable durations
      update_progress <- function(value, message, duration = 1) {
        js_code <- sprintf(
          "$('#whale').css({'transition': 'left %ds ease-out', 'left': '%d%%'});
        ",
          duration, value*100
        )
        shinyjs::runjs(js_code)
        output$process_text <- renderText(message)
      }

      # Process in stages with custom durations
      result <- tryCatch({
        # Stage 1: Fast (0-10% in 1s)
        update_progress(0.3, "Reading WAV file...", 45)
        file_audio <- readWave(file_info()$path)

        # Stage 2: Slow (10-90% in 60s)
        update_progress(0.9, "Calculating audio features (this may take 30 seconds)...", 100)
        audio2 <- melfcc(file_audio, numcep = 13, maxfreq = 600, wintime = 0.2, hoptime = 0.1)

        # Stage 3: Fast (90-100% in 1s)
        update_progress(1, "Finalizing results...", 1)
        result <- as.data.frame(audio2) %>%
          mutate(
            time_start = seq(0, by = 0.1, length.out = nrow(audio2)),
            time_end = time_start + 0.1
          )

        result
      }, error = function(e) {
        update_progress(1, paste("Error:", e$message), 1)
        return(paste("Error:", e$message))
      })

      # Handle the result
      processed_data(result)

      if (is.data.frame(result)) {
        output$data_preview <- renderPrint({
          cat("Data dimensions:", dim(result)[1], "rows ×", dim(result)[2], "columns\n")
          cat("\nColumn names:\n")
          cat(paste(names(result), collapse = ", "), "\n")
          if(nrow(result) > 0) {
            cat("\nPreview of first 5 rows:\n")
            print(head(result, 5))
          }
        })
        shinyjs::enable("download")
      } else {
        output$data_preview <- renderPrint({ cat(result) })
      }
      shinyjs::enable("process")
    })

    output$download <- downloadHandler(
      filename = function() {
        req(file_info())
        paste0(tools::file_path_sans_ext(file_info()$name), ".csv")
      },
      content = function(file) {
        req(processed_data())
        write.csv(processed_data(), file, row.names = FALSE)
      }
    )
  }

  shinyApp(ui, server)
}
