#' Runs app to upload .wav file and transofrm into csv
#'
#' @import tuneR
#' @import seewave
#' @import shinyjs
#'
#' @export

run_transform_app <- function() {

  # Increase file size limit to 400MB
  options(shiny.maxRequestSize = 400 * 1024^2)
  # Removed parallel processing with future package

  ui <- fluidPage(
    useShinyjs(),
    titlePanel("WAV File Feature Extractor"),
    sidebarLayout(
      sidebarPanel(
        fileInput("wav_file", "Choose WAV File", accept = c(".wav", ".wave")),
        actionButton("process", "Process File", class = "btn-primary"),
        br(), br(),
        downloadButton("download", "Download CSV", class = "btn-success")
      ),
      mainPanel(
        h4("File Details"),
        verbatimTextOutput("file_datetime"),
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

    # Disable buttons initially
    observe({
      shinyjs::disable("process")
      shinyjs::disable("download")
    })

    # Observe file input changes
    observeEvent(input$wav_file, {
      req(input$wav_file)

      filename <- input$wav_file$name
      timestamp_str <- str_extract(filename, "(?<=\\.)\\d{12}(?=\\.wav$)")

      # Store file info in reactive value for later use
      file_info(list(
        name = filename,
        path = input$wav_file$datapath,
        size = input$wav_file$size,
        timestamp = timestamp_str
      ))

      if(!is.na(timestamp_str)) {
        valid_file(TRUE)
        output$file_datetime <- renderPrint({
          month_num <- as.integer(substr(timestamp_str, 3, 4))
          month_name <- month.name[month_num]
          cat("month:", month_name, ", ")
          cat("day:", substr(timestamp_str, 5, 6), ", ")
          year_full <- 2000 + as.integer(substr(timestamp_str, 1, 2))
          cat("year:", year_full, "\n")
          cat("hour:", substr(timestamp_str, 7, 8), ", ")
          cat("minute:", substr(timestamp_str, 9, 10), "\n")


          cat("File size:", round(input$wav_file$size/1024/1024, 2), "MB\n")
        })
        shinyjs::enable("process")
      } else {
        valid_file(FALSE)
        output$file_datetime <- renderPrint({
          cat("Invalid filename format\n")
        })
        shinyjs::disable("process")
      }
    })

    observeEvent(input$process, {
      req(valid_file())
      req(file_info())

      shinyjs::disable("process")
      shinyjs::disable("download")

      # Show a processing message
      output$data_preview <- renderPrint({
        cat("Processing audio file. This may take a while for large files...\n")
      })

      # Get file path from our reactive value
      file_path <- file_info()$path

      # Direct processing without using future/promises
      result <- tryCatch({
        file_audio <- readWave(file_path)
        audio2 <- melfcc(file_audio, numcep = 13, maxfreq = 600, wintime = 0.2, hoptime = 0.1)

        as.data.frame(audio2) %>%
          mutate(
            time_start = seq(0, by = 0.1, length.out = nrow(audio2)),
            time_end = time_start + 0.1
          )
      }, error = function(e) {
        return(paste("Error:", e$message))
      })

      # Handle the result
      processed_data(result)

      if (is.data.frame(result)) {
        output$data_preview <- renderPrint({
          cat("Data dimensions:", dim(result)[1], "rows ×", dim(result)[2], "columns\n")
          cat("\nColumn names:\n")
          cat(paste(names(result), collapse = ", "), "\n")

          # Display first few rows if available
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
