#' Runs app to predict times of Grey Whale calls
#'
#' @import shiny
#' @importFrom data.table fread
#' @export

run_prediction_app <- function() {
  ## APP -------------------------------------------------------------------------

  options(shiny.maxRequestSize = 30*1024^2)  # 30 MB max
  rf_melfcc <- rf_melfcc_small

  ## User Intterface -------------------------------------------------------------
  ui <- fluidPage(
    titlePanel("Grey Whale Model Predictions"),
    sidebarLayout(

      sidebarPanel(
        tags$img(
          src = "https://media.giphy.com/media/FaKV1cVKlVRxC/giphy.gif",
          height = "200px",
          style = "margin-top:10px;"
        ),
        helpText(HTML(
          "Instructions:<br>
         1. Upload a .csv file containing your transformed acoustic data<br>
         2. Then choose the prediction type and click 'Get Predictions'<br>
         3. Once the prediction is complete, the corresponding download button will appear<br>
         4. Click download and save to a familiar place<br>
         5. Open selection table and corresponding .wav file in Raven"
        )),

        fileInput("file", "Choose a CSV File", accept = c(".csv")),
        selectInput("method", "Prediction Type", choices = c("wide", "precise", "both")),

        actionButton("process", "Get Predictions"),

        conditionalPanel(
          condition = "output.showWideDownload",
          downloadButton("download_wide", "Download Wide Predictions")
        ),

        conditionalPanel(
          condition = "output.showPreciseDownload",
          downloadButton("download_precise", "Download Precise Predictions")
        )

      ),
      mainPanel(
        verbatimTextOutput("status"),
        h3("Selection Table"),
        uiOutput("table_ui")
      )
    )
  ) # end ui

  ## server ----------------------------------------------------------------------
  server <- function(input, output, session) {

    wide_table_reactive <- reactiveVal()
    precise_table_reactive <- reactiveVal()

    # Logic to show/hide download buttons
    output$showWideDownload <- reactive({
      !is.null(wide_table_reactive())
    })
    outputOptions(output, "showWideDownload", suspendWhenHidden = FALSE)

    output$showPreciseDownload <- reactive({
      !is.null(precise_table_reactive())
    })
    outputOptions(output, "showPreciseDownload", suspendWhenHidden = FALSE)

    observeEvent(input$process, {
      req(input$file)

      file_name <- input$file$name
      df <- fread(input$file$datapath)
      df$rf.pred.mel <- predict(rf_melfcc, new_data = df)$.pred_class

      if (input$method == "wide") {
        wide_table <- to_selection_table(df, file_name, i = 350)
        wide_table_reactive(wide_table)
        output$table_ui <- renderUI({
          tagList(
            h4("Summary"),
            tableOutput("summary_table"),
            h4("Wide Predictions"),
            tableOutput("selection_table")
          )
        })
        output$selection_table <- renderTable({ wide_table })
        output$summary_table <- renderTable({
          summarize_selection_table(wide_table)
        })

      } else if (input$method == "precise") {
        precise_table <- to_selection_table(df, file_name, i = 50)
        precise_table_reactive(precise_table)
        output$table_ui <- renderUI({
          tagList(
            h4("Summary"),
            tableOutput("summary_table"),
            h4("Precise Predictions"),
            tableOutput("selection_table")
          )
        })
        output$selection_table <- renderTable({ precise_table })
        output$summary_table <- renderTable({
          summarize_selection_table(precise_table)
        })

      } else {  # both
        wide_table <- to_selection_table(df, file_name, i = 350)
        precise_table <- to_selection_table(df, file_name, i = 50)
        precise_table_reactive(precise_table)
        wide_table_reactive(wide_table)

        output$table_ui <- renderUI({
          tabsetPanel(
            id = "active_tab",
            tabPanel("Wide",
                     h4("Summary"),
                     tableOutput("wide_summary"),
                     h4("Wide Predictions"),
                     tableOutput("wide_table")
            ),
            tabPanel("Precise",
                     h4("Summary"),
                     tableOutput("precise_summary"),
                     h4("Precise Predictions"),
                     tableOutput("precise_table")
            )
          )
        })



        output$wide_table <- renderTable({ wide_table })
        output$wide_summary <- renderTable({ summarize_selection_table(wide_table) })

        output$precise_table <- renderTable({ precise_table })
        output$precise_summary <- renderTable({ summarize_selection_table(precise_table) })

      }
    })

    output$download_wide <- downloadHandler(
      filename = function() {
        paste0("wide.", sub("\\.csv$", "", input$file$name), ".txt")
      },
      content = function(file) {
        write_tsv(wide_table_reactive(), file)
      }
    )

    output$download_precise <- downloadHandler(
      filename = function() {
        paste0("precise.", sub("\\.csv$", "", input$file$name), ".txt")
      },
      content = function(file) {
        write_tsv(precise_table_reactive(), file)
      }
    )
  } # end server

  # LOAD APP ---------------------------------------------------------------------
  shinyApp(ui = ui, server = server)

} # end function
