#' Runs app to predict times of Grey Whale calls
#'
#' @import shiny
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
        fileInput("file", "Choose a CSV File",
                  accept = c(".csv")),
        selectInput("method", "Prediction Type",
                    choices = c("wide", "precise", "both")),
        downloadButton("download_table", "Download Selection Table"),
        actionButton("process", "Get Predictions")
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

    observeEvent(input$process, {
      req(input$file)

      file_name <- input$file$name
      save_file <- input$annotated
      df <- fread(input$file$datapath)

      df$rf.pred.mel <- predict(rf_melfcc, new_data = df)$.pred_class

      if (input$method == "wide") {
        wide_table <- to_selection_table(df, file_name, i = 350)
        wide_table_reactive(wide_table)
        output$table_ui <- renderUI({
          tagList(
            h3("Wide Predictions"),
            tableOutput("selection_table")
          )
        })
        output$selection_table <- renderTable({ wide_table })

      } else if (input$method == "precise") {
        precise_table <- to_selection_table(df, file_name, i = 50)
        precise_table_reactive(precise_table)
        output$table_ui <- renderUI({
          tagList(
            h3("Precise Predictions"),
            tableOutput("selection_table")
          )
        })
        output$selection_table <- renderTable({ precise_table })

      } else {  # both
        wide_table <- to_selection_table(df, file_name, i = 350)
        precise_table <- to_selection_table(df, file_name, i = 50)
        precise_table_reactive(precise_table)
        wide_table_reactive(wide_table)

        output$table_ui <- renderUI({
          tabsetPanel(
            id = "active_tab",
            tabPanel("Wide",
                     h4("Wide Predictions"),
                     tableOutput("wide_table")),
            tabPanel("Precise",
                     h4("Precise Predictions"),
                     tableOutput("precise_table"))
          )
        })

        output$wide_table <- renderTable({ wide_table })
        output$precise_table <- renderTable({ precise_table })
      }
    })

    output$download_table <- downloadHandler(
      filename = function() {
        file_name <- sub("\\.csv$", "", input$file$name)

        method <- input$method
        if (method == "both") {
          tab <- input$active_tab
          return(paste0(tolower(tab), ".", file_name, ".txt"))
        } else {
          return(paste0(method, ".", file_name, ".txt"))
        }
      },
      content = function(file) {
        method <- input$method
        if (method == "wide") {
          write.csv(wide_table_reactive(), file, row.names = FALSE)
        } else if (method == "precise") {
          write.csv(precise_table_reactive(), file, row.names = FALSE)
        } else if (method == "both") {
          tab <- input$active_tab
          if (tab == "Wide") {
            write.csv(wide_table_reactive(), file, row.names = FALSE)
          } else if (tab == "Precise") {
            write.csv(precise_table_reactive(), file, row.names = FALSE)
          }
        }
      }
    )


  } # end server

  # LOAD APP ---------------------------------------------------------------------
  shinyApp(ui = ui, server = server)

} # end function
