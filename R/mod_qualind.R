#' UI Module for Kvalitetsindikator
#'
#' This function defines the UI for the "Kvalitetsindikator" module.
#'
#' @param id A unique identifier for the module namespace.
#'
#' @return A Shiny UI element for the module.
#'
#' @details This module is designed to create the user interface for the
#' "Kvalitetsindikator" functionality within the application. The `id` parameter
#' is used to create a namespace for the module, ensuring that its UI elements
#' are uniquely identified within the Shiny application.
#' @export

module_kvalitetsindikator_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput( # First select
          inputId = ns("kval_var"),
          label = "Velg Kvalitetsindikator:",
          choices = c(
            "Behandlingsplan på plass tidlig i forløpet" = "behandlingsplan",
            "Kriseplan på plass tidlig i forløpet" = "kriseplan",
            "Opplevd stort utbytte" = "utbytte",
            "Avsluttet behandling med gjensidig avtale" = "gjensidig"
          ),
          selected = "behandlingsplan"
        )
      ),

      shiny::mainPanel(
        shiny::strong(
          shiny::textOutput(
            outputId = ns("text_header")
          )
        ),
        shiny::htmlOutput(
          outputId = ns("text_body")
        ),
        bslib::navset_card_underline(
          bslib::nav_panel("Figur",
                           shiny::plotOutput(outputId = ns("kval_plot"), height = "auto"),
                           shiny::downloadButton(ns("download_fig"), "Last ned figur")),
          bslib::nav_panel("Tabell",
                           DT::DTOutput(outputId = ns("kval_table")))
        )
      )
    )
  )
}


#' Server Module for Quality Indicators
#'
#' This function defines the server logic for the Quality Indicators module.
#'
#' @param id A unique identifier for the module namespace.
#'
#' @return A Shiny server function for the Quality Indicators module.
#'
#' @details
#' This module is designed to handle the server-side logic for displaying
#' and interacting with quality indicators in the application.
#'
#'@export

module_qualind_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ### Load in data ###
      punktData <- getTimepointData()

      ### Clean and tidy data ###
      punktData <- prePros(punktData)

      # Count quality indicator:
      kval_df_reactive <- shiny::reactive({
        kval_count(punktData, input$kval_var)
      })




      ###### PLOT ####################################################################
      # Make labs for ggplot:
      ggdata_reactive <- shiny::reactive({
        makeGGdata(input$kval_var, "kval")
      })

      # Make annotations for plot:
      anno_reactive <- shiny::reactive({
        annotations(input$kval_var)
      })

      # Make plot:
      kval_plot_reactive <- shiny::reactive({
        kval_plot(kval_df_reactive(), ggdata_reactive(), anno_reactive())
      })

      output$kval_plot <- shiny::renderPlot({
        kval_plot_reactive()
      }, height = function() {
        # Dynamisk høyde basert på antall sykehus
        n_sykehus <- nrow(kval_df_reactive())
        height_per_sykehus <- 20 # Ekstra høyde per sykehus
        total_height <- (n_sykehus * height_per_sykehus)
        return(max(total_height, 400)) # Sørg for at det ikke blir mindre enn basishøyden
      })

      ####### TABLE ##################################################################

      output$kval_table <- DT::renderDT(
        {
          DT::datatable(
            kval_df_reactive(),
            extensions = "Buttons",
            options = list(dom = "Bfrtip",
                           buttons = c("copy", "csv", "excel", "pdf")),
            class = "white-space:nowrap compact",
            colnames = c("Sykehus",
                         "Antall nasjonalt",
                         "Antall per sykehus",
                         "Antall - kvalitetsindikator",
                         "Andel - kvalitetsindikator")
          )
        }
      )

      ###### NEDLASTING FIGUR/TABELL #################################################

      output$download_fig <-  shiny::downloadHandler(
        filename = function() {
          paste("Figur_", input$kval_var, "_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, onefile = TRUE, width = 15, height = 9)
          plot(kval_plot_reactive())
          dev.off()
        }
      )

      output$download_tbl <- shiny::downloadHandler(
        filename = function() {
          paste("Tabell_", input$kval_var, "_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(kval_df_reactive(), file)
        }
      )

      ####### EXPLANATION OF CALCULATION QUALIND #####################################

      output$text_header <- shiny::renderText({
        data <- explanation_kvalind(input$kval_var)
        data$header
      })

      output$text_body <- shiny::renderText({
        data <- explanation_kvalind(input$kval_var)
        data$text
      })

    }
  )
}
