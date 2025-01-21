####### MODULE FOR KVALITETSINDIKATORER ########################################

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
            "Opplevd stort utbytte" = "utbytte"
          ),
          selected = "behandlingsplan"
        )
      ),

      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Figur",
                           shiny::plotOutput(outputId = ns("kval_plot")),
                           shiny::downloadButton(ns("download_fig"), "Last ned figur")),
          bslib::nav_panel("Tabell",
                           DT::DTOutput(outputId = ns("kval_table"))
        )),

        bslib::navset_card_underline(
          title = shiny::h4("Slik er kvalitetsindikatoren regnet ut:"),
          bslib::card_header(
            htmltools::tags$em(
              shiny::textOutput(
                outputId = ns("text_header")
              )
            )
          ),
          bslib::card_body(
            shiny::htmlOutput(
              outputId = ns("text_body")
            )
          )
        )
      )
    )
  )
}



# Server-part

#'@export

module_kvalitetsindikator_server <- function(id) {
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
      })

      ####### TABLE ##################################################################

      output$kval_table <- DT::renderDT(
        {
          DT::datatable(
            kval_df_reactive(),
            extensions = "Buttons",
            class = "white-space:nowrap compact",
            colnames = c("Sykehus",
                         "Antall nasjonalt",
                         "Antall per sykehus",
                         "Antall - kvalitetsindikator",
                         "Andel - kvalitetsindikator"),
            options = list(paging = TRUE,
                           scrollX=TRUE,
                           searching = TRUE,
                           ordering = TRUE,
                           dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf'),
                           pageLength=30,
                           lengthMenu=c(3,5,10)
                           )
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
