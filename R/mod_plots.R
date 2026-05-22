#' Shiny module providing GUI for the plot tab
#'
#' @param id Character string module namespace
#' @export
plots_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(
        inputId = ns("var"),
        label = "Variabel:",
        c("PatientAge", "oppfolging_nav_frekvens")
      )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(ns("Figur"), shiny::plotOutput(ns("distPlot"))),
        shiny::tabPanel(ns("Tabell"), DT::DTOutput(ns("distTable")))
      )
    )
  )
}

#' Shiny module providing server logic for the plot tab
#'
#' @param id Character string module namespace
#' @export
plots_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Last inn data
      basisData <- shiny::reactive({
        shiny::req(input$var)
        getBasisData(input$var)
      })

      # Figur og tabell
      # Figur
      output$distPlot <- shiny::renderPlot({
        makeHist(df = basisData(), var = input$var)
      })

      # Tabell
      output$distTable <- DT::renderDT({
        tableData <- basisData() |>
          dplyr::group_by(.data[[input$var]]) |>
          dplyr::summarise(Antall = dplyr::n()) |>
          dplyr::arrange(.data[[input$var]])
        DT::datatable(tableData)
      })
    }
  )
}

#' Run plots module as a standalone Shiny app
#'
#' Convenience wrapper to launch the plots module for development and testing.
#' @export
plotsApp <- function() {
  ui <- shiny::fluidPage(
    plots_ui("test")
  )
  server <- function(input, output, session) {
    plots_server("test")
  }
  shiny::shinyApp(ui, server)
}
