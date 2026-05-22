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
#' @param user A reactive expression containing user information, including organization ID
#'
#' @export
plots_server <- function(id, user) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Last inn data
      basisData <- shiny::reactive({
        shiny::req(c(input$var, user$org()))
        getBasisData(input$var, user$org())
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
    user <- rapbase::navbarWidgetServer2(
      "navbar-widget", "kvarus"
    )
    plots_server("test", user)
  }
  shiny::shinyApp(ui, server)
}
