#' Shiny module providing GUI and server logic for the report tab
#'
#' @param id Character string module namespace
NULL

samlerapport_ui <- function(id) {
  ns <- shiny::NS(id)


}

samlerapport_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
    }
  )
}