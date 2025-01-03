#' Shiny module providing GUI and server logic for the dispatch tab
#'
#' @param id Character string module namespace
NULL

utsending_ui <- function(id) {
  ns <- shiny::NS(id)


}

utsending_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
    }
  )
}