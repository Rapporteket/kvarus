#' Shiny module providing GUI and server logic for the subscription tab
#'
#' @param id Character string module namespace
NULL

abonnement_ui <- function(id) {
  ns <- shiny::NS(id)


}

abonnement_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
    }
  )
}