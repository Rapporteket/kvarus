#' Shiny module providing GUI and server logic for the subscription v2 tab
#'
#' @param id Character string module namespace
NULL

abonnementV2_ui <- function(id) {
  ns <- shiny::NS(id)


}

abonnementV2_server <- function(i) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      
    }
  )
}