#' Shiny module providing GUI for the subscription tab
#'
#' @param id Character string module namespace
#' @export
abonnement_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      rapbase::autoReportInput(ns("testSubscription"))
    ),
    shiny::mainPanel(
      rapbase::autoReportUI(ns("testSubscription"))
    )
  )
}

#' Shiny module providing server logic for the subscription tab
#'
#' @param id Character string module namespace
#' @export
abonnement_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ## nye abonnement
      ## Objects currently shared among subscription and dispathcment
      orgs <- list(Sykehus1 = 1234,
                   Sykehus2 = 4321)
      reports <- list(
        Samlerapport1 = list(
          synopsis = "Automatisk samlerapport1",
          fun = "samlerapport1Fun",
          paramNames = c("p1", "p2"),
          paramValues = c("Alder", 1)
        ),
        Samlerapport2 = list(
          synopsis = "Automatisk samlerapport2",
          fun = "samlerapport2Fun",
          paramNames = c("p1", "p2"),
          paramValues = c("BMI", 1)
        )
      )

      ## Subscription
      rapbase::autoReportServer(
        id = "testSubscription", registryName = "kvarus",
        type = "subscription", reports = reports, orgs = orgs, freq = "quarter"
      )
    }
  )
}
