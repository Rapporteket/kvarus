#' Server logic for the kvarus app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

app_server <- function(input, output, session) {

  # data.frame som mapper ReshID og sykehusnavn
  map_orgname <- data.frame(
    UnitId = c(111, 222, 333),
    orgname = c("Sykehus 1", "Sykehus 2", "Sykehus 3")
  )
  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "kvarus",
    map_orgname = map_orgname,
    caller = "kvarus"
  )

  info_server("info", user = user)

  ################
  # SC user tabs #
  ################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        message("Removing export tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Eksport")
      } else {
        message("Adding export tab for user with role ", user$role())
        shiny::appendTab(
          "tabs",
          shiny::tabPanel(
            "Eksport",
            shiny::sidebarLayout(
              shiny::sidebarPanel(
                rapbase::exportUCInput("export")
              ),
              shiny::mainPanel(
                rapbase::exportGuideUI("exportGuide")
              )
            )
          )
        )
      }
    }
  )

  ###############
  # Export data #
  ###############

  ## brukerkontroller
  rapbase::exportUCServer(
    "export",
    dbName = "data",
    teamName = Sys.getenv("SHINYPROXY_APPID", unset = "unknown")
  )
  ## veiledning
  rapbase::exportGuideServer("exportGuide", dbName = "data")
}
