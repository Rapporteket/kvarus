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
  unitAccessTree <- Sys.getenv("MRS_ACCESS_HIERARCHY_URL", unset = "noTree")
  if (unitAccessTree != "noTree") {
    units <- jsonlite::fromJSON(unitAccessTree)$AccessUnits
    map_orgname <- data.frame(
      UnitId = units$UnitId,
      orgname = units$TitleWithPath
    )
  } else {
    map_orgname <- NULL
  }

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "kvarus",
    map_orgname = map_orgname,
    caller = "kvarus"
  )

  info_server("info", user = user)
  plots_server("plots", user = user)

  ################
  # SC user tabs #
  ################

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != "SC") {
        message("Removing export tab for user with role ", user$role())
        shiny::removeTab("tabs", target = "Kvalitetsindikatorer")
        shiny::removeTab("tabs", target = "Eksport")
      } else {
        message("Adding kvalitetsindikatorer tab for user with role ", user$role())
        module_qualind_server("kval1")
        shiny::appendTab(
          "tabs",
          shiny:: tabPanel(
            title = "Kvalitetsindikatorer",
            module_kvalitetsindikator_ui("kval1")
          )
        )
        message("Adding export tab for user with role ", user$role())
        ## brukerkontroller
        rapbase::exportUCServer(
          "export",
          dbName = "data",
          teamName = Sys.getenv("SHINYPROXY_APPID", unset = "unknown")
        )
        ## veiledning
        rapbase::exportGuideServer("exportGuide", dbName = "data")
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
}
