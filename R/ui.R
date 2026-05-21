#' Client (ui) for the kvarus app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  regTitle <- "kvarus"

  shiny::tagList(
    shiny::navbarPage(
      title = rapbase::regTitle(regTitle),
      windowTitle = regTitle,
      theme = rapbase::rapTheme(version = 5),
      id = "tabs",
      shiny::tabPanel(
        "Informasjon",
        info_ui("info"),
        rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE)
      ),
      shiny::tabPanel(
        "Figur og tabell",
        plots_ui("plots")
      )
    ) # navbarPage
  ) # tagList
}
