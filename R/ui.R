#' Client (ui) for the kvarus app
#'
#' @return An shiny app ui object
#' @export

app_ui <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  regTitle <- "kvarus"

  shiny::tagList(
    shiny::navbarPage(
      title = shiny::div(
        shiny::a(shiny::includeHTML(
          system.file("www/logo.svg", package = "rapbase")
        )
        ),
        regTitle
      ),
      windowTitle = regTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",

      shiny::tabPanel(
        "Veiledning",
        veiledning_ui("veiledning")
      ),
      shiny::tabPanel(
        "Figur og tabell",
        plots_ui("plots")
      ),


      ################################################################################
      ##### TAB: Kvalitetsindikatorer ################################################

      shiny:: tabPanel(
        title = "Kvalitetsindikatorer",
        deformitet::module_kvalitetsindikator_UI("kval1")
      ),

      shiny::tabPanel(
        "Samlerapport",
        samlerapport_ui("samlerapport")
      ),
      shiny::tabPanel(
        "Abonnement",
        abonnement_ui("abonnement")
      ),
      shiny::tabPanel(
        shiny::span("Abonnement v2",
                    title = "Bestill tilsending av rapporter p\u00e5 e-post"),
        abonnementV2_ui("abonnementV2")
      ),
      shiny::tabPanel(
        "Utsending",
        utsending_ui("utsending")
      )

    ) # navbarPage
  ) # tagList
}
