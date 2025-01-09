#' Shiny module providing GUI and server logic for the plot tab
#'
#' @param id Character string module namespace
NULL

plots_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::selectInput(
        inputId = ns("var"),
        label = "Variabel:",
        c("PatientAge", "oppfolging_nav_frekvens")
      ),
      shiny::sliderInput(
        inputId = ns("bins"),
        label = "Antall grupper:",
        min = 1,
        max = 10,
        value = 5
      )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(ns("Figur"), shiny::plotOutput(ns("distPlot"))),
        shiny::tabPanel(ns("Tabell"), shiny::tableOutput(ns("distTable")))
      )
    )
  )
}

plots_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Last inn data
      basisData <- getBasisData()
      timepointData <- getTimepointData()

      # TODO: Lag en figur som viser andel pasienter i timepointData som har utarbeidet behandlingsplan tidlig i forløpet.
      # For hver pasient-ID (PasientGUID), sorter på registreringstidspunkt (date_mp_beh) og filtrer ut det første tidspunktet per pasient. 
      # Bruk disse radene videre.
      # Teller: Har behandlingsplan (plan_beh == 2) og status er aktiv (behandlingsstatus == 1)
      # Nevner: Status er aktiv (behandlingsstatus == 1)

      # Figur og tabell
      # Figur
      output$distPlot <- shiny::renderPlot({
        makeHist(df = basisData, var = input$var, bins = input$bins)
      })

      # Tabell
      output$distTable <- shiny::renderTable({
        makeHist(df = basisData, var = input$var, bins = input$bins,
                makeTable = TRUE)
      })
    }
)}