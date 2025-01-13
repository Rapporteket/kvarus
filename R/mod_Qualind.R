####### MODULE FOR KVALITETSINDIKATORER ########################################

#' @export

module_kvalitetsindikator_UI <- function(id){
  ns <- NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        selectInput( # First select
          inputId = ns("kval_var"),
          label = "Velg Kvalitetsindikator:",
          choices = c("Behandlingsplan på plass tidlig i forløpet" = "behandlingsplan",
                      "Kriseplan på plass tidlig i forløpet" = "kriseplan"
          ),
          selected = "behandlingsplan")),


      shiny::mainPanel(
        bslib::navset_card_underline(
          bslib::nav_panel("Figur",
                           shiny::plotOutput(outputId = ns("kval_plot")),
                           shiny::downloadButton(ns("download_fig"), "Last ned figur")),
          bslib::nav_panel("Tabell",
                           DT::DTOutput(outputId = ns("kval_table")),
                           shiny::downloadButton(ns("download_tbl"), "Last ned tabell", class = "butt2"))
          #bslib::nav_panel("Over tid", plotOutput(outputID = "kval_over_tid")) # hvis vi vil legge dette inn etter hvert
        ),

        bslib::navset_card_underline(
          title = h4("Slik er kvalitetsindikatoren regnet ut:"),
          bslib::card_header(
            tags$em(
              shiny::textOutput(
                outputId = ns("text_header")))),
          bslib::card_body(
            shiny::htmlOutput(
              outputId = ns("text_body")))
        )
      )
    )
  )
}



# Server-part

#'@export

module_kvalitetsindikator_server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

    ### Load in data ###
    punktData <- getTimepointData()

    basisData <- getBasisData()

    ### Clean and tidy data ###
    punktData <- prePros(punktData)

    # Count quality indicator:
    kval_df_reactive <- reactive({
      x <- kval_count(punktData, input$kval_var)
    })

    #### kval <- kval_count(punktData, "behandlingsstatus")


###### PLOT ####################################################################
    # Make labs for ggplot:
    ggData_reactive <- reactive({
      ggData <- makeGGdata(input$kval_var, "kval")
    })

    #### ggData <- makeGGdata("behandlingsstatus", "kval")

    # Make annotations for plot:
    anno_reactive <- reactive({
      anno <- annotations(input$kval_var)
    })


    # Make plot:

    kval_plot <- reactive({
      kval_plot(kval_df_reactive(), ggData_reactive())
    })

    output$kval_plot <- renderPlot({
      kval_plot()
    })

    ##### kval_plot <- kval_plot(kval, ggData)

####### TABLE ##################################################################

    output$kval_table <- DT::renderDT({datatable(kval_df_reactive(),
                                                 class = 'white-space:nowrap compact',
                                                 colnames = c("Sykehus",
                                                              "Antall nasjonalt",
                                                              "Antall per sykehus",
                                                              "Antall - kvalitetsindikator",
                                                              "Andel - kvalitetsindikator"))
    })

###### NEDLASTING FIGUR/TABELL #################################################

    output$download_fig <-  downloadHandler(
      filename = function(){
        paste("Figur_", input$kval_var,"_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file){
        pdf(file, onefile = TRUE, width = 15, height = 9)
        plot(kval_plot())
        dev.off()
      }
    )

    output$download_tbl <- downloadHandler(
      filename = function(){
        paste("Tabell_", input$kval_var, "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file){
        write.csv(kval_df_reactive(), file)
      }
    )

####### EXPLANATION OF CALCULATION QUALIND #####################################

    output$text_header <- renderText({
      data <- explanation_kvalind(input$kjønn_var, input$kval_var)
      data$header
    })

    output$text_body <- renderText({
      data <- explanation_kvalind(input$kjønn_var, input$kval_var)
      data$text
    })

    })
}
