#' @title
#'
#'
#' @export

kval_count <- function(punktData, var) { # legg evt. til flere variabler her avhengig av brukervalg

  # Prosessindikatorene med KUN: første målepunkt ###############################

  ###### ONLY FIRST POINT OF MEASURE ############################################
  data <- getFirstRegistrations(punktData)

  data <- data %>%
    dplyr::filter(behandlingsstatus == "aktiv")

  #data <- data %>%
   # filter(Sykehus != "NA")

  ##### Make tiny data set with counts ###########################################
  my_tiny_data <- data %>%
    dplyr::add_tally(name = "alle") %>% # antall pasienter i datasettet
    dplyr::group_by(Sykehus) %>%
    dplyr::add_tally(name = "per_syk") %>% # antall pasienter per sykehus
    dplyr::ungroup() %>%
    dplyr::select(Sykehus, alle, per_syk) %>%
    dplyr::distinct()


  ###### Filter based on kvalitetsindikatorer ####################################

  kval <- data %>%
    dplyr::filter(dplyr::case_when({{var}} == "behandlingsplan" ~
                                    behandlingsplan == "ja",
                                   {{var}} == "kriseplan" ~
                                     kriseplan == "ja",
                                   {{var}} == "utbytte" ~
                                     eval_utbytte %in% c(4, 5),
                                   ## Legg evt. til flere variabler her
                                   TRUE ~
                                     behandlingsplan != "ja")) %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::count(name = "antall_kval_syk") %>%
    dplyr::ungroup()

  ##### Join data with counts based on kvalitetsindikator ########################
  ###### with data based on whole data set #######################################

  jak <- dplyr::left_join(my_tiny_data, kval)

  jak <- jak %>%
    dplyr::mutate(antall_kval_syk = tidyr::replace_na(antall_kval_syk, 0))



  magnus <- jak %>%
    dplyr::mutate(
      andel_per_syk =
        round(antall_kval_syk/per_syk*100, 2))


  return(magnus)
}

# testing:
## kval <-  kval_count(punktData, "utbytte")

# Test for å sjekke:
## k <- kval_count(punktData, "behandlingsplan")


#' Annotations
#'
#' Get the right annotations in plot based on desired points of measurement
#' for each of the quality indicators
#'
#' If a certain quality indicator is selected by user, the right level must be
#' indicated in green and in orange. xmax and xmin indicate the level for high
#' (=green) achievement. xmax_moderate and xmin_moderate indicate the level for
#' moderate achievement (i.e., orange)
#'
#'
#' @return data frame with numeric values
#' @examples
#' my_annotations <- annotations("behandlingsplan")
#'
#' @export

annotations <- function(var){
  anno = data.frame(xmax = 100, # in most cases xmax would be 100%
                    xmin = 0, # this must be altered
                    xmax_moderate = 0, # this must be altered
                    xmin_moderate = 0) # this must be altered

  anno <- anno %>%
    dplyr::mutate(xmin = dplyr::case_when({{var}} == "behandlingsplan" ~ 80,
                                          {{var}} == "kriseplan" ~ 60,
                                          {{var}} == "utbytte" ~ 80),
                  xmax_moderate = xmin,
                  xmin_moderate = dplyr::case_when({{var}} == "behandlingsplan" ~ 60,
                                                   {{var}} == "kriseplan" ~ 40,
                                                   {{var}} == "utbytte" ~ 60)
    )


  return(anno)
}

# Test what it works::
## anno <- annotations("kriseplan")



# This function takes data frames made by other functions as input
# "data" is the data frame made by kval_count
# "gg_data" is the data frame made by makeGGdata
# "anno" is the data frame made by "annotations"

#' @export

kval_plot <- function(data, ggData, anno){

  kval_plot <- data %>%

      ggplot2::ggplot(aes(x = andel_per_syk, y = Sykehus))+

      ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                        xmin = anno$xmin,
                        xmax = anno$xmax,
                        ymin = -Inf, ymax = Inf , fill = "lightgreen",
                        alpha = .25)+

      ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                        xmin = anno$xmin_moderate,
                        xmax = anno$xmax_moderate,
                        ymin = -Inf, ymax = Inf , fill = "gold",
                        alpha = .15)+


      ggplot2::geom_col(fill = "#6CACE4", alpha = .7)+

      # ggplot2::geom_rect(aes(ymin = 0, ymax = 5, xmin = x_start, xmax = x_end), alpha = .5)+

      #### TITLES ################################################################

      ggplot2::xlab(ggData$xlab)+

      ggplot2::ylab("Sykehus")+

      ggplot2::ggtitle(ggData$title)+


      ggplot2::geom_label(aes(x = 0, label = paste(antall_kval_syk, "av", per_syk)),
                          fill = "#BFCED6", color = "#003087", fontface = "italic",
                          position = position_dodge(.9), vjust = .5, size = 3,
                          alpha = .8)+

    ggplot2::scale_x_continuous(breaks = c(20,40,60,80,100))+ # maybe alter this based if other variables are chosen


      ##### THEME AND COLOURS ####################################################

    ggplot2::theme_light() # light theme



    return(kval_plot)
  }


# Test to see that it works
## kval_plot(kval, ggData, anno)


#' @title Function for getting explanations for kvalitetsindikatorer
#'
#' @export


explanation_kvalind <- function(var) {

  data <- data.frame(header = "", text = "")


  data <- data %>%
    dplyr::mutate(text =  dplyr::case_match({{var}},
                                            "behandlingsplan" ~
                                              paste0("Figuren viser fordelingen av andel pasienter som tidlig i forløpet er under aktiv behandling og har på plass en behandlingsplan. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som ved første målepunkt har status som 'aktiv' og har en behandlingsplan / antall pasienter som ved første målepunkt har status som 'aktiv' *100", "<br/>"),
                                            {{var}},
                                            "kriseplan" ~
                                              paste0("Figuren viser fordelingen av andel pasienter som tidlig i forløpet er under aktiv behandling og har på plass en behandlingsplan. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som ved første målepunkt har status som 'aktiv' og har en kriseplan / antall pasienter som ved første målepunkt har status som 'aktiv' *100", "<br/>"),
                                            "utbytte" ~
                                              paste0("Figuren viser fordelingen av andel pasienter som svarer at de i stor (=4) eller svært stor (=5) grad har utbytte av behandlingen. For hvert sykehus er det regnet ut slik:", "<br/>", "antall pasienter som ved første målepunkt har status som 'aktiv' og har svart 4 eller 5 / antall pasienter som ved første målepunkt har status som 'aktiv' *100", "<br/>"))
                  ,
                  header =
                    dplyr::case_match({{var}},
                                      "behandlingsplan" ~ "Behandlingsplan på plass tidlig i forløpet",
                                      "kriseplan" ~ "Kriseplan på plass tidlig i forløpet",
                                      "utbytte" ~ "Stort utbytte av behandlingen")
    )

  return(data)

}

# Testing that it works
## g <- explanation_kvalind("behandlingsplan")

