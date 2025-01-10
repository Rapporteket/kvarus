#' @title
#'
#'
#' @export

kval_count <- function(data, var){ # legg evt. til flere variabler her avhengig av brukervalg

  # Prosessindikatorene med KUN: første målepunkt ###############################

  ###### ONLY FIRST POINT OF MEASURE ############################################
  data <- getFirstRegistrations(data)

  data <- data %>%
    dplyr::filter(behandlingsstatus == "aktiv")

  data <- data %>%
    filter(Sykehus != "NA")

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
## kval <-  kval_count(punktData, "behandlingsstatus")



# Test for å sjekke:
## k <- kval_count(punktData, "behandlingsplan")

# This function takes data frames made by other functions as input
# "data" is the data frame made by kval_count
# "gg_data" is the data frame made by makeGGdata

#' @export

kval_plot <- function(data, gg_data){

  kval_plot <- data %>%

      ggplot2::ggplot(aes(x = andel_per_syk, y = Sykehus))+

      ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                        xmin = 80,
                        xmax = 100,
                        ymin = -Inf, ymax = Inf , fill = "lightgreen",
                        alpha = .25)+

      ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                        xmin = 60,
                        xmax = 79.999,
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
## kval_plot(kval, ggData)

