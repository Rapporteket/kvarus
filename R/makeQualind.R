#' @export

kval_count <- function(data, var) { # legg evt. til flere variabler her avhengig av brukervalg

  ##### Make tiny data set with counts #########################################
  my_tiny_data <- data %>%
    dplyr::add_tally(name = "alle") %>% # antall pasienter i datasettet
    dplyr::group_by(.data$Sykehus) %>%
    dplyr::add_tally(name = "per_syk") %>% # antall pasienter per sykehus
    dplyr::ungroup() %>%
    dplyr::select(.data$Sykehus, .data$alle, .data$per_syk) %>%
    dplyr::distinct()

  # ### Prosessindikatorer som ikke ser på første målepunkt ######################

  if (var == "gjensidig") {
    data <- data %>%
      dplyr::filter(.data$behandlingsstatus %in% c("avsluttet med gjensidig avtale", "avbrutt"))
  } else { # Prosessindikatorene med KUN: første målepunkt ######################

    data <- getFirstRegistrations(punktData)

    data <- data %>%
      dplyr::filter(.data$behandlingsstatus == "aktiv")
  }

  #data <- data %>% # nolint
  #  filter(Sykehus != "NA") # nolint

  ###### Filter based on kvalitetsindikatorer ##################################

  kval <- data %>%
    dplyr::filter(dplyr::case_when({{var}} == "behandlingsplan" ~
                                     behandlingsplan == "ja",
                                   {{var}} == "kriseplan" ~
                                     kriseplan == "ja",
                                   {{var}} == "utbytte" ~
                                     eval_utbytte %in% c(4, 5),
                                   {{var}} == "gjensidig" ~
                                     behandlingsstatus == "avsluttet med gjensidig avtale",
                                   ## Legg evt. til flere variabler her
                                   TRUE ~
                                     behandlingsplan != "ja")) %>%
    dplyr::group_by(.data$Sykehus) %>%
    dplyr::count(name = "antall_kval_syk") %>%
    dplyr::ungroup()

  ##### Join data with counts based on kvalitetsindikator ######################
  ###### with data based on whole data set #####################################

  jak <- dplyr::left_join(my_tiny_data, kval)

  jak <- jak %>%
    dplyr::mutate(antall_kval_syk = tidyr::replace_na(.data$antall_kval_syk, 0))



  magnus <- jak %>%
    dplyr::mutate(andel_per_syk = round(.data$antall_kval_syk / .data$per_syk * 100, 2))


  return(magnus)
}
# nolint start
# testing:
## kval <-  kval_count(punktData, "gjensidig") # nolint

# Test for å sjekke:
## k <- kval_count(punktData, "behandlingsplan")

# nolint end

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

annotations <- function(var) {
  anno <- data.frame(xmax = 100, # in most cases xmax would be 100%
                     xmin = 0, # this must be altered
                     xmax_moderate = 0, # this must be altered
                     xmin_moderate = 0) # this must be altered

  anno <- anno %>%
    dplyr::mutate(xmin = dplyr::case_when({{var}} == "behandlingsplan" ~ 80,
                                          {{var}} == "kriseplan" ~ 60,
                                          {{var}} == "utbytte" ~ 80,
                                          {{var}} == "gjensidig" ~ 80),
      xmax_moderate = .data$xmin,
      xmin_moderate = dplyr::case_when({{var}} == "behandlingsplan" ~ 60,
                                       {{var}} == "kriseplan" ~ 40,
                                       {{var}} == "utbytte" ~ 60,
                                       {{var}} == "gjensidig" ~ 60)
    )

  return(anno)
}

# nolint start
# Test what it works::
## anno <- annotations("kriseplan")

# This function takes data frames made by other functions as input
# "data" is the data frame made by kval_count
# "gg_data" is the data frame made by makeGGdata
# "anno" is the data frame made by "annotations"

# nolint end

#' @export

kval_plot <- function(data, ggData, anno) {

  kval_plot <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$andel_per_syk, y = .data$Sykehus)) +

    ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                      xmin = anno$xmin,
                      xmax = anno$xmax,
                      ymin = -Inf, ymax = Inf, fill = "lightgreen",
                      alpha = .25) +

    ggplot2::annotate("rect", ######### DENNE KAN HELLER BRUKES "OVER TID"...
                      xmin = anno$xmin_moderate,
                      xmax = anno$xmax_moderate,
                      ymin = -Inf, ymax = Inf, fill = "gold",
                      alpha = .15) +

    ggplot2::geom_col(fill = "#6CACE4", alpha = .7) +

    # ggplot2::geom_rect(aes(ymin = 0, ymax = 5, xmin = x_start, xmax = x_end), alpha = .5)+

    #### TITLES ################################################################

    ggplot2::xlab(ggData$xlab) +

    ggplot2::ylab("Sykehus") +

    ggplot2::ggtitle(ggData$title) +

    ggplot2::geom_label(ggplot2::aes(x = 0, label = paste(.data$antall_kval_syk, "av", .data$per_syk)),
                        fill = "#BFCED6", color = "#003087", fontface = "italic",
                        position = ggplot2::position_dodge(.9), vjust = .5, size = 3,
                        alpha = .8) +

    ggplot2::scale_x_continuous(breaks = c(20, 40, 60, 80, 100)) +
    # maybe alter this based if other variables are chosen


    ##### THEME AND COLOURS ####################################################

    ggplot2::theme_light() # light theme

  return(kval_plot)
}


# Test to see that it works
## kval_plot(kval, ggData, anno) # nolint


#' @title Function for getting explanations for kvalitetsindikatorer
#'
#' @export


explanation_kvalind <- function(var) {

  data <- data.frame(header = "", text = "")
  config <- get_config()

  data <- data |>
    dplyr::mutate(text =  dplyr::case_match({{var}},
                                            "behandlingsplan" ~ config$kvalind$behandlingsplan$forklaring,
                                            "kriseplan" ~ config$kvalind$kriseplan$forklaring,
                                            "utbytte" ~ config$kvalind$utbytte$forklaring,
                                            "gjensidig" ~ config$kvalind$gjensidig$forklaring,
                                            .default = config$kvalind$default$forklaring),
      header = dplyr::case_match({{var}},
                                 "behandlingsplan" ~ "Behandlingsplan på plass tidlig i forløpet",
                                 "kriseplan" ~ "Kriseplan på plass tidlig i forløpet",
                                 "utbytte" ~ "Stort utbytte av behandlingen",
                                 "gjensidig" ~ "Gjensidig avslutning av behandlingen")
    )

  return(data)
}
