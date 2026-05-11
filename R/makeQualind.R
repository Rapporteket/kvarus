#' Calculate Quality Indicator Count
#'
#' @description
#' Counts observations for quality indicator calculations based on specified variable(s).
#'
#' @param data A data frame containing the data to be analyzed
#' @param var Variable name(s) to use for counting. Additional variables can be added
#'   as needed based on user selection.
#'
#' @return
#' A numeric count or aggregated result based on the specified variable(s).
#'
#' @keywords internal

kval_count <- function(data, var) { # legg evt. til flere variabler her avhengig av brukervalg

  ##### Make tiny data set with counts #########################################
  my_tiny_data <- data |>
    dplyr::add_tally(name = "alle") |> # antall pasienter i datasettet
    dplyr::group_by(.data$Sykehus) |>
    dplyr::add_tally(name = "per_syk") |> # antall pasienter per sykehus
    dplyr::ungroup() |>
    dplyr::select("Sykehus", "alle", "per_syk") |>
    dplyr::distinct()

  # ### Prosessindikatorer som ikke ser på første målepunkt ######################

  if (var == "gjensidig") {
    data <- data |>
      dplyr::filter(.data$behandlingsstatus %in% c("avsluttet med gjensidig avtale", "avbrutt"))
  } else { # Prosessindikatorene med KUN: første målepunkt ######################

    data <- getFirstRegistrations(data)

    data <- data |>
      dplyr::filter(.data$behandlingsstatus == "aktiv")
  }

  #data <- data |> # nolint
  #  filter(Sykehus != "NA") # nolint

  ###### Filter based on kvalitetsindikatorer ##################################

  kval <- data |>
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
                                     behandlingsplan != "ja")) |>
    dplyr::group_by(.data$Sykehus) |>
    dplyr::count(name = "antall_kval_syk") |>
    dplyr::ungroup()

  ##### Join data with counts based on kvalitetsindikator ######################
  ###### with data based on whole data set #####################################

  jak <- dplyr::left_join(my_tiny_data, kval, by = "Sykehus")

  jak <- jak |>
    dplyr::mutate(antall_kval_syk = tidyr::replace_na(.data$antall_kval_syk, 0))



  magnus <- jak |>
    dplyr::mutate(andel_per_syk = round(.data$antall_kval_syk / .data$per_syk * 100, 2))


  return(magnus)
}

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
#'
#' @keywords internal

annotations <- function(var) {
  anno <- data.frame(xmax = 100, # in most cases xmax would be 100%
                     xmin = 0, # this must be altered
                     xmax_moderate = 0, # this must be altered
                     xmin_moderate = 0) # this must be altered

  anno <- anno |>
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


#' Create a Quality Indicator Plot
#'
#' @description
#' Generates a quality indicator plot based on the provided data and graphical parameters.
#'
#' @param data A data frame or data object containing the quality indicator data to be plotted.
#' @param ggData A data frame formatted for use with ggplot2, containing processed data for visualization.
#' @param anno A list or data frame containing annotation information to be added to the plot.
#'
#' @return
#' A ggplot2 plot object representing the quality indicator visualization.
#'
#' @keywords internal
kval_plot <- function(data, ggData, anno) {

  kval_plot <- data |>
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


#' Get Explanations for Quality Indicators
#'
#' @description
#' Retrieves explanatory text and header information for quality indicators
#' (kvalitetsindikatorer) based on the provided variable name.
#'
#' @param var character. The name of the quality indicator variable for which
#'   to retrieve explanations.
#'
#' @return
#' A data frame with columns:
#'   \item{header}{Character string containing the header/title for the indicator}
#'   \item{text}{Character string containing the explanation text for the indicator}
#'
#' @details
#' The function retrieves configuration settings via \code{get_config()} and
#' uses them to populate explanatory information for the specified quality indicator.
#'
#' @examples
#' \dontrun{
#'   explanation_kvalind("indicator_name")
#' }
#' @keywords internal

explanation_kvalind <- function(var) {

  data <- data.frame(header = "", text = "")
  config <- get_config()

  data <- data |>
    dplyr::mutate(text =  dplyr::recode_values({{var}},
                                            "behandlingsplan" ~ config$kvalind$behandlingsplan$forklaring,
                                            "kriseplan" ~ config$kvalind$kriseplan$forklaring,
                                            "utbytte" ~ config$kvalind$utbytte$forklaring,
                                            "gjensidig" ~ config$kvalind$gjensidig$forklaring,
                                            default = config$kvalind$default$forklaring),
      header = dplyr::recode_values({{var}},
                                 "behandlingsplan" ~ "Behandlingsplan på plass tidlig i forløpet",
                                 "kriseplan" ~ "Kriseplan på plass tidlig i forløpet",
                                 "utbytte" ~ "Stort utbytte av behandlingen",
                                 "gjensidig" ~ "Gjensidig avslutning av behandlingen")
    )

  return(data)
}
