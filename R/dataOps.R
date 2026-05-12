#' Filter out the first measurement point per patient
#'
#' This function sorts the rows for each patient according to the registration time
#' and keeps the first registration
#'
#' @param timepointData data frame with all registrations for all patients
#' @return timepointData data frame
#' @keywords internal
getFirstRegistrations <- function(timepointData) {
  filteredData <- timepointData |>
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::arrange(.data$dato_mp_beh, .by_group = TRUE) |>
    dplyr::slice_head() |>
    dplyr::ungroup()

  return(filteredData)
}


#' Preprocess Data
#'
#' Preprocesses the input data for analysis in Rapporteket's KVARUS registry.
#'
#' @param data A data frame or tibble to be preprocessed.
#'
#' @return A preprocessed data frame with cleaned and formatted data ready for analysis.
#'
#' @details
#' This function performs various preprocessing operations on the input data,
#' including data validation, cleaning, and transformation as required by
#' the KVARUS registry specifications.
#'
#' @keywords internal

prePros <- function(data) {

  data <- data |>
    dplyr::rename(Sykehus = .data$db_unit_title,
                  Alder = .data$PatientAge)

  data <- data |>
    dplyr::mutate(
      behandlingsstatus_ny = dplyr::recode(.data$behandlingsstatus,
                                           "0" = "velg verdi",
                                           "1" = "aktiv",
                                           "2" = "overført annen juridisk enhet",
                                           "3" = "avbrutt",
                                           "4" = "avsluttet med gjensidig avtale",
                                           "5" = "fulgt opp etter 1 år",
                                           "6" = "ikke oppnådd kontakt etter 1 år",
                                           "7" = "ønsker ikke å svare etter 1 år"),

      behandlingsplan = dplyr::recode(.data$plan_beh,
                                      "0" = "velg verdi",
                                      "1" = "nei",
                                      "2" = "ja",
                                      "3" = "vet ikke"),

      kriseplan = dplyr::recode(.data$plan_krise,
                                "0" = "velg verdi",
                                "1" = "nei",
                                "2" = "ja",
                                "3" = "vet ikke"),


      Kjønn = dplyr::recode(.data$PatientGender,
                            "1" = "mann",
                            "2" = "kvinne")
    ) # Vill gjetning

  data <- data |>
    dplyr::select(!.data$behandlingsstatus) |>
    dplyr::rename(behandlingsstatus = .data$behandlingsstatus_ny)

  return(data)
}
