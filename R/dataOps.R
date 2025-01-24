#' Filter out the first measurement point per patient
#'
#' This function sorts the rows for each patient according to the registration time
#' and keeps the first registration
#'
#' @return timepointData data frame
#' @export
getFirstRegistrations <- function(timepointData) {
  filteredData <- timepointData |>
    dplyr::group_by(.data$PasientGUID) |>
    dplyr::arrange(.data$dato_mp_beh, .by_group = TRUE) |>
    dplyr::slice_head() |>
    dplyr::ungroup()

  return(filteredData)
}

#' Pre-process the dataset
#'
#' This function relevels the different levels of the columns and changes
#' numbers into readble texts etc.
#'
#' @export

prePros <- function(data) {

  data <- data |>
    dplyr::rename(Sykehus = .data$db_unit_title,
                  Alder = .data$PatientAge)

  data <- data |>
    dplyr::mutate(
      behandlingsstatus_ny = dplyr::recode(.data$behandlingsstatus,
                                           "0" = "velg verdi", "1" = "aktiv",
                                           "2" = "overført annen juridisk enhet",
                                           "3" = "avbrutt", "4" = "avsluttet med gjensidig avtale",
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


      gender = dplyr::recode(.data$PatientGender,
                             "1" = "mann",
                             "2" = "kvinne")
    )

  data <- data |>
    dplyr::select(!.data$behandlingsstatus) |>
    dplyr::rename(behandlingsstatus = .data$behandlingsstatus_ny)

  data$dato_mp_beh <-as.Date(data$dato_mp_beh)

  return(data)
}

#'Sample the data
#'
#'This function extracts samples from the large dataset. This function works
#'on datasets that have been preprosessed by prePros.
#'Sorting on gender, status, date of treatment measuring point, age of patient and unit.
#'
#'@export

basicSample <- function (data, gender, status, time1, time2, age1, age2, unit) {

    # Filter by gender

    data <- data |>
      dplyr::filter(dplyr::case_when({{gender}} == "kvinne" ~ gender == "kvinne",
                                     {{gender}} == "mann" ~ gender == "mann",
                                     {{gender}} != "kvinne" |
                                       {{gender}} != "mann" ~ gender %in% c("kvinne", "mann")))

    # Filter by operation type

  data <- data |>
    dplyr::filter(behandlingsstatus == dplyr::case_when({{status}} == "aktiv" ~ "aktiv",
                                                        {{status}} == "avstluttet med gjensidig avtale" ~ "avsluttet med gjensidig avtale",
                                                        {{status}} == "avbrutt" ~ "avbrutt",
                                                        {{status}} == "overført" ~ "overført annen juridisk enhet",
                                                        {{status}} == "fulgt opp" ~ "fulgt opp etter 1 år",
                                                        {{status}} == "ikke oppnådd kontakt" ~ "ikke oppnådd kontakt etter 1 år"))

    # Add filter on surgery date------------------------------------------------

    data <- data|>
      dplyr::filter(dplyr::between(dato_mp_beh,
                                   as.Date({{time1}}),
                                   as.Date({{time2}})))

    # Add filter on age---------------------------------------------------------

    # Using column "Alder_num" in which alder is given as an integer

    data <- data |>
      dplyr::filter(dplyr::between(Alder,
                                   {{age1}},
                                   {{age2}}))


    # Unit ---------------------------------------------------------------------

    data <- data |>
      dplyr::filter(Sykehus == {{unit}})


    return (data)

  }

