#' Filter out the first measurement point per patient
#'
#' This function sorts the rows for each patient according to the registration time
#' and keeps the first registration
#'
#' @return timepointData data frame
#' @export

getFirstRegistrations <- function(timepointData) {
  filteredData <- timepointData |>
    dplyr::group_by(PasientGUID) |> # nolint
    dplyr::arrange(dato_mp_beh, .by_group = TRUE) |> # nolint
    dplyr::slice_head() |>
    dplyr::ungroup()

  return(filteredData)
}

#' @export

prePros <- function(data) {

  data <- data |>
    dplyr::rename(Sykehus = db_unit_title,  # nolint
                  Alder = PatientAge)       # nolint

  data <- data |>
    dplyr::mutate(
      behandlingsstatus_ny = dplyr::recode(behandlingsstatus, # nolint
                                           "0" = "velg verdi", "1" = "aktiv",
                                           "2" = "overført annen juridisk enhet",
                                           "3" = "avbrutt", "4" = "avsluttet med gjensidig avtale",
                                           "5" = "fulgt opp etter 1 år",
                                           "6" = "ikke oppnådd kontakt etter 1 år",
                                           "7" = "ønsker ikke å svare etter 1 år"),

        behandlingsplan = dplyr::recode(plan_beh, # nolint
                                        "0" = "velg verdi",
                                        "1" = "nei",
                                        "2" = "ja",
                                        "3" = "vet ikke"),

        kriseplan = dplyr::recode(plan_krise, # nolint
                                  "0" = "velg verdi",
                                  "1" = "nei",
                                  "2" = "ja",
                                  "3" = "vet ikke"),


        Kjønn = dplyr::recode(PatientGender, # nolint
                              "1" = "mann",
                              "2" = "kvinne")
    ) # Vill gjetning

  data <- data |>
    dplyr::select(!behandlingsstatus) |> # nolint
    dplyr::rename(behandlingsstatus = behandlingsstatus_ny) # nolint

  return(data)
}
