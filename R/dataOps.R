#' Filter out the first measurement point per patient
#'
#' This function sorts the rows for each patient according to the registration time
#' and keeps the first registration
#'
#' @return timepointData data frame
#' @export

getFirstRegistrations <- function(timepointData) {
  filteredData <- timepointData %>%
    dplyr::group_by(PasientGUID) %>%
    dplyr::arrange(dato_mp_beh, .by_group = TRUE) %>%
    dplyr::slice_head()

  return(filteredData)
}
