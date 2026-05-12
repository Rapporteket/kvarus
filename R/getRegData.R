#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getBasisData <- function(columns = c("PatientAge", "oppfolging_nav_frekvens")) {

  query <- sprintf(
    "SELECT %s FROM basisopplysninger_1;",
    paste(columns, collapse = ", ")
  )

  rapbase::loadRegData("data", query)

}
