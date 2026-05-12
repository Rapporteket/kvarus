#' Provides a dataframe containing data from a registry
#'
#' @param columns A character vector of column names to retrieve from the database.
#' @return regData data frame
#' @export

getBasisData <- function(columns = c("PatientAge")) {

  query <- sprintf(
    "SELECT %s FROM basisopplysninger_1;",
    paste(columns, collapse = ", ")
  )

  rapbase::loadRegData("data", query)

}
