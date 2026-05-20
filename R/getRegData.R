#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getBasisData <- function() {

  query <- "SELECT * FROM basisopplysninger_1;"

  rapbase::loadRegData("data", query)

}

#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getTimepointData <- function() {

  query <- "SELECT * FROM maalepunkt_2;"

  rapbase::loadRegData("data", query)

}
