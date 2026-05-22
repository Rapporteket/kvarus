#' Provides a dataframe containing data from a registry
#'
#' @param columns A character vector of column names to retrieve from the database.
#' @param org The organization ID for which to retrieve data.
#'
#' @return regData data frame
#' @export

getBasisData <- function(columns = c("PatientAge"), org = NULL) {

  query <- sprintf(
    "SELECT %s FROM basisopplysninger_1 WHERE UnitId = %s;",
    paste(columns, collapse = ", "), org
  )

  rapbase::loadRegData("data", query)

}

#' Provides a dataframe containing data from a registry
#'
#' @return regData data frame
#' @export

getTimepointData <- function() {

  query <- "SELECT
   PasientGUID,
   dato_mp_beh,
   UnitId,
   db_unit_title as Sykehus,
   PatientAge as Alder,
   behandlingsstatus,
   plan_beh,
   plan_krise,
   eval_utbytte,
   PatientGender
   FROM maalepunkt_2;
  "

  rapbase::loadRegData("data", query)

}
