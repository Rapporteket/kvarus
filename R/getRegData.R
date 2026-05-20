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
