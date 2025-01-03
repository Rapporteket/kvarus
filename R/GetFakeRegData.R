#' Provide dataframe of fake registry data
#'
#' Provides a dataframe containing built-in data (and not a registry) for demo
#' purposes
#'
#' @return regData data frame
#' @export

library(readxl)

getFakeRegData <- function() {

  regData <- read_excel("DataDump_MRS-DEMO_Basisopplysninger_2024-10-28_1403.xlsx")

  return(regData)
}
