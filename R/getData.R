#' Provide dataframe of fake registry data
#'
#' Provides a dataframe containing built-in data (and not a registry) for demo
#' purposes
#'
#' @return regData data frame
#' @export
getBasisData <- function() {

  basisData <- readxl::read_excel("/home/rstudio/kvarus/DataDump_MRS-DEMO_Basisopplysninger_2024-10-28_1403.xlsx") %>%
    as.data.frame()

  return(basisData)
}

#' @export

getTimepointData <- function() {
  timepointData <- readxl::read_excel("/home/rstudio/kvarus/DataDump_MRS-DEMO_Målepunkt_2024-10-28_1403.xlsx") %>%
    as.data.frame()

  return(timepointData)
}
