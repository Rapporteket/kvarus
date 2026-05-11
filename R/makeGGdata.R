#' Prepare Data for GGplot Visualization
#'
#' Transforms and organizes data into a format suitable for creating ggplot2
#' visualizations based on the specified variable and plot type.
#'
#' @param var A variable or data object to be processed for visualization.
#' @param type_plot A character string specifying the type of plot to generate
#'   (e.g., "scatter", "line", "bar", "boxplot").
#'
#' @return A data frame or list formatted for use with ggplot2 functions.
#'
#' @keywords internal

makeGGdata <- function(var, type_plot) {
  ggdata <- data.frame(title = "", xlab = "")

  config <- get_config()

  if (type_plot == "kval") {
    ggdata <- ggdata |>
      dplyr::mutate(
        title = dplyr::case_when({{var}} == "behandlingsplan" ~ config$kvalind$behandlingsplan$tittel,
                                 {{var}} == "kriseplan" ~ config$kvalind$kriseplan$tittel,
                                 {{var}} == "utbytte" ~ config$kvalind$utbytte$tittel,
                                 {{var}} == "gjensidig" ~ config$kvalind$gjensidig$tittel,
                                 .default = config$kvalind$default$tittel),
        xlab = "Andel pasienter"
      )
  }

  return(ggdata)
}
