#'@export
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
