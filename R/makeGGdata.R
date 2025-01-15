#'
#'
#'
#'@export

makeGGdata <- function(var, type_plot) {
  ggdata <- data.frame(title = "", xlab = "")

  config <- get_config() # nolint

  if (type_plot == "kval") {
    ggdata <- ggdata |>
      dplyr::mutate(
        title = dplyr::case_when({{var}} == "behandlingsplan" ~ config$kvalind$behandlingsplan$tittel,
                                 {{var}} == "kriseplan" ~ config$kvalind$kriseplan$tittel,
                                {{var}} == "utbytte" ~ config$kvalind$utbytte$tittel),
        xlab = dplyr::case_when({{var}} == "behandlingsplan" ~ "Andel pasienter",
                                {{var}} == "kriseplan" ~ "Andel pasienter",
                                {{var}} == "utbytte" ~ "Andel pasienter")
      )
  }

  return(ggdata)
}
