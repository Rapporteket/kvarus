#'
#'
#'
#'@export

makeGGdata <- function(var, type_plot){
  ggdata <- data.frame(title = "", xlab = "")

  if (type_plot == "kval"){
  ggdata <- ggdata %>%
    dplyr::mutate(title = dplyr::case_match({{var}}, "behandlingsstatus" ~ "Andel pasienter som tidlig i forløpet (første målepunkt) har fått utarbeidet behandlingsplan"),
                  xlab = dplyr::case_match({{var}}, "behandlingsstatus" ~ "Andel pasienter"))
  }
}

ggData <- makeGGdata("behandlingsstatus", "kval")
