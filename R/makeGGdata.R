#'
#'
#'
#'@export

makeGGdata <- function(var, type_plot) {
  ggdata <- data.frame(title = "", xlab = "")

  if (type_plot == "kval") {
  ggdata <- ggdata %>%
    dplyr::mutate(title = dplyr::case_when({{var}} == "behandlingsplan" ~ "Andel pasienter som tidlig i forløpet (første målepunkt) har fått utarbeidet behandlingsplan",
                                           {{var}} == "kriseplan" ~ "Andel pasienter som tidlig i forløpet (første målepunkt) har fått utarbeidet behandlingsplan",
                                           {{var}} == "utbytte" ~ "Andel pasienter som har opplevd stort utbytte (skår 4 eller 5) av behandlingen"),
                  xlab = dplyr::case_when({{var}} == "behandlingsplan" ~ "Andel pasienter",
                                          {{var}} == "kriseplan" ~ "Andel pasienter",
                                          {{var}} == "utbytte" ~ "Andel pasienter"))
  }

  return(ggdata)
}

# Test to see if it works:
## ggData <- makeGGdata("behandlingsplan", "kval")
