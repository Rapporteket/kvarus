#' @title
#'
#'
#' @export

kval_count <- function(data, var){ #, var, kjønn, type_op){ # legg til flere variabler her avhengig av brukervalg

  # Prosessindikatorene med KUN: første målepunkt ###############################

  ###### ONLY FIRST POINT OF MEASURE ############################################
  data <- getFirstRegistrations(data)

  data <- data %>%
    dplyr::filter(behandlingsstatus == "aktiv")

  ##### Make tiny data set with counts ###########################################
  my_tiny_data <- data %>%
    dplyr::add_tally(name = "alle") %>% # antall pasienter i datasettet
    dplyr::group_by(Sykehus) %>%
    dplyr::add_tally(name = "per_syk") %>% # antall pasienter per sykehus
    dplyr::ungroup() %>%
    # dplyr::group_by(Sykehus, Kjønn) %>%
    # dplyr::add_tally(name = "per_syk_kjønn") %>% # antall pasienter per sykehus per kjønn
    # dplyr::ungroup() %>%
    # dplyr::select(Sykehus, Kjønn, alle, per_syk, per_syk_kjønn) %>%
    dplyr::select(Sykehus, alle, per_syk) %>%
    dplyr::distinct()


  ###### Filter based on kvalitetsindikatorer ####################################

  kval <- data %>%
    dplyr::filter(dplyr::case_when({{var}} == "behandlingsplan" ~
                                    behandlingsplan == "ja",
                                   ## Legg evt. til flere variabler her
                                   TRUE ~
                                     behandlingsplan != "ja")) %>%
    dplyr::group_by(Sykehus) %>%
    dplyr::count(name = "antall_kval_syk") %>%
    dplyr::ungroup()

  ##### Join data with counts based on kvalitetsindikator ########################
  ###### with data based on whole data set #######################################

  jak <- dplyr::left_join(my_tiny_data, kval)



  return(jak)


}

# Test for å sjekke:
## k <- kval_count(punktData, "behandlingsplan")

