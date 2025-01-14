
#' @rdname config
#' @export
get_config <- function() {
  config_file <- "inst/config.yml"
  config <- yaml::read_yaml(config_file)
  return(config)
}