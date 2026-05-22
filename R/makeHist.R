#' Make a histogram
#'
#' @param df dataframe from which output is to be made
#' @param var string defining which varable in the data frame to use
#'
#' @return a graphical object
#' @export
#'
#' @examples
#' makeHist(df = mtcars, var = "mpg")

makeHist <- function(df, var) {

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_bar(fill = "#154ba2", color = "white") +
    ggplot2::labs(title = paste("Fordeling av", var), x = var, y = "Antall")
}
