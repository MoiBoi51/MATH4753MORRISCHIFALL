#' @title Scatter Plot
#'
#' @param spruce.df Data set
#'
#' @returns A Scatter Plot of Height vs. Breast Height
#' @export
#'
#' @examples if (interactive()) {
#'   Scatterplot()
#' }
#'
#'

Scatterplot <- function(spruce.df) {

  file_path <- system.file("extdata", "SPRUCE.csv", package = "MATH4753SPRING")

  if (file_path == "") {
    stop("Data file not found in the package.")
  }

  spruce.df <- read.csv(file_path)


  plot(spruce.df$BHDiameter, spruce.df$Height,
       main = "Scatter Plot of Spruce Tree Height vs Breast Height Diameter",
       xlab = "Breast Height Diameter (cm)",
       ylab = "Height of Spruce Trees (m)",
       xlim = c(0, 1.1 * max(spruce.df$BHDiameter)),
       ylim = c(0, 1.1 * max(spruce.df$Height)),
       pch = 21,
       bg = "blue",
       cex = 1.2
  )
}


