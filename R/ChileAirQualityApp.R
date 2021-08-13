#' Title ChileAirQualityApp
#' @description This tool is a dashboard that allows you to use the data download
#'  functions of this package enhanced with analysis, visualization and
#'  descriptive statistics tools.
#' @return A shiny dashboard to work with the package
#' @export
#' @seealso <https://chileairquality.shinyapps.io/chileairquality/>
#' @examples \dontrun{ChileAirQualityApp()}
#' @import shiny
#' @import dplyr
#' @import data.table
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import shiny
#' @import openair
#' @import lubridate
#' @import shinycssloaders
ChileAirQualityApp <- function() {
  Directory <- system.file("shiny", package = "AtmChile")
  if (Directory == "") {
    stop("Try reinstalling the package 'AtmChile'.", call. = FALSE)
  }
  shiny::runApp(Directory, display.mode = "normal")
}

