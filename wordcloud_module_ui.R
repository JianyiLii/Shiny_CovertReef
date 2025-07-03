wordcloudUI <- function(id) {
  ns <- NS(id)
  plotOutput(ns("wordcloudPlot"), height = "500px")
}
