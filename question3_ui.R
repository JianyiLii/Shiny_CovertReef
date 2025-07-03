
# --------------------------
# question3_ui.R
# --------------------------
question3_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    p("Explore how observed pseudonyms relate to real identities and their community affiliation."),
    selectInput(ns("person"), "Select a Real Identity:", choices = NULL),  # Will be populated dynamically
    plotOutput(ns("alluvialPlot"), height = "600px")
  )
}
