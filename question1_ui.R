# question1_ui.R
question1_ui <- function(id) {
  ns <- NS(id)
  tabBox(
    id = ns("tabset1"),
    width = 12,
    tabPanel("Timeline Table", 
             DT::dataTableOutput(ns("timeline_table"))),
    tabPanel("Time Filter & Chord Diagram",
             fluidRow(
               box(
                 width = 12,
                 title = "Time Filter (Every 2 Hours)",
                 uiOutput(ns("time_slider_ui"))
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Interactive Chord Diagram",
                 uiOutput(ns("chord_plot"))
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Tooltip Info",
                 helpText("Hover over a chord to view the number of messages sent between entities.")
               )
             )
    ),
    tabPanel("Heatmap",
             fluidRow(
               box(
                 width = 12,
                 title = "Communication Heatmap (All Time)",
                 plotly::plotlyOutput(ns("heatmap_plot"), height = "600px"),
                 helpText("Each tile shows the number of messages sent from sender (rows) to recipient (columns). Hover over tiles to see counts.")
               )
             )
    )
  )
}
