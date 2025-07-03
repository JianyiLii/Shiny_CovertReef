# data_prep_module_ui.R
dataPrepUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "DataPrep",
    fluidPage(
      h2("Data Preparation"),
      tabsetPanel(
        tabPanel("3.1 Node Cleaning",
                 h4("Steps Applied:"),
                 tags$ul(
                   tags$li("Convert id to character"),
                   tags$li("Remove NA ids"),
                   tags$li("Remove duplicated ids"),
                   tags$li("Drop unnecessary columns: thing_collected, time, date, friendship_type")
                 ),
                 verbatimTextOutput(ns("node_clean_summary"))
        ),
        
        tabPanel("3.2 Edge Cleaning",
                 h4("Steps Applied:"),
                 tags$ul(
                   tags$li("Rename source/target to from_id/to_id"),
                   tags$li("Convert from_id/to_id to character"),
                   tags$li("Drop rows where from_id/to_id are missing or not in nodes"),
                   tags$li("Split to_id for supertype info")
                 ),
                 verbatimTextOutput(ns("edge_clean_summary"))
        ),
        
        tabPanel("3.3 Final Graph Build",
                 h4("Node and Edge Indexing Details"),
                 verbatimTextOutput(ns("graph_summary"))
        )
      )
    )
  )
}

