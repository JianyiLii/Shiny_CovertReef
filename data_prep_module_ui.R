# data_prep_module_ui.R
dataPrepUI <- function(id) {
  ns <- NS(id)
  
  tabItem(
    tabName = "DataPrep",
    
    fluidPage(
      h2("📝 Data Preparation"),
      
      fluidRow(
        box(
          title = "Our Sub-Task for Take Home Ex 03",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          p("Explain how the data was loaded, cleaned, and structured for use in Shiny.")
        )
      ),
      
      tabsetPanel(
        tabPanel("1.1 Node Cleaning",
                 h4("Steps Applied:"),
                 tags$ul(
                   tags$li("Convert id to character"),
                   tags$li("Remove NA ids"),
                   tags$li("Remove duplicated ids"),
                   tags$li("Drop unnecessary columns: thing_collected, time, date, friendship_type")
                 ),
                 verbatimTextOutput(ns("node_clean_summary"))
        ),
        
        tabPanel("1.2 Edge Cleaning",
                 h4("Steps Applied:"),
                 tags$ul(
                   tags$li("Rename source/target to from_id/to_id"),
                   tags$li("Convert from_id/to_id to character"),
                   tags$li("Drop rows where from_id/to_id are missing or not in nodes"),
                   tags$li("Split to_id for supertype info")
                 ),
                 verbatimTextOutput(ns("edge_clean_summary"))
        ),
        
        tabPanel("1.3 Communication Extraction",
                 h4("Logic Applied:"),
                 tags$ul(
                   tags$li("Filter for 'sent' edges from nodes of supertype 'Event'"),
                   tags$li("Join to get sender/recipient names and subtypes"),
                   tags$li("Join to get content and timestamp of the communication event"),
                   tags$li("Reorganize to long-format dataframe with sender, recipient, content, timestamp")
                 ),
                 verbatimTextOutput(ns("comm_extract_summary"))
        )
      )
    )
  )
}
