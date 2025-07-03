# question2_ui.R
question2_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabsetPanel(
      tabPanel("Timeline: People and Vessels",
               h4("7.2.2 Timeline of People and Vessels"),
               plotOutput(ns("timeline_people_and_vessels")),
               br(),
               h4("7.2.2.1 Findings"),
               p("Communication frequency between key individuals and vessels is concentrated on certain days, potentially indicating coordinated activity.")
      ),
      tabPanel("Bigram & Louvain Network",
               h4("7.5.1 Community Network (Louvain Clustering)"),
               plotOutput(ns("community_network_plot")),
               br(),
               h4("7.5.2 Bigram Network Plot"),
               plotOutput(ns("bigram_network_plot"))
      ),
      tabPanel("Community Summary Table",
               h4("7.5.3 Community Membership Summary"),
               reactableOutput(ns("community_summary_table"))
      ),
      tabPanel("Chord Diagram & Findings",
               h4("7.5.6 Community-based Chord Diagram"),
               textOutput(ns("chord_diagram_text")),
               br(),
               h4("7.5.6.1 Findings"),
               p("Chord diagram reveals frequent connections within and between specific communities.")
      ),
      tabPanel("Community-People-Vessels View",
               h4("7.5.8 Community-People-Vessels View"),
               plotOutput(ns("community_flow_plot")),
               br(),
               h4("7.5.8.1 Findings"),
               p("This diagram displays the flow of interactions involving vessels and individuals by community.")
      ),
      tabPanel("Community Timeline",
               h4("7.5.12-13 Timeline of Community Interactions"),
               plotOutput(ns("community_timeline_plot"))
      )
    )
  )
}
