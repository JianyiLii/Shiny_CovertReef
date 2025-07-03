question4_ui <- function(id) {
  ns <- NS(id)
  
  tabsetPanel(
    # Question 4a
    tabPanel("Nadia's Networks",
             h3("Ego Network - Sent"),
             visNetworkOutput(ns("nadia_ego_sent"), height = "600px"),
             h3("Ego Network - Received"),
             visNetworkOutput(ns("nadia_ego_received"), height = "600px"),
             h3("2-Hop Network"),
             visNetworkOutput(ns("nadia_2hop"), height = "600px"),
             h3("2-Hop Communication Timeline"),
             plotlyOutput(ns("nadia_2hop_timeline")),
             h3("Findings Summary"),
             verbatimTextOutput(ns("nadia_network_findings"))
    ),
    
    # Question 4b
    tabPanel("Suspicious?",
             h3("Keyword Timeline"),
             plotlyOutput(ns("keyword_timeline")),
             h3("Two-word Keyword Timeline"),
             plotlyOutput(ns("two_word_timeline")),
             h3("Keyword Findings"),
             verbatimTextOutput(ns("keyword_findings")),
             h3("Suspicious Discussions"),
             reactableOutput(ns("suspicious_table_1")),
             br(),
             reactableOutput(ns("suspicious_table_2")),
             br(),
             reactableOutput(ns("suspicious_table_3")),
             h3("Final Findings Summary"),
             verbatimTextOutput(ns("final_findings"))
    )
  )
}
