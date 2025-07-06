question2_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tabBox(
      id = ns("q2_tabs"),
      width = 12,
      
      tabPanel("📅 Person-Vessel Timeline",
               fluidRow(
                 box(
                   width = 12,
                   title = tagList(
                     "Filter Person and Vessel Entities",
                     tags$span(
                       icon("info-circle"),
                       style = "color: #31708f; cursor: help; margin-left: 6px;",
                       title = paste(
                         "• Use the dropdown to select one or more people and/or vessels.\n",
                         "• Use the radio buttons to choose whether to match as sender, recipient, or either.\n",
                         "• Use the buttons on the right to:\n",
                         "   - Select All People\n",
                         "   - Select All Vessels\n",
                         "   - Reset all filters"
                       )
                     )
                   ),
                   
                   status = "info",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   fluidRow(
                     column(
                       width = 8,
                       shinyWidgets::pickerInput(
                         inputId = ns("pv_selected_name"),
                         label = strong("Click the dropdown to select one or more people and/or vessels to filter the timeline and network:"),
                         choices = NULL,
                         multiple = TRUE,
                         options = list(
                           `actions-box` = TRUE,
                           `live-search` = TRUE,
                           `none-selected-text` = "No entities selected"
                         )
                       ),
                       radioButtons(
                         inputId = ns("pv_name_role"),
                         label = strong("Filter role:"),
                         choices = c("Either" = "either", "Sender Only" = "sender", "Recipient Only" = "recipient"),
                         selected = "either",
                         inline = TRUE
                       ),
                       helpText(HTML(paste(
                         "<strong>Tips:</strong>",
                         "Use the buttons on the right to select all people, all vessels, or to reset your selection.",
                         sep = "<br/>"
                       )))
                     ),
                     column(
                       width = 4,
                       br(),
                       actionButton(ns("pv_select_people"),
                                    label = tagList(icon("user"), "All People"),
                                    style = "background-color: #e0f0ff; color: #003366; font-weight: bold; width: 100%;"
                       ),
                       br(), br(),
                       actionButton(ns("pv_select_vessels"),
                                    label = tagList(icon("ship"), "All Vessels"),
                                    style = "background-color: #e0ffe0; color: #004400; font-weight: bold; width: 100%;"
                       ),
                       br(), br(),
                       actionButton(ns("pv_reset"),
                                    label = tagList(icon("rotate"), "Reset"),
                                    style = "background-color: #ffe0e0; color: #660000; font-weight: bold; width: 100%;"
                       )
                     )
                   )
                 )
               ),
               
               fluidRow(
                 box(
                   width = 12,
                   title = "🕒 Timeline of Communication Between Persons and Vessels (Filtered by Selected Time Slice)",
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   plotlyOutput(ns("pv_timeline_plot"), height = "500px")
                 )
               ),
               
               fluidRow(
                 box(
                   width = 12,
                   title = "🕒 Timeline Table (Filtered by Selected Time Slice)",
                   solidHeader = TRUE,
                   status = "primary",
                   collapsible = TRUE,
                   DT::dataTableOutput(ns("filtered_timeline_table"))
                 )
               ),
               
               fluidRow(
                 box(
                   width = 12,
                   title = "Key Findings and Interpretation",
                   status = "info",
                   solidHeader = TRUE,
                   collapsible = TRUE,
                   htmlOutput(ns("pv_findings_pplvessel"))
                 )
               )
      ), # End Person-Vessel Timeline tab
      
      tabPanel("🔍 Louvain Network Analysis",
               box(
                 width = 12,
                 title = "🌐 Network Graph: Louvain Communities & PageRank Centrality",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput(ns("pv_network_plot"), height = "600px")
               ),
               
               box(
                 width = 12,
                 title = "📈 Top 10 Nodes by Centrality (PageRank, Degree, etc.)",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 tableOutput(ns("pv_centrality_table"))
               ),
               
               box(
                 width = 12,
                 title = "Key Findings and Interpretation",
                 status = "info",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 htmlOutput(ns("pv_findings_louvain1"))
               ),
               
               box(
                 width = 12,
                 title = "☁️ Wordclouds by Community",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput(ns("pv_wordcloud_plot"), height = "400px")
               ),
               
               box(
                 width = 12,
                 title = "🔁 Top Bigrams per Community (Circular Plot)",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 plotOutput(ns("pv_bigram_circular"), height = "400px")
               ),
               
               box(
                 width = 12,
                 title = "📋 Community Summary Table",
                 status = "primary",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 tableOutput(ns("pv_summary_table"))
               ),
               
               box(
                 width = 12,
                 title = "Key Findings and Interpretation",
                 status = "info",
                 solidHeader = TRUE,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 htmlOutput(ns("pv_findings_louvain2"))
               )
      )
    )
  )
}
