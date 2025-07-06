# question1_ui.R
question1_ui <- function(id) {
  ns <- NS(id)
  
  tabBox(
    id = ns("tabset1"),
    width = 12,
    
    # Full timeline with filters
    tabPanel("Timeline Table",
             fluidRow(
               box(
                 width = 12,
                 title = tagList(
                   "Filter Options",
                   tags$span(
                     icon("info-circle"),
                     style = "color: #31708f; cursor: help; margin-left: 6px;",
                     title = paste(
                       "• Use the date range to select time period.\n",
                       "• Filter by entity type (Person, Vessel, etc).\n",
                       "• Narrow down by sender or recipient.\n",
                       "• Reset to clear all selections."
                     )
                   )
                 ),
                 solidHeader = TRUE,
                 status = "info",
                 fluidRow(
                   column(3,
                          dateRangeInput(
                            inputId = ns("timeline_date_range"),
                            label = "Date Range",
                            start = NULL, end = NULL,
                            format = "yyyy-mm-dd"
                          )
                   ),
                   column(3,
                          shinyWidgets::pickerInput(
                            inputId = ns("entity_type_filter_timeline"),
                            label = "Entity Type (Sender or Recipient)",
                            choices = c("Person", "Vessel", "Organization", "Group", "Location"),
                            selected = c("Person", "Vessel", "Organization", "Group", "Location"),
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE)
                          )
                   ),
                   column(3,
                          shinyWidgets::pickerInput(
                            inputId = ns("sender_filter"),
                            label = "Filter by Sender",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE)
                          )
                   ),
                   column(3,
                          shinyWidgets::pickerInput(
                            inputId = ns("recipient_filter"),
                            label = "Filter by Recipient",
                            choices = NULL,
                            multiple = TRUE,
                            options = list(`actions-box` = TRUE, `live-search` = TRUE)
                          )
                   )
                 ),
                 fluidRow(

                   column(3,
                          actionButton(ns("reset_timeline_filters"), "Reset Filters", icon = icon("undo"))
                   )
                 )
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Full Timeline Table (Filtered)",
                 solidHeader = TRUE,
                 status = "primary",
                 DT::dataTableOutput(ns("timeline_table")),
                 helpText("This table shows all messages sent and received, filtered by date, entity type, sender, and recipient.")
               )
             )
    ),
    
    
    # All-time heatmap with entity selection and contact summary
    tabPanel("Communication Heatmap (All Time)",
             fluidRow(
               box(
                 width = 8,
                 title = "Communication Heatmap (All Time)",
                 solidHeader = TRUE,
                 status = "primary",
                 plotly::plotlyOutput(ns("heatmap_plot"), height = "600px"),
                 helpText("Each tile shows the number of messages sent from sender (rows) to recipient (columns). Hover over tiles to see counts.")
               ),
               box(
                 width = 4,
                 title = tagList(
                   "Top Contacts of Selected Entity",
                   tags$span(
                     icon("info-circle"),
                     style = "color: #31708f; cursor: help; margin-left: 6px;",
                     title = paste(
                       "• Select an entity from the dropdown.\n",
                       "• Table shows who this entity communicated with most.\n",
                       "• Includes number of messages and contact subtype."
                     )
                   )
                 ),
                 
                 solidHeader = TRUE,
                 status = "info",
                 selectInput(ns("selected_person"), "Select Entity:", choices = NULL),
                 DT::dataTableOutput(ns("top_contacts_table")),
                 helpText("This shows who the selected entity communicated with most frequently, along with their subtype.")
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Findings from Heatmap",
                 solidHeader = TRUE,
                 status = "info",
                 HTML("
                   <p>The heatmap visualization shows the overall count of sender-recipient messages across all time.</p>
                   <p>This allows us to have an overview of specific communication flows to identify key pairs and communication intensity between them.</p>
                   
                   <p><strong>Entities who communicated frequently with others and may warrant further attention include:</strong></p>
                   <ul>
                     <li><strong>Person:</strong> The Intern, The Lookout, Clepper Jensen, Davis, Miranda Jordan, Mrs. Money</li>
                     <li><strong>Organization:</strong> Oceanus City Council, Green Guardian</li>
                     <li><strong>Vessel:</strong> Reef Guardian, Neptune, Mako, Remora</li>
                     <li><strong>Location:</strong> Himark Harbor</li>
                     <li><strong>Group:</strong> N/A</li>
                   </ul>
                 ")
               )
             )
    ),
    
    # Week comparison
    # Week comparison
    tabPanel("Week 1 vs Week 2 Communication Patterns",
             fluidRow(
               box(
                 width = 8,
                 title = "Hourly Communication Comparison (Week 1 vs Week 2)",
                 solidHeader = TRUE,
                 status = "primary",
                 plotOutput(ns("week_comparison_plot"), height = "500px"),
                 helpText("This chart shows the normalized proportion of messages sent during each hour, comparing the first and second week of communication.")
               ),
               box(
                 width = 4,
                 title = tagList(
                   "Filter by Entity Type",
                   tags$span(
                     icon("info-circle"),
                     style = "color: #31708f; cursor: help; margin-left: 6px;",
                     title = paste(
                       "• Select which entity types (Person, Vessel, etc.) you want included in the plot.\n",
                       "• Multiple types can be selected.\n",
                       "• Deselecting all will hide data for that type."
                     )
                   )
                 ),
                 solidHeader = TRUE,
                 status = "info",
                 shinyWidgets::pickerInput(
                   inputId = ns("entity_type_filter"),
                   label = "Select Entity Types to Include:",
                   choices = c("Person", "Vessel", "Organization", "Group", "Location"),
                   selected = c("Person", "Vessel", "Organization", "Group", "Location"),
                   multiple = TRUE,
                   options = list(
                     `actions-box` = TRUE,
                     `live-search` = TRUE
                   )
                 )
               )
               
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Findings",
                 solidHeader = TRUE,
                 status = "info",
                 HTML("
               <p>The visualization of hourly communication patterns reveals that message activity in Oceanus follows a pronounced daily cycle, with distinct peaks and lows across both observed weeks.</p>
               <p><strong>In Week 1</strong>, communication is highly concentrated in the morning hours, particularly between <strong>8 AM and 12 PM</strong>, where a majority of the messages are exchanged.</p>
               <p><strong>By contrast, Week 2</strong> shows a broader distribution of communication throughout the day, with notable increases in afternoon and evening activity.</p>
               <p>This shift suggests that while the first week’s communications were more focused and possibly related to regular planning or operational updates, the second week’s patterns reflect heightened activity or more dynamic coordination, potentially due to emergent events or increased urgency among entities.</p>
             ")
               )
             )
    )
    ,
    
    # Time-filtered analysis
    tabPanel("Time Filtered Chord Diagram, Timeline Table, and Heatmap by Time",
             fluidRow(
               box(
                 width = 12,
                 title = tagList(
                   "Select Time Slice (Every 1 Hour)",
                   tags$span(
                     icon("info-circle"),
                     style = "color: #31708f; cursor: help; margin-left: 6px;",
                     title = paste(
                       "• Use the slicer to choose an hourly window.\n",
                       "• Chord diagram and timeline will update based on this hour.\n",
                       "• Hover over heatmap to explore message volume by hour/date."
                     )
                   )
                 ),
                 solidHeader = TRUE,
                 status = "info",
                 uiOutput(ns("time_slider_ui")),
                 helpText("Choose an hour to view filtered communications, chord diagram, and timeline below.")
               )
                 ),
             
             fluidRow(
               box(
                 width = 8,
                 title = "Chord Diagram (Filtered by Selected Time Slice)",
                 solidHeader = TRUE,
                 status = "primary",
                 uiOutput(ns("chord_plot"))
               ),
               box(
                 width = 4,
                 title = "Top Communication Pairs (Filtered by Selected Time Slice)",
                 solidHeader = TRUE,
                 status = "primary",
                 DT::dataTableOutput(ns("chord_table")),
                 helpText("This table shows the most frequent sender-recipient pairs in the selected time slice.")
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Findings from Chord Diagram",
                 solidHeader = TRUE,
                 status = "info",
                 HTML("
               <p>The Chord visualization shows the overall count of sender-recipient messages across all time.</p>
               <p>This allows us to have an overview of specific communication flows to identify key pairs and communication intensity between them.</p>
               <p>The thickness of each ribbon (chord) represents the magnitude of the relationship. A thicker ribbon represents more frequent communications (sent + received) between a sender and recipient.</p>
               <p>Here, we have an overview of paired communicators who have higher frequencies. We also can see the links between communicators. These are the entities who communicated frequently with others that we might want to focus on:</p>
               <ul>
                 <li><strong>Person:</strong> The Intern, The Lookout, Clepper Jensen, Davis, Miranda Jordan, Mrs. Money</li>
                 <li><strong>Organization:</strong> Oceanus City Council, Green Guardian</li>
                 <li><strong>Vessel:</strong> Reef Guardian, Neptune, Mako, Remora</li>
                 <li><strong>Location:</strong> Himark Habor</li>
                 <li><strong>Group:</strong> N/A</li>
               </ul>
              <p>We also noticed that at times, certain individuals sent messages but there were no responses back. 
               This could possibly be due to pseudonyms being used to send or reply to the same content. 
               For instance, there was a message from <strong>Davis</strong> to <strong>Rodriguez</strong> on <strong>14 Oct around 12:13</strong>, 
               but there was no reply from Rodriguez. Upon closer inspection of the <em>content</em> field, 
               we found that Rodriguez was likely using the pseudonym <strong>Small Fry</strong>, 
               as the responses Davis received (originally addressed to Rodriguez) were consistent with Small Fry’s identity in the content around 12.14 on the same day.</p>
              <p>(Noticed error in original raw data where sender and recipient names might not match to the content. Traced it back to the raw data provided in json.)<p>
             ")
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Timeline Table (Filtered by Selected Time Slice)",
                 solidHeader = TRUE,
                 status = "primary",
                 collapsible = TRUE,
                 DT::dataTableOutput(ns("filtered_timeline_table"))
               )
             ),
             fluidRow(
               box(
                 width = 12,
                 title = "Summary of Daily and Hourly Communication by Static Heatmap",
                 solidHeader = TRUE,
                 status = "primary",
                 collapsible = TRUE,
                 plotly::plotlyOutput(ns("heatmap_daily_plot"), height = "400px"),
                 helpText("This shows message activity by hour and date. Hover to see exact count of messages.")
               )
             )
    )
  )
}
