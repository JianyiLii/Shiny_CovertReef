question4_ui <- function(id) {
  ns <- NS(id)
  
  tabBox(
    id = ns("q4_tabbox"),
    width = 12,
    
    # 4a
    tabPanel("🔗 4a: Ego Networks",
             tabsetPanel(
               tabPanel("🧬 Louvain Ego Network (ggraph)",
                        fluidRow(
                          box(
                            title = "Nadia Conti's Louvain Ego Network",
                            status = "primary", solidHeader = TRUE, width = 12,
                            plotOutput(ns("nadia_ego_plot"), height = "600px")
                          )
                        )
               ),
               tabPanel("🧬 Directed Ego Networks (visNetwork)",
                        fluidRow(
                          box(
                            title = "Sent Messages from Nadia",
                            status = "primary", solidHeader = TRUE, width = 6,
                            visNetworkOutput(ns("nadia_sent_plot"), height = "500px")
                          ),
                          box(
                            title = "Received Messages to Nadia",
                            status = "primary", solidHeader = TRUE, width = 6,
                            visNetworkOutput(ns("nadia_received_plot"), height = "500px")
                          )
                        ),
                        fluidRow(
                          box(
                            title = "📊 Communication Summary for Nadia Conti",
                            status = "primary", solidHeader = TRUE, width = 12,
                            htmlOutput(ns("nadia_summary_text"))
                          )
                        )
               )
             )
    ),
    
    # 4b Timeline & Keywords
    tabPanel("📅 4b: Timeline & Keywords",
             tabsetPanel(
               tabPanel("👧 Nadia's 3-hop Timeline",
                        fluidRow(
                          box(
                            width = 12,
                            title = tagList(
                              "Filter Suspicious Entities",
                              tags$span(
                                icon("info-circle"),
                                style = "color: #31708f; cursor: help; margin-left: 6px;",
                                title = paste(
                                  "• Use the dropdown to select suspicious entities for timeline filtering.\n",
                                  "• Use 'Show' to control if you want to see all, only suspicious, or non-suspicious communications.\n",
                                  "• Filter by role: sender, recipient, or either.\n",
                                  "• Use the buttons on the right to select all people, vessels, organizations, groups, or locations.\n",
                                  "• Click 'Reset' to clear your filters."
                                )
                              )
                            ),
                            
                            status = "info", solidHeader = TRUE, collapsible = TRUE,
                            fluidRow(
                              column(
                                width = 8,
                                shinyWidgets::pickerInput(
                                  inputId = ns("suspicious_selected_name"),
                                  label = strong("Select one or more entities:"),
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(
                                    `actions-box` = TRUE,
                                    `live-search` = TRUE,
                                    `none-selected-text` = "No entities selected"
                                  )
                                ),
                                radioButtons(
                                  inputId = ns("susp_mode"),
                                  label = strong("Show:"),
                                  choices = c(
                                    "All 3-hop Communications" = "all",
                                    "Only Suspicious Communications" = "only",
                                    "Only Non-Suspicious Communications" = "non"
                                  ),
                                  selected = "all",
                                  inline = TRUE
                                ),
                                radioButtons(
                                  inputId = ns("susp_role"),
                                  label = strong("Filter role:"),
                                  choices = c("Either" = "either", "Sender Only" = "sender", "Recipient Only" = "recipient"),
                                  selected = "either",
                                  inline = TRUE
                                ),
                                helpText(HTML(paste(
                                  "<strong>Tips:</strong>",
                                  "Use the buttons on the right to select all people, vessels, or organizations, or reset your selection.",
                                  sep = "<br/>"
                                )))
                              ),
                              column(
                                width = 4,
                                br(),
                                actionButton(ns("susp_select_people"), label = tagList(icon("user"), "All People"),
                                             style = "background-color: #e0f0ff; color: #003366; font-weight: bold; width: 100%;"),
                                br(), br(),
                                actionButton(ns("susp_select_vessels"), label = tagList(icon("ship"), "All Vessels"),
                                             style = "background-color: #e0ffe0; color: #004400; font-weight: bold; width: 100%;"),
                                br(), br(),
                                actionButton(ns("susp_select_groups"), label = tagList(icon("users"), "All Groups"),
                                             style = "background-color: #f0e6ff; color: #4b0082; font-weight: bold; width: 100%;"),
                                br(), br(),
                                actionButton(ns("susp_select_locations"), label = tagList(icon("map-marker-alt"), "All Locations"),
                                             style = "background-color: #e6ffe6; color: #006600; font-weight: bold; width: 100%;"),
                                br(), br(),
                                actionButton(ns("susp_select_orgs"), label = tagList(icon("building"), "All Orgs"),
                                             style = "background-color: #fff0cc; color: #8b4513; font-weight: bold; width: 100%;"),
                                br(), br(),
                                actionButton(ns("susp_reset"), label = tagList(icon("rotate"), "Reset"),
                                             style = "background-color: #ffe0e0; color: #660000; font-weight: bold; width: 100%;")
                              )
                            )
                          )
                        ),
                        fluidRow(
                          box(
                            title = "🕒 3-Hop Timeline (Filtered)",
                            status = "primary", solidHeader = TRUE, width = 12,
                            collapsible = TRUE,
                            plotlyOutput(ns("suspicious_timeline_plot"), height = "500px")
                          )
                        ),
                        fluidRow(
                          box(
                            title = "🕒 Communication Table (Filtered)",
                            status = "primary", solidHeader = TRUE, width = 12,
                            collapsible = TRUE,
                            DT::dataTableOutput(ns("suspicious_entity_table"))
                          )
                        )
               ),
               tabPanel("🔑 Keyword Query Timeline",
                        fluidRow(
                          box(
                            title = tagList(
                              "Click a Keyword to View Timeline",
                              tags$span(
                                icon("info-circle"),
                                style = "color: #31708f; cursor: help; margin-left: 6px;",
                                title = paste(
                                  "• Click one keyword to show the timeline of messages mentioning it.\n",
                                  "• This will update the keyword timeline plot and filtered message table below.\n",
                                  "• Useful for quickly finding conversations tied to specific terms like '1045', 'Permit', or 'Mining'."
                                )
                              )
                            ),
                            
                            status = "info", solidHeader = TRUE, width = 12,
                            shinyWidgets::radioGroupButtons(
                              inputId = ns("keyword_selected"),
                              label = strong("Select a keyword:"),
                              choices = c("1045", "7844", "Permit", "Music Video", "Reef Operation", "Mining", "Lighting"),
                              justified = TRUE,
                              status = "light",
                              size = "sm",
                              checkIcon = list(yes = icon("check"), no = icon("")),
                              direction = "horizontal"
                            )
                          )
                        ),
                        fluidRow(
                          box(
                            title = "Timeline of Matching Communications",
                            status = "primary", solidHeader = TRUE, width = 12,
                            collapsible = TRUE,
                            plotlyOutput(ns("keyword_timeline_plot"), height = "500px")
                          )
                        ),
                        fluidRow(
                          box(
                            title = "🕒 Communication Table (Filtered)",
                            status = "primary", solidHeader = TRUE, width = 12,
                            collapsible = TRUE,
                            reactable::reactableOutput(ns("keyword_entity_table"))
                          )
                        )
               )
             )
    ),
    
    # Discussion tab
    tabPanel("❓ Discussion & Summary",
             fluidRow(
               box(
                 title = "Summary of Findings",
                 status = "success", solidHeader = TRUE, width = 12,
                 htmlOutput(ns("final_findings_text"))
               )
             )
    )
  )
}
