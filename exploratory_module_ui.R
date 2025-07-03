# exploratory_module_ui.R
exploratoryUI <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = "Exploratory",
    fluidPage(
      h2("🌎 Exploratory Data Analysis"),
      
      tabsetPanel(
        
        # === Node-Level EDA ===
        tabPanel("Node-Level EDA",
                 h3("1.1 Overview"),
                 p("We will use the EDA findings to determine data to focus on or eliminate. From the bar charts and the original data on mc3_nodes_raw, it was observed that:"),
                 tags$ul(
                   tags$li("Nodes were one of three types (Entity, Event, Relationship), where each of these types have their sub_types. Majority were of event type, followed by relationship, and entity."),
                   tags$li("There were 25 subtypes. Communications made up the bulk of the sub_type for Events. Coordinates made up the bulk of the sub_type for Relationship. The additional node sub_types not mentioned in the VAST 2025 MC3 Data Description under Node Attributes were: fishing, communication and coordinates.")
                 ),
                 
                 br(),
                 h3("1.2 Node Subtype Distributions"),
                 fluidRow(
                   column(6,
                          h4("Event Subtypes"),
                          plotOutput(ns("plot_event_subtypes"))
                   ),
                   column(6,
                          h4("Entity Subtypes"),
                          plotOutput(ns("plot_entity_subtypes"))
                   )
                 ),
                 fluidRow(
                   column(6,
                          h4("Relationship Subtypes"),
                          plotOutput(ns("plot_relationship_subtypes"))
                   )
                 ),
                 
                 br(),
                 h3("1.3 Node Type Distributions"),
                 fluidRow(uiOutput(ns("all_node_plots"))),
                 
                 br(),
                 h3("1.4 Observations by Node Type"),
                 h4("Event Types"),
                 tags$ul(
                   tags$li("Findings field were filled when there were monitoring_type."),
                   tags$li("Content refers to radio communication content."),
                   tags$li("Results field were filled when there were assessment_type performed."),
                   tags$li("When there is an enforcement_type of enforcement operations or warnings, there might be an outcome at times."),
                   tags$li("When there is a movement_type, there might be a place of destination at times.")
                 ),
                 h4("Relationship Types"),
                 tags$ul(
                   tags$li("When the subtype was coordinate, there were data in the field named coordination_types."),
                   tags$li("When the subtype was operate, there were data in the field named operational_roles."),
                   tags$li("When there is a jurisdiction_type, there might be an authority_level."),
                   tags$li("There are only restricted or special access data within permission_types."),
                   tags$li("When there is a report_type of data transmission or environmental report, there might be a submission_date.")
                 ),
                 h4("Entity Types"),
                 tags$ul(
                   tags$li("The 5 id under Group sub-types were not very useful information.")
                 )
        ),
        
        # === Edge-Level EDA ===
        tabPanel("Edge-Level EDA",
                 h3("1.5 Edge Attribute Overview"),
                 p("The code chunk below used ExpCatViz() of SmartEDA package to reveal the frequency distribution of all categorical fields in mc3_edges_raw tibble dataframe."),
                 p("Entities are connected by edges to other Entities via an Event or Relationship node. The one exception to this is the Communication Event subtype, which is additionally linked to either an Event or Relationship node. The type field denotes the connector or edge type for the Entities, Event, and Relationship nodes. The edges are one of these: received, evidence_for, sent, NA."),
                 fluidRow(
                   column(12, plotOutput(ns("plot_edge_exp_catviz")))
                 )
        ),
        
        # === Insights ===
        tabPanel("Further Insights",
                 h3("1.6 Insights and Courses of Action"),
                 p("This section summarizes exploratory findings on non-graph-specific fields such as metadata, timing, fields that were sparsely populated, and any additional information used to guide downstream analysis."),
                 tags$ul(
                   tags$li("Sparse fields identified include: activity_type, references, dates, time, and friendship_type. These were excluded from analysis."),
                   tags$li("Event types like assessment_type, movement_type, enforcement_type, permission_type, report_type, and authority_level had very low prevalence (<5%) and were deprioritized."),
                   tags$li("Key focus was placed on Event_Communication, Event_Monitoring, and Event_VesselMovement due to richer and more interpretable content."),
                   tags$li("Mapped fields such as coordination_types, operational_roles, outcome, destination were selectively examined depending on subtype presence.")
                 )
        ),
        
        # === Knowledge Graph ===
        tabPanel("Knowledge Graph",
                 h3("1.7 Interactive Knowledge Graph"),
                 p("This interactive visualization enables deeper inspection of nodes and their connections using the visNetwork package."),
                 p("Nodes are styled by entity sub-type or category, and hovering reveals tooltips with important fields such as findings, coordination_type, content, etc."),
                 fluidRow(
                   column(12, visNetworkOutput(ns("knowledge_graph"), height = "700px"))
                 )
        )
      )
    )
  )
}
