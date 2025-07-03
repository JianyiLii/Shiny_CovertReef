# ================================
# Load Required Libraries
# ================================
pacman::p_load(
  shiny, shinydashboard, fresh,
  jsonlite, tidyverse, ggtext, knitr, lubridate, hms, scales,
  tidytext, tm, SnowballC, SmartEDA, patchwork, ggraph, tidygraph, igraph,
  ggiraph, plotly, wordcloud, ggh4x, visNetwork, RColorBrewer, circlize,
  ggalluvial, reactable, networkD3, highcharter, leaflet, conflicted
)

install.packages("conflicted")


# Settle Conflict issues
conflicts_prefer(shinydashboard::box)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(networkD3::JS)

# ================================
# Load Module Files
# ================================
source("wordcloud_module_ui.R")
source("wordcloud_module_server.R")
source("exploratory_module_ui.R")
source("exploratory_module_server.R")
source("data_prep_module_ui.R")  
source("data_prep_module_server.R")
source("question1_ui.R")
source("question1_server.R")
source("question2_ui.R")
source("question2_server.R")
source("question3_ui.R")
source("question3_server.R", encoding = "UTF-8")
source("question4_ui.R")
source("question4_server.R", encoding = "UTF-8")

# ================================
# Load Data Once at App Start
# ================================
# Loading
mc3_data <- fromJSON("data/MC3_graph.json")
mc3_schema <- fromJSON("data/MC3_schema.json")

# Nodes and edges as tibble
mc3_nodes_raw <- as_tibble(mc3_data$nodes)
mc3_edges_raw <- as_tibble(mc3_data$edges)

# Cleaning nodes
mc3_nodes_cleaned <- mc3_nodes_raw %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  select(-thing_collected, -time, -date, -friendship_type)

# Cleaning and wrangling edges
mc3_edges_cleaned <- mc3_edges_raw %>%
  rename(from_id = source, to_id = target) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  separate(to_id, into = c("to_id_supertype", "to_id_sub_type", "to_id_num"),
           sep = "_", remove = FALSE, fill = "right", extra = "merge") %>%
  filter(from_id %in% mc3_nodes_cleaned$id, to_id %in% mc3_nodes_cleaned$id)

# Create mapping of node ID to row index
node_index_lookup <- mc3_nodes_cleaned %>%
  mutate(.row_id = row_number()) %>%
  select(id, .row_id)

# Convert to integer indices and filter valid edges
mc3_edges_indexed <- mc3_edges_cleaned %>%
  left_join(node_index_lookup, by = c("from_id" = "id")) %>%
  rename(from = .row_id) %>%
  left_join(node_index_lookup, by = c("to_id" = "id")) %>%
  rename(to = .row_id) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  select(from, to, id, is_inferred, type,
         from_id, to_id, to_id_supertype, to_id_sub_type, to_id_num)

# Subset nodes to those used in edges
used_node_indices <- sort(unique(c(mc3_edges_indexed$from, mc3_edges_indexed$to)))
mc3_nodes_final <- mc3_nodes_cleaned %>%
  slice(used_node_indices) %>%
  mutate(new_index = row_number())

# Rebuild lookup and update edge indices
old_to_new_index <- tibble(
  old_index = used_node_indices,
  new_index = seq_along(used_node_indices)
)

mc3_edges_final <- mc3_edges_indexed %>%
  left_join(old_to_new_index, by = c("from" = "old_index")) %>%
  rename(from_new = new_index) %>%
  left_join(old_to_new_index, by = c("to" = "old_index")) %>%
  rename(to_new = new_index) %>%
  select(from = from_new, to = to_new,
         id, is_inferred, type,
         from_id, to_id, to_id_supertype, to_id_sub_type, to_id_num)


# ================================
# Define UI
# ================================
mytheme <- create_theme(
  adminlte_color(red = "#ffb6c1"),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#ffb6c1",         # sidebar background
    dark_hover_bg = "#ffb6c1",   # sidebar hover/active (same as background)
    dark_color = "#222222"       # sidebar text color
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
  )
)

header <- dashboardHeader(title = "🌊 Covert Reef 🪸")

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("Introduction", tabName = "Introduction", icon = icon("info-circle")),
    menuItem("Background", tabName = "Background", icon = icon("book-open")),
    menuItem("Setup", tabName = "Setup", icon = icon("tools")),
    menuItem("Data Preparation", tabName = "DataPrep", icon = icon("file")),
    menuItem("Exploratory", tabName = "Exploratory", icon = icon("globe")),
    menuItem("Question1", tabName = "Question1", icon = icon("magnifying-glass-chart")),
    menuItem("Question2", tabName = "Question2", icon = icon("circle-nodes")),
    menuItem("Question3", tabName = "Question3", icon = icon("masks-theater")),
    menuItem("Question4", tabName = "Question4", icon = icon("person-circle-question")),
    menuItem("Visit Vast Challenge", icon = icon("external-link-alt"),
             href = "https://vast-challenge.github.io/2025/")
  )
)

body <- dashboardBody(
  tags$head(
    tags$style(HTML(".skin-red .main-header .navbar,
      .skin-red .main-header .logo,
      .skin-red .main-sidebar,
      .skin-red .sidebar-menu>li>a {
        background-color: #ffb6c1 !important;
        color: #222 !important;
      }
      .skin-red .sidebar-menu>li.active>a,
      .skin-red .sidebar-menu>li:hover>a {
        background-color: #f8a6b1 !important;
        color: #222 !important;
      }
      .main-header > .navbar { margin-left: 250px; }
      h1#titleText {
        color: #8e44ad;
        font-weight: bold;
        text-align: center;
        margin-top: 40px;
      }
    "))
  ),
  use_theme(mytheme),
  tabItems(
    
    # ===============================
    # Introduction Tab
    # ===============================
    tabItem(tabName = "Introduction",
            h2("Welcome to the Covert Reef Dashboard!"),
            fluidRow(
              box(title = "About This Site", width = 12, solidHeader = TRUE, status = "info",
                  p("This dashboard is built for the VAST 2025 Challenge."),
                  p("Navigate using the sidebar.")
              )
            ),
            fluidRow(
              box(title = "Visual Thematic Overview", width = 12, solidHeader = TRUE, status = "danger",
                  wordcloudUI("wordcloud_intro")
              )
            )
    ),
    
    # ===============================
    # Background Story
    # ===============================
    tabItem(tabName = "Background",
            h2("📓 Background"),
            tabsetPanel(
              
              # --- Tab 1: Intro to Final Product ---
              tabPanel("🌊 Introduction to Final Product",
                       h4("1.1 Vast Challenge"),
                       p("In this study, we will be tackling Mini-case 3 of VAST Challenge 2025."),
                       h4("1.2 Background"),
                       p("Over the past decade, the community of Oceanus has faced numerous transformations and challenges evolving from its fishing-centric origins. Following major crackdowns on illegal fishing activities, suspects have shifted investments into more regulated sectors such as the ocean tourism industry, resulting in growing tensions."),
                       p("This increased tourism has recently attracted the likes of international pop star Sailor Shift, who announced plans to film a music video on the island."),
                       p("Clepper Jensen, a former analyst at FishEye and now a seasoned journalist for the Hacklee Herald, has been keenly observing these rising tensions. Recently, he turned his attention towards the temporary closure of Nemo Reef."),
                       p("By listening to radio communications and utilizing his investigative tools, Clepper uncovered a complex web of expedited approvals and secretive logistics..."),
                       p("Our task is to develop new and novel visualizations and visual analytics approaches to help Clepper get to the bottom of this story.")
              ),
              
              # --- Tab 2: Questions ---
              tabPanel("🧠 Questions",
                       h4("1.3 Questions"),
                       tags$ol(
                         
                         # Question 1
                         tags$li(
                           HTML("<b>Question 1:</b> Clepper found that messages frequently came in at around the same time each day."),
                           tags$ul(
                             tags$li("1a. Develop a graph-based visual analytics approach to identify any daily temporal patterns in communications."),
                             tags$li("1b. How do these patterns shift over the two weeks of observations?"),
                             tags$li("1c. Focus on a specific entity and use this information to determine who has influence over them.")
                           )
                         ),
                         
                         # Question 2
                         tags$li(
                           HTML("<b>Question 2:</b> Clepper has noticed that people often communicate with (or about) the same people or vessels, and that grouping them together may help with the investigation."),
                           tags$ul(
                             tags$li("2a. Use visual analytics to help Clepper understand and explore the interactions and relationships between vessels and people in the knowledge graph."),
                             tags$li("2b. Are there groups that are more closely associated? If so, what are the topic areas that are predominant for each group?"),
                             tags$li("For example, these groupings could be related to: Environmentalism (known associates of Green Guardians), Sailor Shift, and fishing/leisure vessels.")
                           )
                         ),
                         
                         # Question 3
                         tags$li(
                           HTML("<b>Question 3:</b> It was noted by Clepper's intern that some people and vessels are using pseudonyms to communicate."),
                           tags$ul(
                             tags$li(
                               "3a. Expanding upon your prior visual analytics, determine who is using pseudonyms to communicate, and what these pseudonyms are.",
                               tags$ul(
                                 tags$li('Some that Clepper has already identified include: "Boss", and "The Lookout", but there appear to be many more.'),
                                 tags$li("To complicate the matter, pseudonyms may be used by multiple people or vessels.")
                               )
                             ),
                             tags$li("3b. Describe how your visualizations make it easier for Clepper to identify common entities in the knowledge graph."),
                             tags$li("3c. How does your understanding of activities change given your understanding of pseudonyms?")
                           )
                         ),
                         
                         # Question 4
                         tags$li(
                           HTML("<b>Question 4:</b> Clepper suspects that Nadia Conti, who was formerly entangled in an illegal fishing scheme, may have continued illicit activity within Oceanus."),
                           tags$ul(
                             tags$li("4a. Through visual analytics, provide evidence that Nadia is, or is not, doing something illegal."),
                             tags$li("4b. Summarize Nadia's actions visually. Are Clepper's suspicions justified?")
                           )
                         )
                       )
              ),
              
              
              # --- Tab 3: Data ---
              tabPanel("📊 The Data",
                       h4("1.4 The Data"),
                       p("We used the dataset provided by VAST Challenge. The data contains 1,159 nodes and 3,226 edges. It allows us to build a knowledge graph from transcripts of boat radio communications over two weeks on Oceanus."),
                       p("Nodes represent people, events, locations, and vessels. Edges represent relationships and communications between them.")
              ),
              
              # --- Tab 4: Methodology ---
              tabPanel("🧪 Methodology",
                       h4("1.5 Methodology"),
                       p("To answer the questions, we explored communications and relationships among entities."),
                       tags$ul(
                         tags$li("Created subgraphs of networks and ego networks."),
                         tags$li("Used heatmaps to explore communication timings"),
                         tags$li("Used chord diagrams to explore group relationships."),
                         tags$li("Plotted timeline-based visualizations to observe patterns."),
                         tags$li("Used wordclouds and circular bar charts for summary."),
                         tags$li("Sankey diagram for identities"),
                         tags$li("Interpreted findings and built a narrative around actors and events.")
                       )
              )
            )
    ),
    
    # ===============================
    # Setup Tab
    # ===============================
    tabItem(tabName = "Setup",
            h2("🚢 Setup"),
            
            fluidRow(
              box(
                title = "Our Sub-Task for Take Home Ex 03",
                status = "info",
                solidHeader = TRUE,
                width = 12,
                p("To list, evaluate and determine the necessary R packages needed for our Shiny application are supported in R CRAN."),
                p("Explain how the data was loaded, cleaned, and structured for use in Shiny.")
              )
            ),
            
            fluidRow(
              box(title = "Tackling the Task", status = "danger", solidHeader = TRUE, width = 12,
                  p("Here, we loaded up our ship to set sail to work on our project with these R CRAN supported packages.")
              )
            ),
            
            fluidRow(
              box(title = "📦 1.1 Packages supported in R CRAN", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,
                  h4("🛠 Utility Tools & Graphing Tools"),
                  tags$ul(
                    tags$li("jsonlite – To parse JSON"),
                    tags$li("tidyverse – Data science tools"),
                    tags$li("ggtext – Tools for text formatting"),
                    tags$li("knitr – For better table displays"),
                    tags$li("lubridate – For processing date and time"),
                    tags$li("hms – For durations"),
                    tags$li("scales – For breaks and labels"),
                    tags$li("tidytext – For text mining functions"),
                    tags$li("tm – For text mining"),
                    tags$li("SnowballC – Porter's word stemming"),
                    tags$li("SmartEDA – EDA with some graphing"),
                    tags$li("reactable – Interactive data tables"),
                    tags$li("patchwork – Combine ggplot plots"),
                    tags$li("ggraph – Network plotting"),
                    tags$li("tidygraph – Graph manipulation"),
                    tags$li("igraph – Network analysis functions"),
                    tags$li("ggiraph – Interactive ggplot-based plots"),
                    tags$li("plotly – Interactive plots"),
                    tags$li("wordcloud – Word frequency plots"),
                    tags$li("ggh4x – Axis, facet, strip tweaks"),
                    tags$li("visNetwork – Interactive network visualisation"),
                    tags$li("RColorBrewer – Colour schemes for graphics"),
                    tags$li("circlize – Circular plots"),
                    tags$li("ggalluvial – Alluvial diagrams")
                  )
              )
            ),
            
            fluidRow(
              box(title = "🚀 1.2 Code to Load Packages", status = "warning", solidHeader = TRUE, width = 12,
                  p("We will use the code chunk below to check and load all required packages using `pacman::p_load()`."),
                  verbatimTextOutput("pkg_loader")
              )
            ),
            
            fluidRow(
              box(title = "🧰 1.3 Part of the Tidyverse", status = "warning", solidHeader = TRUE, width = 12,
                  tags$ul(
                    tags$li("stringr – Text wrapping & manipulation"),
                    tags$li("readr – Read CSV, TSV, and other rectangular data"),
                    tags$li("dplyr – Transform, filter, summarise data"),
                    tags$li("ggplot2 – Build visualizations")
                  )
              ),
              box(title = "🔄 1.4 Workarounds", status = "warning", solidHeader = TRUE, width = 12,
                  p("magick – replaced by "),
                  code("renderImage() + imageOutput()"),
                  br(),
                  p("wordcloud2 – used via "),
                  code("renderWordcloud2() + wordcloud2Output()")
              )
            ),
            # ===============================
            # 2.1 Inspecting Knowledge Graph Structure
            # ===============================            
            fluidRow(
              box(title = "📂 2.1 Loading Data", status = "success", solidHeader = TRUE, width = 12,
                  p("In the code chunk below, `fromJSON()` from the `jsonlite` package is used to import"),
                  code("mc3_graph.json"), p("and"),
                  code("MC3_schema.json"), p("into R and save the output objects."),
                  verbatimTextOutput("load_data_code"),
                  br(),
                  p("It contains graph data, where nodes can be accessed via nodes and edges via links.")
              )
            ),
            # ===============================
            # 2.2 Inspecting Knowledge Graph Structure
            # ===============================            
            fluidRow(
              box(title = "🎨 2.2 Defining Common Variables", status = "success", solidHeader = TRUE, width = 12,
                  p("We set styles and color variables for use across all network graphs."),
                  h4("Style and Colours"),
                  verbatimTextOutput("style_code")
              )
            ),
            # ===============================
            # 2.3 Inspecting Knowledge Graph Structure
            # ===============================
            fluidRow(
              box(title = "🔍 2.3 Inspecting Knowledge Graph Structure", status = "success", solidHeader = TRUE, width = 12,
                  p("We inspect the structure of the `mc3_data` knowledge graph using `glimpse()`."),
                  h4("The Code"),
                  code("glimpse(mc3_data)"),
                  br(), br(),
                  h4("The Result"),
                  verbatimTextOutput("glimpse_output")
              )
            ),
            
            # ===============================
            # 2.4 Extracting Edges and Nodes
            # ===============================
            fluidRow(
              box(title = "📊 2.4 Extracting Edges and Nodes Tables", status = "success", solidHeader = TRUE, width = 12,
                  p("We use `as_tibble()` to extract the nodes and edges tables from the raw `mc3_data`."),
                  h4("The Code"),
                  code("mc3_nodes_raw <- as_tibble(mc3_data$nodes)"),
                  br(),
                  code("mc3_edges_raw <- as_tibble(mc3_data$edges)")
              )
            ),
            
            fluidRow(
              box(title = "🧬 2.5 Nodes Structure", status = "success", solidHeader = TRUE, width = 12,
                  p("Exploratory summary of nodes data using `SmartEDA::ExpData()`."),
                  verbatimTextOutput("node_structure")
              )
            ),
            
            fluidRow(
              box(title = "🔗 2.6 Edges Structure", status = "success", solidHeader = TRUE, width = 12,
                  p("Exploratory summary of edges data using `SmartEDA::ExpData()`."),
                  verbatimTextOutput("edge_structure")
              )
            )
            
    ),
    
    # ===============================
    # Data Preparation
    # ===============================
    tabItem(
      tabName = "DataPrep",
      dataPrepUI("dataPrep")  # ✅ module UI goes inside tabItem
    ),
    
    # ===============================
    # Exploratory Tab
    # ===============================
    # exploratory_module_ui.R
    tabItem(
      tabName = "Exploratory",
      exploratoryUI("exploratory")  # ✅ module UI goes inside tabItem
    ),
    
    # ===============================
    # Other Question Tabs
    # ===============================
    
    tabItem(tabName = "Question1",
            h2("🔍 Question 1: Communication Analysis"),
            question1_ui("question1")
    ),
    tabItem(tabName = "Question2",
            h2("🔗 Question 2: Topic Clustering & Network Exploration"),
            question2_ui("question2")
    ),
    tabItem(tabName = "Question3",
            h2("🎭 Pseudonym Mapping: Real vs Observed Identities"),
            
            # 3a
            fluidRow(
              box(
                title = "3a) Identifying Pseudonyms", 
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                HTML("
              <b>Core Logic:</b><br>
              • If two names appear as sender and recipient in the same message, they <b>cannot belong to the same person</b> — they are <i>not aliases</i>.<br>
              • If two names sent a message at the <b>exact same time</b>, they <b>cannot belong to the same person</b>.<br><br>
              We created <b>Alluvial Diagrams</b> to chart:<br>
              <code>Real Identity → Observed Name → Community</code><br>
              This helps reveal overlapping or reused pseudonyms across entities.
            ")
              )
            ),
            
            # 3b
            fluidRow(
              box(
                title = "3b) Visualization for Clarity", 
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                HTML("
              We created a graph connecting characters' real names, pseudonyms, and communities. 
              Background details were incorporated — for example, <i>Davis</i> is a captain, and <i>Serenity</i> is a luxury yacht.<br><br>
              The interactive dropdown allows filtering by real identity, letting users focus on each person's alias network with clarity.
            ")
              )
            ),
            
            # 3c
            fluidRow(
              box(
                title = "3c) Impact of Pseudonyms on Activity Interpretation", 
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                HTML("
              Understanding pseudonyms significantly reshapes our interpretation of the events in Oceanus.<br><br>
              Without resolving aliases, the communication network appears fragmented — it may seem like dozens of separate individuals are involved. 
              However, by mapping pseudonyms to real identities, we discover that a small number of actors are coordinating more activity than initially apparent.<br><br>
              For example, a person using multiple pseudonyms may appear in many places at once — suggesting high influence or deception. 
              This reveals orchestrated behavior, such as coordinated logistics, manipulation of event timelines, or masking involvement in controversial operations.
            ")
              )
            ),
            
            # Visualization UI
            fluidRow(
              box(
                title = "Identity Flow Viewer", 
                width = 12, 
                solidHeader = TRUE, 
                status = "danger",
                question3_ui("question3")
              )
            )  # properly closes the tabItem for Question3
    ),
    
    
    tabItem(
      tabName = "Question4",
      tagList(
        h2("👧❓ Question 4: Nadia-Centric Analysis"),
        question4_ui("question4")
      )
    )
    
    
  )
)

# ================================
# Define UI
# ================================

ui <- dashboardPage(
  title = 'Covert Reef: VAST Challenge 2025',
  header = header,
  sidebar = sidebar,
  body = body,
  skin = 'red'
)

# ================================
# Define Server
# ================================

server <- function(input, output) {
  output$pkg_loader <- renderPrint({
    pacman::p_load(jsonlite, tidyverse, ggtext, knitr, lubridate, hms, scales,
                   tidytext, tm, SnowballC, SmartEDA, patchwork, ggraph, tidygraph,
                   igraph, ggiraph, plotly, wordcloud, ggh4x, visNetwork,
                   RColorBrewer, circlize, ggalluvial, reactable)
    cat("✔ Packages loaded with pacman::p_load()")
  })
  
  wordcloudServer("wordcloud_intro")
  
  exploratoryServer("exploratory",
                    nodes_df = mc3_nodes_final,
                    edges_df = mc3_edges_final,
                    nodes_raw = mc3_nodes_raw,
                    edges_raw = mc3_edges_raw)
  
  # Call Question 1 server and store returned outputs
  question1_data <- question1_server(
    id = "question1",
    mc3_nodes_raw = mc3_nodes_raw,
    mc3_edges_raw = mc3_edges_raw
  )
  
  # Pass its reactive comm_df into Question 2
  question2_server(
    id = "question2",
    mc3_nodes_raw = mc3_nodes_raw,
    mc3_edges_raw = mc3_edges_raw,
    comm_df = question1_data$comm_df  # ✅ use from question1_data
  )
  
  
  question3_server("question3", name_mapping_df = tibble())
  
  question4_server("question4",
                   mc3_nodes_raw = mc3_nodes_raw,
                   mc3_edges_raw = mc3_edges_raw,
                   comm_df = reactive({ comm_df_static }),
                   nadia_2hop_timeline_plot = nadia_2hop_timeline_plot,
                   nadia_network_findings_text = nadia_network_findings_text,
                   keyword_timeline_plot = keyword_timeline_plot,
                   two_word_timeline_plot = two_word_timeline_plot,
                   keyword_findings_text = keyword_findings_text,
                   suspicious_table_1 = suspicious_table_1,
                   suspicious_table_2 = suspicious_table_2,
                   suspicious_table_3 = suspicious_table_3,
                   final_findings_text = final_findings_text)
}

# ================================
# Run App
# ================================
shinyApp(ui = ui, server = server)





