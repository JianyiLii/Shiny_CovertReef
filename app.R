# ================================
# Load Required Libraries
# ================================
pacman::p_load(
  shiny, shinydashboard, fresh, shinyWidgets,
  jsonlite, tidyverse, ggtext, knitr, lubridate, hms, scales, tidyr,
  tidytext, tm, SnowballC, SmartEDA, patchwork, ggraph, tidygraph, igraph,
  ggiraph, plotly, wordcloud, ggh4x, visNetwork, RColorBrewer, circlize,
  ggalluvial, reactable, networkD3, highcharter, leaflet, conflicted,
  lubridate
)

# Settle Conflict issues
conflicts_prefer(shinydashboard::box)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(networkD3::JS)
conflicted::conflicts_prefer(igraph::as_data_frame)

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
# Create comm_df_static
# ================================

# Step 1: Clean nodes
nodes_static <- mc3_nodes_raw %>%
  mutate(id = as.character(id)) %>%
  filter(!is.na(id)) %>%
  distinct(id, .keep_all = TRUE) %>%
  rename(supertype = type) %>%
  select(id, name, sub_type, supertype, content, timestamp)

# Step 2: Clean edges
edges_static <- mc3_edges_raw %>%
  rename(from_id = source, to_id = target, edge_type = type) %>%
  mutate(across(c(from_id, to_id), as.character)) %>%
  filter(from_id %in% nodes_static$id, to_id %in% nodes_static$id)

# Step 3: Build comm_df_static based on sent → received merge
comm_df_static <- edges_static %>%
  filter(edge_type == "sent") %>%
  mutate(sender_id_actual = from_id) %>%
  left_join(nodes_static %>% select(id, content, timestamp), by = c("to_id" = "id")) %>%
  rename(event_id = to_id, event_content = content, event_timestamp = timestamp) %>%
  left_join(edges_static %>%
              filter(edge_type == "received") %>%
              select(event_id_match = from_id, recipient_id = to_id),
            by = c("event_id" = "event_id_match")) %>%
  left_join(nodes_static %>% select(id, name, sub_type), by = c("sender_id_actual" = "id")) %>%
  rename(sender_name = name, sender_sub_type = sub_type) %>%
  left_join(nodes_static %>% select(id, name, sub_type), by = c("recipient_id" = "id")) %>%
  rename(recipient_name = name, recipient_sub_type = sub_type) %>%
  mutate(
    timestamp = as.POSIXct(event_timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    date = as.Date(timestamp),
    time = format(timestamp, "%H:%M:%S")
  ) %>%
  select(
    communication_type = edge_type,
    sender_id = sender_id_actual,
    sender_name,
    sender_sub_type,
    recipient_id,
    recipient_name,
    recipient_sub_type,
    event_id,
    content = event_content,
    timestamp
  )


# ===============================================
# GLOBAL: Nadia Conti Ego Network Preprocessing
# ===============================================
# --- Define color and shape legends for visNetwork ---
node_legend_colors_plot <- c(
  "Person" = "#88CCEE",
  "Vessel" = "#D55E00",
  "Organization" = "#117733",
  "Location" = "#AA4499",
  "Group" = "#CC79A7",
  "Event" = "#DDCC77",
  "Relationship" = "#AF8DC3",
  "Nadia Conti" = "red"
)

node_legend_shapes_plot <- c(
  "Person" = "dot",
  "Vessel" = "triangle",
  "Organization" = "square",
  "Location" = "diamond",
  "Group" = "circle",
  "Event" = "star",
  "Relationship" = "box",
  "Nadia Conti" = "star"
)


# --- STEP 1: Extract Nadia’s ID from mc3_nodes_cleaned ---
nadia_id <- mc3_nodes_cleaned %>%
  filter(name == "Nadia Conti") %>%
  pull(id) %>%
  first()

if (is.null(nadia_id) || is.na(nadia_id)) stop("Nadia Conti not found in node list.")

# --- STEP 2: Build undirected edge list from comm_df_static ---
comm_edges <- comm_df_static %>%
  filter(!is.na(sender_id), !is.na(recipient_id)) %>%
  select(from = sender_id, to = recipient_id) %>%
  distinct() %>%
  filter(from != to)  # Remove self-loops

# --- STEP 3: Build graph ---
g_full <- tbl_graph(edges = comm_edges, directed = FALSE)

# --- STEP 4: Confirm Nadia exists in graph ---
if (!"Nadia Conti" %in% V(g_full)$name) {
  stop("Nadia Conti not found in comm_df_static network.")
}

# --- STEP 5: Create 1-hop ego network ---
g_ego <- make_ego_graph(as.igraph(g_full), order = 1, nodes = which(V(g_full)$name == "Nadia Conti"), mode = "all")[[1]]

# --- STEP 6: Convert to tidygraph and run Louvain + PageRank ---
g_ego_tbl <- as_tbl_graph(g_ego) %>%
  to_undirected() %>%
  activate(nodes) %>%
  mutate(
    community = group_louvain(),
    pagerank = centrality_pagerank()
  )

# --- STEP 7: Plot with ggraph ---
set.seed(1234)
nadia_ego_plot <- ggraph(g_ego_tbl, layout = "fr") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(aes(size = pagerank, color = as.factor(community)), alpha = 0.9) +
  geom_node_text(aes(label = name), repel = TRUE, size = 6) +
  scale_color_brewer(palette = "Set2") +
  theme_void() +
  labs(
    title = "Nadia Conti’s Ego Network (1-hop)",
    subtitle = "Nodes sized by PageRank, colored by Louvain community",
    color = "Community", size = "PageRank"
  )

# ================================
# Nadia's Sent and Received Plots
# ================================

# --- Step 1: Identify Nadia’s ID and sub_type from cleaned nodes ---
nadia_info <- mc3_nodes_cleaned %>%
  filter(name == "Nadia Conti") %>%
  select(id, sub_type)

nadia_id <- nadia_info %>% pull(id)
nadia_sub_type <- nadia_info %>% pull(sub_type)

if (length(nadia_id) == 0) {
  stop("Nadia Conti not found in the nodes data.")
} else if (length(nadia_id) > 1) {
  warning("Multiple entries found for Nadia Conti. Using the first one.")
  nadia_id <- nadia_id[1]
  nadia_sub_type <- nadia_sub_type[1]
}

# --- Step 2: Extract directed communications involving Nadia ---

# (A) Sent: Nadia → others
edges_sent <- comm_df_static %>%
  filter(sender_id == nadia_id) %>%
  count(from_id = sender_id, to_id = recipient_id, name = "width") %>%
  filter(!is.na(to_id))

# (B) Received: others → Nadia
edges_received <- comm_df_static %>%
  filter(recipient_id == nadia_id) %>%
  count(from_id = sender_id, to_id = recipient_id, name = "width") %>%
  filter(!is.na(from_id))

# --- Step 3: Shared plotting function using visNetwork ---
build_directed_vis_plot <- function(edges_df, all_nodes) {
  node_ids <- unique(c(edges_df$from_id, edges_df$to_id))
  
  nodes <- all_nodes %>%
    filter(id %in% node_ids) %>%
    mutate(
      group = ifelse(name == "Nadia Conti", "Nadia Conti", sub_type),
      label = name,
      title = paste0("Name: ", name, "<br>Type: ", sub_type),
      shape = node_legend_shapes_plot[group],
      color = node_legend_colors_plot[group]
    ) %>%
    select(id, label, group, title, shape, color)
  
  edges <- edges_df %>%
    filter(!is.na(from_id), !is.na(to_id)) %>%
    select(from = from_id, to = to_id, width) %>%
    mutate(arrows = "to")
  
  visNetwork(nodes, edges, width = "100%", height = "500px") %>%
    visEdges(arrows = list(to = list(enabled = TRUE))) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visLegend(
      addNodes = tibble(
        label = names(node_legend_shapes_plot),
        shape = unname(node_legend_shapes_plot),
        color = unname(node_legend_colors_plot)
      ),
      useGroups = FALSE,
      ncol = 2,
      position = "left"
    )
}

# --- Step 4: Build Plots ---
nadia_sent_plot <- build_directed_vis_plot(edges_sent, mc3_nodes_cleaned)
nadia_received_plot <- build_directed_vis_plot(edges_received, mc3_nodes_cleaned)

# --- Step 5: Wrap into reactive-friendly objects (optional) ---
nadia_sent_plot_output <- reactiveVal(nadia_sent_plot)
nadia_received_plot_output <- reactiveVal(nadia_received_plot)




# ====================================================================
# GLOBAL: Question 4B - Suspicious Timeline & Keyword Timelines
# ====================================================================

library(igraph)
library(tidytext)
library(ggplot2)


# Step 1: Identify Nadia's ID
nadia_id <- mc3_nodes_cleaned %>%
  filter(name == "Nadia Conti") %>%
  pull(id) %>%
  first()

if (is.null(nadia_id)) stop("Nadia Conti not found in nodes.")

# Step 2: Build directed communication graph
g_comm <- graph_from_data_frame(
  d = comm_df_static %>% select(from = sender_id, to = recipient_id),
  directed = TRUE
)

# Step 3: Get 3-hop neighbors
ego_nodes_3hop <- make_ego_graph(g_comm, order = 3, nodes = nadia_id, mode = "all")[[1]] %>%
  igraph::V() %>%
  names()

# Step 4: Filter comm_df_static for messages involving 3-hop nodes
df_3hop <- comm_df_static %>%
  filter(sender_id %in% ego_nodes_3hop | recipient_id %in% ego_nodes_3hop) %>%
  filter(!is.na(timestamp)) %>%
  mutate(
    entity = ifelse(sender_name == "Nadia Conti", recipient_name, sender_name),
    entity_type = ifelse(sender_name == "Nadia Conti", recipient_sub_type, sender_sub_type)
  )

# Step 5: Filter suspicious content
suspicious_keywords <- c("package", "dock", "route", "loading", "shipment", "secure")

df_suspicious <- df_3hop %>%
  filter(str_detect(tolower(content), str_c(suspicious_keywords, collapse = "|"))) %>%
  mutate(
    comm_date = as.Date(timestamp),
    comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
    wrapped_content = str_wrap(content, width = 50),
    tooltip_text = paste0(
      "<b>Date:</b> ", comm_date, "<br>",
      "<b>Time:</b> ", format(timestamp, "%H:%M:%S"), "<br>",
      "<b>From:</b> ", sender_name, "<br>",
      "<b>To:</b> ", recipient_name, "<br>",
      "<b>Event ID:</b> ", event_id, "<br><br>",
      "<b>Content:</b><br>", wrapped_content
    )
  )


# === Suspicious Communications Timeline (Faceted by Entity Type) ===
p_suspicious <- ggplot(df_suspicious, aes(x = comm_date, y = comm_time_of_day)) +
  geom_point(
    aes(color = entity, shape = entity_type, text = tooltip_text),
    size = 2, alpha = 0.7, show.legend = c(color = TRUE, shape = FALSE)
  ) +
  scale_shape_manual(values = c("Person" = 16, "Vessel" = 17, "Organization" = 15, "Location" = 18)) +
  facet_wrap(~entity_type, ncol = 1) +
  scale_y_time(
    limits = hms::as_hms(c("08:00:00", "14:00:00")),
    breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
    labels = sprintf("%02d:00", 8:14)
  ) +
  scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
  labs(
    title = "Suspicious Communications Timeline (3-Hop)",
    subtitle = "Faceted by Entity Type",
    x = "Date", y = "Time of Day", color = "Entity"
  ) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

nadia_3hop_timeline_plot <- ggplotly(p_suspicious, tooltip = "text")

# === Keyword Timeline (Single Words) ===
stop_words <- tidytext::stop_words

keyword_counts <- df_suspicious %>%
  unnest_tokens(word, content) %>%
  filter(!word %in% stop_words$word) %>%
  count(date = as.Date(timestamp), word, sort = TRUE) %>%
  group_by(word) %>%
  top_n(10, n) %>%
  ungroup()

p_keyword <- ggplot(keyword_counts, aes(x = date, y = n, fill = word)) +
  geom_col(position = "dodge") +
  labs(
    title = "Single-Word Keyword Timeline",
    x = "Date",
    y = "Mentions"
  ) +
  theme_minimal()

keyword_timeline_plot <- ggplotly(p_keyword)

# === Two-Word Keyword Timeline (Bigrams) ===
bigram_counts <- df_suspicious %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2) %>%
  count(date = as.Date(timestamp), bigram, sort = TRUE) %>%
  group_by(bigram) %>%
  top_n(10, n) %>%
  ungroup()

p_bigram <- ggplot(bigram_counts, aes(x = date, y = n, fill = bigram)) +
  geom_col(position = "dodge") +
  labs(
    title = "Two-Word (Bigram) Keyword Timeline",
    x = "Date",
    y = "Mentions"
  ) +
  theme_minimal()

two_word_timeline_plot <- ggplotly(p_bigram)

# === Suspicious Entities Table ===
suspicious_table_1 <- df_suspicious %>%
  select(timestamp, sender_name, recipient_name, content) %>%
  arrange(desc(timestamp)) %>%
  reactable(
    searchable = TRUE,
    columns = list(
      timestamp = colDef(name = "Timestamp"),
      sender_name = colDef(name = "Sender"),
      recipient_name = colDef(name = "Recipient"),
      content = colDef(name = "Message Content")
    )
  )

# === Final Findings HTML ===
final_findings_text <- paste0(
  "<h4><strong>Summary of Final Findings</strong></h4>",
  "<ul>",
  "<li><strong>Suspicious interactions</strong> around Nadia Conti show frequent use of words like 'permit' and 'reef operation'.</li>",
  "<li>Multiple messages reference <strong>shipping activities</strong> and <strong>coordination efforts</strong>.</li>",
  "<li>The timing and parties involved suggest a <strong>hidden operation</strong> spanning multiple vessels and organizations.</li>",
  "</ul>",
  "<br/><hr/><br/>",
  
  "<h4><strong>Nadia’s Ego Network – Louvain Community</strong></h4>",
  "<p>We wanted to find out if there were subcommunities within Nadia’s direct network that worked closely together. This helps uncover potentially illicit behaviours.</p>",
  "<p>The <strong>orange community</strong> was possibly involved in Sailor Shifts’s music video, while the <strong>green community</strong> likely involved officials, harbour, and conservation teams ensuring compliance.</p>",
  "<p>Nadia, Elise, and Marlin were orange nodes directly linked to green nodes.</p>",
  "<br/><hr/><br/>",
  
  "<h4><strong>Sent and Received Ego Networks</strong></h4>",
  "<p>We further analyzed Nadia’s correspondences. The first graph shows Nadia’s <strong>sent</strong> messages and the second graph her <strong>received</strong> messages.</p>",
  "<ul>",
  "<li><strong>Nadia sent</strong> only 8 messages but <strong>received</strong> 18 – possibly indicating pseudonym use.</li>",
  "<li>Most messages were sent to <strong>Liam (2)</strong> and <strong>Neptune (2)</strong>.</li>",
  "<li>She received more from <strong>Davis (5)</strong>, <strong>Elise (3)</strong>, and <strong>Liam (2)</strong>.</li>",
  "</ul>",
  "<p>Nodes involved include: Nadia, Davis, Elise, Haacklee Harbor, Liam, Marlin, Neptune, Oceanus City Council, Remora, Rodriguez, Sailor Shifts Team, Sentinel, V. Miesel Shipping.</p>",
  "<br/><hr/><br/>",
  
  "<h4><strong>Key Q&amp;A Findings</strong></h4>",
  "<table class='table table-bordered' style='font-size: 90%;'>",
  "<thead><tr><th>Question</th><th>Answer</th></tr></thead><tbody>",
  
  "<tr><td><strong>Who were Nadia’s direct contacts (1-hop)? Any suspicious ones?</strong></td>",
  "<td>Liam, Elise, and Davis. Liam was the middleman in Nadia’s Louvain community. Elise, Liam, EcoVigil, Sentinel, Oceanus City Council, and V. Miesel Shipping had suspicious relationships.</td></tr>",
  
  "<tr><td><strong>Any events or relationships directly linked to Nadia that hint at illicit activity?</strong></td>",
  "<td>Rodriguez, previously linked to mining, was a focus. Communications mentioning 'mining' and Rodriguez were monitored.</td></tr>",
  
  "<tr><td><strong>Which vessel was permit #CR-7844 prepared for?</strong></td>",
  "<td>For V. Miesel’s Marine Research Permit, involving Mako (lead vessel), Neptune, and Remora.</td></tr>",
  
  "<tr><td><strong>What suspicious activity occurred at Nemo Reef, and when?</strong></td>",
  "<td>A music video production on 14 Oct 2040.</td></tr>",
  
  "<tr><td><strong>Why was underwater lighting used at Nemo Reef?</strong></td>",
  "<td>For the same music video production.</td></tr>",
  
  "<tr><td><strong>What were the expedited approvals and secret logistics?</strong></td>",
  "<td>Permits CR-788 and NR-1045 were rushed. Secretive equipment and crates were used on vessels during production.</td></tr>",
  
  "<tr><td><strong>Who were the key Oceanus officials, Sailor Shift’s team, local families, and conservation groups?</strong></td>",
  "<td><strong>Officials:</strong> Commissioner Blake, Torres, Knowles, Jensen, Liam.<br/>
       <strong>Team:</strong> Boss, Council Knowles, Davis, Liam, Mako, Mrs. Money, Nadia, Neptune, Remora, Rodriguez, Sam, Samantha Blake, Small Fry, The Accountant, The Intern, The Middleman.<br/>
       <strong>Families:</strong> Council Knowles, V. Miesel Shipping.<br/>
       <strong>Conservationists:</strong> Defender, EcoVigil, Green Guardians, Horizon, Kelly, Reef Guardians, Seawatch, Sentinel, The Lookout.</td></tr>",
  
  "<tr><td><strong>Was the music video legal?</strong></td>",
  "<td>No mining or environmental damage occurred, but no prior environmental assessment was done. Legality depends on whether assessments were mandatory for commercial reef activity.</td></tr>",
  
  "</tbody></table>"
)




# ================================
# Define UI
# ================================
mytheme <- create_theme(
  adminlte_color(red = "#9A3E41"),
  adminlte_sidebar(
    width = "250px",
    dark_bg = "#F8F8F8",
    dark_hover_bg = "#9A3E41",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#FFF",
    info_box_bg = "#FFF"
  )
)

header <- dashboardHeader(title = "Covert Reef")

sidebar <- dashboardSidebar(
  tags$style(HTML(".main-sidebar { width: 250px; }
    .main-header > .navbar { margin-left: 250px; }
    .box.box-solid.box-info>.box-header {
      color:#000000; background:#F8F8F8;
    }
    .box.box-solid.box-info {
      border-color:#9A3E41;
      background: #F8F8F8;
    }")),
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
                           HTML("<b>Question 3:</b> It was noted by Clepper’s intern that some people and vessels are using pseudonyms to communicate."),
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
                             tags$li("4b. Summarize Nadia’s actions visually. Are Clepper’s suspicions justified?")
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
                         tags$li("Alluvial diagram for identities"),
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
              box(title = "What we did in Setup?", status = "info", solidHeader = TRUE, width = 12,
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
                    tags$li("ggalluvial – Alluvial diagrams"),
                    tags$li("shinyWidgets – Collection of custom input controls and user interface components")
                  
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
              box(
                title = "🎨 2.2 Defining Common Variables",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                p("We set styles and color variables for use across all network graphs.")
              )
            ),
            # ===============================
            # 2.3 Inspecting Knowledge Graph Structure
            # ===============================
            fluidRow(
              box(
                title = "🔍 2.3 Inspecting Knowledge Graph Structure",
                status = "success",
                solidHeader = TRUE,
                width = 12,
                p("We inspect the structure of the `mc3_data` knowledge graph using `glimpse()`."),
                h4("The Code"),
                code("glimpse(mc3_data)"),
                br(), br(),
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
      dataPrepUI("dataPrep")  # Module UI goes inside tabItem
    ),
    
    # ===============================
    # Exploratory Tab
    # ===============================
    # exploratory_module_ui.R
    tabItem(
      tabName = "Exploratory",
      exploratoryUI("exploratory")  # Module UI goes inside tabItem
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
            h2("🎭 Question 3: Pseudonym Mapping: Real vs Observed Identities"),
            
            # 3a
            fluidRow(
              box(
                title = "3a) Identifying Pseudonyms", 
                status = "primary", 
                solidHeader = TRUE, 
                width = 12,
                HTML("
              <b>From core logic in question 2:</b><br>
              • If two names appear as sender and recipient in the same message, they <b>cannot belong to the same person</b> — they are <i>not aliases</i>.<br>
              • If two names sent a message at the <b>exact same time</b>, they <b>cannot belong to the same person</b>.<br><br>
              After reading the conversations in question 2 and uncovering the alias, we created <b>Alluvial Diagrams</b> to chart:<br>
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
              We created a graph connecting characters’ real names, pseudonyms, and communities. 
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
#  Generate g_pv_static outside the module for centrality and pagerank
# ================================
edge_tbl_static <- comm_df_static %>%
  filter(
    sender_sub_type %in% c("Person", "Vessel"),
    recipient_sub_type %in% c("Person", "Vessel")
  ) %>%
  count(sender_name, recipient_name, name = "weight") %>%
  rename(from = sender_name, to = recipient_name)

g_pv_static <- as_tbl_graph(edge_tbl_static, directed = FALSE) %>%
  mutate(
    community = group_louvain(),
    pagerank = centrality_pagerank()
  )


# ================================
#  Generate g_pv_static outside the module for centrality and pagerank
  # ================================
edge_tbl_static <- comm_df_static %>%
  filter(
    sender_sub_type %in% c("Person", "Vessel"),
    recipient_sub_type %in% c("Person", "Vessel")
  ) %>%
  count(sender_name, recipient_name, name = "weight") %>%
  rename(from = sender_name, to = recipient_name)

g_pv_static <- as_tbl_graph(edge_tbl_static, directed = FALSE) %>%
  mutate(
    community = group_louvain(),
    pagerank = centrality_pagerank()
  )


# ================================
# Add this to global.R
# ================================

# Extract person-vessel edges 
comm_bigrams <- comm_df_static %>%  #   static communication data
  filter(
    sender_sub_type %in% c("Person", "Vessel"),
    recipient_sub_type %in% c("Person", "Vessel")
  ) %>%
  select(sender_name, recipient_name, content)

# Tokenize into bigrams
# Tokenize bigrams and preserve sender info
bigrams_raw <- comm_bigrams %>%
  unnest_tokens(bigram, content, token = "ngrams", n = 2, drop = FALSE)

# Optional: Remove bigrams with stopwords
stop_words <- tidytext::stop_words
bigrams_clean <- bigrams_raw %>%
  separate(bigram, into = c("word1", "word2"), sep = " ", remove = FALSE) %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(bigram, sender_name, sort = TRUE)

# Match bigrams only from valid network nodes
if (exists("g_pv_static")) {
  valid_senders <- as_tibble(g_pv_static)$name
  
  bigrams_final <- bigrams_clean %>%
    filter(sender_name %in% valid_senders) %>%
    left_join(as_tibble(g_pv_static) %>% select(name, community), 
              by = c("sender_name" = "name")) %>%
    count(bigram, community, wt = n, sort = TRUE)
  
  assign("bigrams", bigrams_final, envir = .GlobalEnv)
} else {
  print("⚠️ g_pv_static not found. Cannot assign communities to bigrams.")
}

# ================================
# Define UI for Vast Challenge
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
  
  # Updated server Function Call for question2_server
  question2_server(
    id = "question2",
    comm_df = question1_data$comm_df  
  )
  
  
  question3_server("question3", name_mapping_df = tibble())
  
  question4_server(
    "question4",
    nadia_ego_plot = nadia_ego_plot,
    nadia_sent_plot = nadia_sent_plot,
    nadia_received_plot = nadia_received_plot,
    final_findings_text = final_findings_text,
    comm_df = reactive({ comm_df_static }),
    nodes_raw = mc3_nodes_raw,
    edges_raw = mc3_edges_raw
  )
  

  
}



# ================================
# Run App
# ================================
shinyApp(ui = ui, server = server)
