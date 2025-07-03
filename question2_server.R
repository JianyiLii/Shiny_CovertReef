question2_server <- function(id, mc3_nodes_raw, mc3_edges_raw, comm_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # =============================
    # Community Detection Graph
    # =============================
    comm_graph <- reactive({
      nodes_clean <- mc3_nodes_raw %>%
        distinct(id, .keep_all = TRUE) %>%
        mutate(id = as.character(id)) %>%
        mutate(unique_name = paste0(id, "_", row_number()))
      
      edges_clean <- comm_df() %>%
        distinct(sender_id, recipient_id) %>%
        rename(from = sender_id, to = recipient_id)
      
      # Join to assign unique vertex names
      vertices <- nodes_clean %>%
        select(id, name = unique_name, label = name, sub_type, supertype)
      
      g <- igraph::graph_from_data_frame(
        d = edges_clean,
        vertices = vertices,
        directed = TRUE
      )
      
      g <- igraph::simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
      
      g <- set_vertex_attr(g, "label", value = V(g)$label)
      g <- set_vertex_attr(g, "sub_type", value = V(g)$sub_type)
      g <- set_vertex_attr(g, "supertype", value = V(g)$supertype)
      
      g
    })
    
    # =============================
    # Community Network Plot
    # =============================
    output$community_network_plot <- renderPlot({
      g <- comm_graph()
      g_tbl <- tidygraph::as_tbl_graph(g) %>%
        mutate(community = as.factor(igraph::membership(igraph::cluster_louvain(g))))
      
      ggraph(g_tbl, layout = 'fr') +
        geom_edge_link(alpha = 0.1, color = "gray50") +
        geom_node_point(aes(color = community), size = 3) +
        geom_node_text(aes(label = label), repel = TRUE, size = 2.5)+
      theme_void() +
        labs(title = "Louvain Community Network") +
        theme(legend.position = "right")
    })
    
    # =============================
    # Timeline of People and Vessels
    # =============================
    output$timeline_people_and_vessels <- renderPlot({
      comm_df() %>%
        filter(!is.na(timestamp)) %>%
        mutate(day = lubridate::day(timestamp)) %>%
        count(day, sender_name, recipient_name) %>%
        ggplot(aes(x = day, y = n)) +
        geom_col(fill = "steelblue") +
        labs(title = "Timeline of People and Vessels", x = "Day", y = "Number of Messages") +
        theme_minimal()
    })
    
    # =============================
    # Placeholder for Bigram Plot
    # =============================
    output$bigram_network_plot <- renderPlot({
      ggplot(mtcars, aes(x = mpg, y = wt)) +
        geom_point() +
        labs(title = "Placeholder Bigram Network Plot")
    })
    
    # =============================
    # Placeholder Community Summary Table
    # =============================
    output$community_summary_table <- reactable::renderReactable({
      reactable::reactable(data.frame(Community = 1:3, Members = c(5, 10, 8)))
    })
    
    # =============================
    # Placeholder Text for Chord Diagram
    # =============================
    output$chord_diagram_text <- renderText({
      "This is where the interactive chord diagram and findings summary would go."
    })
    
    # =============================
    # Placeholder Community Flow Plot
    # =============================
    output$community_flow_plot <- renderPlot({
      ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
        geom_boxplot() +
        labs(title = "Placeholder Community Flow Plot")
    })
    
    # =============================
    # Placeholder Community Timeline Plot
    # =============================
    output$community_timeline_plot <- renderPlot({
      ggplot(mtcars, aes(x = wt, y = mpg)) +
        geom_point() +
        labs(title = "Placeholder Community Timeline Plot")
    })
    
  })
}
