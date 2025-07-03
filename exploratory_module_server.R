exploratoryServer <- function(id, nodes_df, edges_df, nodes_raw, edges_raw) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Ensure supertype
    nodes <- reactive({ nodes_df %>% mutate(supertype = type) })
    edges <- reactive({ edges_df })
    
    # 1.2: ExpCatViz dynamic rendering
    output$all_node_plots <- renderUI({
      viz_all <- SmartEDA::ExpCatViz(data = nodes(), col = "navyblue")
      lapply(seq_along(viz_all), function(i) {
        plotname <- paste0(ns("plot_"), i)
        column(width = 6, plotOutput(plotname, height = "300px"))
      }) |> tagList()
    })
    
    for (i in 1:20) {
      local({
        my_i <- i
        output[[paste0("plot_", my_i)]] <- renderPlot({
          plots <- SmartEDA::ExpCatViz(data = nodes(), col = "navyblue")
          if (my_i <= length(plots)) plots[[my_i]]
        })
      })
    }
    
    # 1.2 subtype plots
    output$plot_event_subtypes <- renderPlot({
      nodes() %>%
        filter(supertype == "Event") %>%
        count(sub_type) %>%
        arrange(n) %>%
        mutate(sub_type = factor(sub_type, levels = sub_type)) %>%
        ggplot(aes(x = sub_type, y = n)) +
        geom_col(fill = "navy") + coord_flip() +
        labs(x = "Event Subtype", y = "Count") +
        theme_minimal()
    })
    
    output$plot_entity_subtypes <- renderPlot({
      nodes() %>%
        filter(supertype == "Entity") %>%
        count(sub_type) %>%
        arrange(n) %>%
        mutate(sub_type = factor(sub_type, levels = sub_type)) %>%
        ggplot(aes(x = sub_type, y = n)) +
        geom_col(fill = "navy") + coord_flip() +
        labs(x = "Entity Subtype", y = "Count") +
        theme_minimal()
    })
    
    output$plot_relationship_subtypes <- renderPlot({
      nodes() %>%
        filter(supertype == "Relationship") %>%
        count(sub_type) %>%
        arrange(n) %>%
        mutate(sub_type = factor(sub_type, levels = sub_type)) %>%
        ggplot(aes(x = sub_type, y = n)) +
        geom_col(fill = "navy") + coord_flip() +
        labs(x = "Relationship Subtype", y = "Count") +
        theme_minimal()
    })
    
    # 1.5 edge EDA
    output$plot_edge_exp_catviz <- renderPlot({
      plots <- SmartEDA::ExpCatViz(data = edges(), col = "navyblue")
      plots[[1]]
    })
    
    # 1.7 Knowledge Graph from RAW nodes
    output$knowledge_graph <- renderVisNetwork({
      raw_nodes <- nodes_raw
      raw_edges <- edges_raw
      
      legend_colors <- c(
        "Person" = "#88CCEE", "Vessel" = "#D55E00",
        "Organization" = "#117733", "Location" = "#AA4499",
        "Group" = "#CC79A7", "Event" = "#DDCC77", "Relationship" = "#AF8DC3"
      )
      legend_shapes <- c(
        "Person" = "dot", "Vessel" = "triangle", "Organization" = "square",
        "Location" = "diamond", "Group" = "circle plus", "Event" = "star", "Relationship" = "square x"
      )
      
      STYLES <- list(node_label_dark = "black", font_family = "Roboto Condensed")
      
      # Graph nodes
      gnodes <- raw_nodes %>%
        mutate(
          label = if_else(is.na(name), id, name),
          tooltip_extra = dplyr::case_when(
            type == "Event" & sub_type == "Communication" ~ content,
            type == "Event" & sub_type == "Monitoring" ~ findings,
            type == "Event" & sub_type == "VesselMovement" ~ destination,
            type == "Event" & sub_type == "Assessment" ~ results,
            type == "Relationship" & sub_type == "Coordinates" ~ coordination_type,
            type == "Relationship" & sub_type == "Operates" ~ operational_role,
            type == "Relationship" & sub_type == "Jurisdiction" ~ jurisdiction_type,
            TRUE ~ NA_character_
          ),
          title = paste0(
            "<b>", label, "</b><br>",
            "Type: ", type, "<br>",
            "Sub-type: ", sub_type, "<br>",
            ifelse(!is.na(tooltip_extra), paste0("<b>Details:</b> ", tooltip_extra), "")
          ),
          group = if_else(sub_type %in% names(legend_colors), sub_type, type)
        ) %>%
        select(id, label, group, title) %>%
        distinct()
      
      # Graph edges
      gedges <- raw_edges %>%
        filter(source %in% gnodes$id, target %in% gnodes$id) %>%
        rename(from = source, to = target)
      
      # Build visNetwork
      net <- visNetwork(gnodes, gedges, width = "100%", height = "600px") %>%
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1.5))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(font = list(size = 14, color = STYLES$node_label_dark, face = STYLES$font_family))
      
      for (g in names(legend_colors)) {
        net <- net %>% visGroups(
          groupname = g,
          color = legend_colors[[g]],
          shape = legend_shapes[[g]]
        )
      }
      
      # Legend
      used_groups <- unique(gnodes$group)
      legend_df <- tibble::tibble(
        label = used_groups,
        shape = legend_shapes[used_groups],
        color = legend_colors[used_groups]
      )
      
      net %>% visLegend(
        addNodes = legend_df,
        ncol = 2,
        position = "left",
        main = "Entity (Sub)Types",
        useGroups = FALSE
      )
    })
  })
}
