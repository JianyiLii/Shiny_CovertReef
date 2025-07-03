question4_server <- function(
    id,
    mc3_nodes_raw,
    mc3_edges_raw,
    comm_df,
    nadia_2hop_timeline_plot,
    nadia_network_findings_text,
    keyword_timeline_plot,
    two_word_timeline_plot,
    keyword_findings_text,
    suspicious_table_1,
    suspicious_table_2,
    suspicious_table_3,
    final_findings_text
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
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
      "Group" = "circle plus",
      "Event" = "star",
      "Relationship" = "square x",
      "Nadia Conti" = "star"
    )
    
    nadia_id <- reactive({
      mc3_nodes_raw %>% filter(name == "Nadia Conti") %>% pull(id)
    })
    
    output$nadia_ego_sent <- visNetwork::renderVisNetwork({
      sent_edges <- mc3_edges_raw %>% filter(from_id == nadia_id())
      sent_nodes <- mc3_nodes_raw %>%
        filter(id %in% unique(c(sent_edges$from_id, sent_edges$to_id))) %>%
        mutate(group = ifelse(name == "Nadia Conti", "Nadia Conti", type))
      
      visNetwork(sent_nodes, sent_edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visLegend(addNodes = tibble(label = names(node_legend_shapes_plot),
                                    shape = unname(node_legend_shapes_plot),
                                    color = unname(node_legend_colors_plot)),
                  ncol = 2, position = "left", useGroups = FALSE)
    })
    
    output$nadia_ego_received <- visNetwork::renderVisNetwork({
      recv_edges <- mc3_edges_raw %>% filter(to_id == nadia_id())
      recv_nodes <- mc3_nodes_raw %>%
        filter(id %in% unique(c(recv_edges$from_id, recv_edges$to_id))) %>%
        mutate(group = ifelse(name == "Nadia Conti", "Nadia Conti", type))
      
      visNetwork(recv_nodes, recv_edges) %>%
        visEdges(arrows = list(to = list(enabled = TRUE))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visLegend(addNodes = tibble(label = names(node_legend_shapes_plot),
                                    shape = unname(node_legend_shapes_plot),
                                    color = unname(node_legend_colors_plot)),
                  ncol = 2, position = "left", useGroups = FALSE)
    })
    
    output$nadia_2hop <- visNetwork::renderVisNetwork({
      one_hop <- mc3_edges_raw %>% filter(from_id == nadia_id() | to_id == nadia_id())
      two_hop <- mc3_edges_raw %>% filter(from_id %in% one_hop$to_id | to_id %in% one_hop$to_id)
      edges_2hop <- bind_rows(one_hop, two_hop)
      node_ids_2hop <- unique(c(edges_2hop$from_id, edges_2hop$to_id))
      nodes_2hop <- mc3_nodes_raw %>%
        filter(id %in% node_ids_2hop) %>%
        mutate(group = ifelse(name == "Nadia Conti", "Nadia Conti", type))
      
      visNetwork(nodes_2hop, edges_2hop) %>%
        visEdges(arrows = list(to = list(enabled = TRUE))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visLegend(addNodes = tibble(label = names(node_legend_shapes_plot),
                                    shape = unname(node_legend_shapes_plot),
                                    color = unname(node_legend_colors_plot)),
                  ncol = 2, position = "left", useGroups = FALSE)
    })
    
    output$nadia_2hop_timeline <- plotly::renderPlotly({ nadia_2hop_timeline_plot })
    output$nadia_network_findings <- renderText({ nadia_network_findings_text })
    output$keyword_timeline <- plotly::renderPlotly({ keyword_timeline_plot })
    output$two_word_timeline <- plotly::renderPlotly({ two_word_timeline_plot })
    output$keyword_findings <- renderText({ keyword_findings_text })
    
    output$suspicious_table_1 <- reactable::renderReactable({ suspicious_table_1 })
    output$suspicious_table_2 <- reactable::renderReactable({ suspicious_table_2 })
    output$suspicious_table_3 <- reactable::renderReactable({ suspicious_table_3 })
    
    output$final_findings <- renderText({ final_findings_text })
  })
}