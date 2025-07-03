# data_prep_module_server.R
# data_prep_module_server.R
dataPrepServer <- function(id, nodes_df, edges_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mc3_nodes_cleaned <- reactive({
      nodes_df %>%
        mutate(id = as.character(id)) %>%
        filter(!is.na(id)) %>%
        distinct(id, .keep_all = TRUE) %>%
        select(-thing_collected, -time, -date, -friendship_type)
    })
    
    mc3_edges_cleaned <- reactive({
      edges_df %>%
        rename(from_id = source, to_id = target) %>%
        mutate(across(c(from_id, to_id), as.character)) %>%
        separate(to_id, into = c("to_id_supertype", "to_id_sub_type", "to_id_num"),
                 sep = "_", remove = FALSE, fill = "right", extra = "merge") %>%
        filter(from_id %in% mc3_nodes_cleaned()$id,
               to_id %in% mc3_nodes_cleaned()$id) %>%
        filter(!is.na(from_id), !is.na(to_id))
    })
    
    mc3_edges_indexed <- reactive({
      lookup <- mc3_nodes_cleaned() %>% mutate(.row_id = row_number()) %>% select(id, .row_id)
      
      mc3_edges_cleaned() %>%
        left_join(lookup, by = c("from_id" = "id")) %>% rename(from = .row_id) %>%
        left_join(lookup, by = c("to_id" = "id")) %>% rename(to = .row_id) %>%
        filter(!is.na(from) & !is.na(to)) %>%
        select(from, to, id, is_inferred, type,
               from_id, to_id, to_id_supertype, to_id_sub_type, to_id_num)
    })
    
    mc3_nodes_final <- reactive({
      used_idx <- sort(unique(c(mc3_edges_indexed()$from, mc3_edges_indexed()$to)))
      mc3_nodes_cleaned() %>% slice(used_idx) %>% mutate(new_index = row_number())
    })
    
    # 1.1 Node summary
    output$node_clean_summary <- renderPrint({
      summarise_all(mc3_nodes_cleaned(), n_distinct)
    })
    
    # 1.2 Edge summary
    output$edge_clean_summary <- renderPrint({
      summarise_all(mc3_edges_cleaned(), n_distinct)
    })
    
    # 1.3 Communication extraction summary
    output$comm_extract_summary <- renderPrint({
      df <- mc3_edges_cleaned() %>%
        filter(type == "sent") %>%
        left_join(mc3_nodes_cleaned() %>% select(id, content, timestamp), 
                  by = c("to_id" = "id")) %>%
        rename(event_id = to_id, event_content = content, event_timestamp = timestamp) %>%
        left_join(mc3_edges_cleaned() %>%
                    filter(type == "received") %>%
                    select(event_id_match = from_id, recipient_id = to_id),
                  by = c("event_id" = "event_id_match")) %>%
        left_join(mc3_nodes_cleaned() %>% select(id, name, sub_type),
                  by = c("from_id" = "id")) %>%
        rename(sender_id_actual = from_id, sender_name = name, sender_sub_type = sub_type) %>%
        left_join(mc3_nodes_cleaned() %>% select(id, name, sub_type),
                  by = c("recipient_id" = "id")) %>%
        rename(recipient_name = name, recipient_sub_type = sub_type) %>%
        select(sender_name, recipient_name, event_content, event_timestamp)
      
      summary <- list(
        total_comm_events = nrow(df),
        unique_senders = n_distinct(df$sender_name),
        unique_recipients = n_distinct(df$recipient_name),
        example_rows = head(df, 5)
      )
      return(summary)
    })
    
    # Expose for downstream modules
    return(list(
      nodes_cleaned = mc3_nodes_cleaned,
      edges_cleaned = mc3_edges_cleaned,
      edges_indexed = mc3_edges_indexed,
      nodes_final = mc3_nodes_final
    ))
  })
}
