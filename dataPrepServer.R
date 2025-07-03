dataPrepServer <- function(id, mc3_nodes_raw, mc3_edges_raw) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ✅ comm_df is reactive and defined here
    comm_df <- reactive({
      sent_edges <- mc3_edges_raw %>%
        filter(type == "sent")
      
      enriched <- sent_edges %>%
        left_join(mc3_nodes_raw, by = c("source" = "id")) %>%
        rename(sender_name = name, sender_type = type.y, sender_sub_type = sub_type) %>%
        select(-type.y)
      
      enriched <- enriched %>%
        left_join(mc3_nodes_raw, by = c("target" = "id")) %>%
        rename(recipient_name = name, recipient_type = type, recipient_sub_type = sub_type)
      
      enriched %>%
        select(timestamp, sender_name, sender_type, sender_sub_type,
               recipient_name, recipient_type, recipient_sub_type,
               content)
    })
    
    # ✅ Diagnostic: placed OUTSIDE the reactive expression
    observe({
      cat("🧪 Checking if comm_df is reactive...\n")
      print(is.reactive(comm_df))  # should print TRUE
    })
    
    return(list(
      comm_df = comm_df
    ))
  })
}
