question4_server <- function(
    id,
    nadia_ego_plot,
    nadia_sent_plot,
    nadia_received_plot,
    final_findings_text,
    comm_df,
    nodes_raw,
    edges_raw
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      req(comm_df())
      df <- comm_df()
      all_names <- sort(unique(c(df$sender_name, df$recipient_name)))
      updatePickerInput(session, "suspicious_selected_name", choices = all_names)
    })
    
    # ======================
    # 4a. Ego Network Plots
    # ======================
    output$nadia_ego_plot <- renderPlot({
      req(nadia_ego_plot)
      nadia_ego_plot
    })
    
    output$nadia_sent_plot <- renderVisNetwork({
      req(nadia_sent_plot)
      nadia_sent_plot
    })
    
    output$nadia_received_plot <- renderVisNetwork({
      req(nadia_received_plot)
      nadia_received_plot
    })
    
    output$nadia_3hop_plot <- renderVisNetwork({
      req(nodes_raw, edges_raw)
      
      event_subtypes <- c("Communication", "Monitoring", "VesselMovement", "Assessment", "Collaborate", "Endorsement", "TourActivity", "TransponderPing", "Harbor Report", "Fishing", "Criticize")
      relationship_subtypes <- c("Coordinates", "AccessPermission", "Operates", "Colleagues", "Suspicious", "Reports", "Jurisdiction", "Unfriendly", "Friends")
      
      node_legend_colors_plot <- c(
        "Person" = "#88CCEE", "Vessel" = "#D55E00", "Organization" = "#117733",
        "Location" = "#AA4499", "Group" = "#CC79A7", "Event" = "#DDCC77", "Relationship" = "#AF8DC3"
      )
      node_legend_shapes_plot <- c(
        "Person" = "dot", "Vessel" = "triangle", "Organization" = "square",
        "Location" = "diamond", "Group" = "circle", "Event" = "star", "Relationship" = "square"
      )
      STYLES <- list(node_label_dark = "black", font_family = "Roboto Condensed")
      
      nodes_dedup <- nodes_raw %>% distinct(id, .keep_all = TRUE)
      g <- igraph::graph_from_data_frame(edges_raw, directed = TRUE, vertices = nodes_dedup)
      nadia_id <- nodes_dedup$id[nodes_dedup$name == "Nadia Conti"]
      if (length(nadia_id) == 0) return(NULL)
      
      ego_graph <- igraph::make_ego_graph(g, order = 3, nodes = nadia_id, mode = "all")[[1]]
      node_ids <- igraph::V(ego_graph)$name
      edge_df <- igraph::as_data_frame(ego_graph, what = "edges")
      
      ego_nodes <- nodes_raw %>%
        filter(id %in% node_ids) %>%
        mutate(
          label = ifelse(is.na(name), id, name),
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
          group = case_when(
            sub_type %in% names(node_legend_colors_plot) ~ sub_type,
            type %in% names(node_legend_colors_plot) ~ type,
            TRUE ~ "Other"
          )
        ) %>%
        select(id, label, group, title) %>%
        distinct()
      
      edges <- edge_df %>% filter(from %in% ego_nodes$id, to %in% ego_nodes$id)
      
      net <- visNetwork(ego_nodes, edges, width = "100%", height = "600px") %>%
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1.5))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(font = list(size = 14, color = STYLES$node_label_dark, face = STYLES$font_family))
      
      used_groups <- intersect(unique(ego_nodes$group), names(node_legend_colors_plot))
      for (g in used_groups) {
        net <- net %>% visGroups(
          groupname = g,
          color = node_legend_colors_plot[[g]],
          shape = node_legend_shapes_plot[[g]]
        )
      }
      
      legend_df <- tibble::tibble(
        label = used_groups,
        shape = node_legend_shapes_plot[used_groups],
        color = node_legend_colors_plot[used_groups]
      ) %>%
        distinct(label, .keep_all = TRUE)
      
      net %>% visLegend(
        addNodes = legend_df,
        ncol = 2,
        position = "left",
        main = "Entity (Sub)Types",
        useGroups = FALSE
      )
      

    })
    output$nadia_summary_text <- renderText({
      req(comm_df())
      
      df <- comm_df()
      
      sent_count <- df %>%
        filter(sender_name == "Nadia Conti") %>%
        nrow()
      
      received_count <- df %>%
        filter(recipient_name == "Nadia Conti") %>%
        nrow()
      
      paste0(
        "Nadia Conti sent ", sent_count, " messages\n",
        "Nadia Conti received ", received_count, " messages"
      )
    })
    output$nadia_bar_plot <- renderPlot({
      req(comm_df())
      
      df <- comm_df()
      
      bar_data <- tibble(
        Direction = c("Sent", "Received"),
        Count = c(
          nrow(df %>% filter(sender_name == "Nadia Conti")),
          nrow(df %>% filter(recipient_name == "Nadia Conti"))
        )
      )
      
      ggplot(bar_data, aes(x = Direction, y = Count, fill = Direction)) +
        geom_bar(stat = "identity", width = 0.5) +
        labs(title = NULL, y = "Messages", x = "") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$nadia_breakdown_table <- renderTable({
      req(comm_df())
      
      df <- comm_df()
      
      df %>%
        filter(sender_name == "Nadia Conti") %>%
        count(recipient_sub_type, name = "Messages_Sent") %>%
        arrange(desc(Messages_Sent))
    })
    
    

    
    # ======================
    # 4b. Timeline & Keywords
    # ======================
    
    # ======================
    # 4b. suspicious_timeline_plot
    # ======================
    output$suspicious_timeline_plot <- renderPlotly({
      req(comm_df(), input$susp_mode, input$susp_role)
      
      df <- comm_df() %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          suspicious_flag = if_else(str_detect(tolower(content), "suspicious"), "Yes", "No"),
          wrapped_content = str_wrap(content, 60),
          tooltip_text = paste0(
            "<b>Date:</b> ", comm_date, "<br>",
            "<b>Time:</b> ", format(timestamp, "%H:%M:%S"), "<br>",
            "<b>From:</b> ", sender_name, "<br>",
            "<b>To:</b> ", recipient_name, "<br><br>",
            "<b>Content:</b><br>", wrapped_content
          )
        )
      
      # Apply entity selection filter by sender/recipient/either
      if (!is.null(input$suspicious_selected_name) && length(input$suspicious_selected_name) > 0) {
        df <- df %>%
          filter(
            (input$susp_role == "either" & (sender_name %in% input$suspicious_selected_name | recipient_name %in% input$suspicious_selected_name)) |
              (input$susp_role == "sender" & sender_name %in% input$suspicious_selected_name) |
              (input$susp_role == "recipient" & recipient_name %in% input$suspicious_selected_name)
          )
      }
      
      # Apply suspicious message filter
      df <- switch(input$susp_mode,
                   "all" = df,
                   "only" = df %>% filter(suspicious_flag == "Yes"),
                   "non" = df %>% filter(suspicious_flag == "No"))
      
      if (nrow(df) == 0) {
        return(ggplotly(
          ggplot() +
            theme_void() +
            ggtitle("No matching communications found.")
        ))
      }
      
      # Sort by sender_sub_type to ensure consistent shapes and colors
      df_sorted <- df %>%
        arrange(sender_sub_type)
      
      # Plot timeline, now sorted by sender_sub_type
      ggplotly(
        ggplot(df_sorted, aes(x = comm_date, y = comm_time_of_day)) +
          geom_point(aes(color = sender_name, shape = sender_sub_type, text = tooltip_text),
                     size = 2, alpha = 0.7) +
          scale_shape_manual(values = c(
            "Person" = 16,  # Circle for Person
            "Vessel" = 17,  # Triangle for Vessel
            "Organization" = 15, # Square for Organization
            "Location" = 18,  # Diamond for Location
            "Group" = 8      # Circle for Group
          )) +
          facet_wrap(~sender_sub_type, ncol = 1, scales = "free_y") +  # Facet by recipient_sub_type
          scale_y_time(
            limits = hms::as_hms(c("08:00:00", "14:00:00")),
            breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
            labels = sprintf("%02d:00", 8:14)
          ) +
          scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
          labs(title = "🕒 Communication Timeline",
               x = "Date", y = "Time of Day", color = "Sender") +
          theme_minimal(base_size = 9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)),
        tooltip = "text"
      )
    })
    
    
    
    
    # ======================
    # 4b.suspicious_entity_table
    # ======================
    output$suspicious_entity_table <- DT::renderDataTable({
      req(comm_df(), input$susp_mode)
      
      df <- comm_df() %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          Date = as.Date(timestamp),
          Time = format(timestamp, "%H:%M:%S"),
          suspicious_flag = if_else(str_detect(tolower(content), "suspicious"), "Yes", "No")
        ) %>%
        select(event_id, Date, Time, sender_name, recipient_name, content)
      
      df <- switch(input$susp_mode,
                   "all" = df,
                   "only" = df %>% filter(str_detect(tolower(content), "suspicious")),
                   "non" = df %>% filter(!str_detect(tolower(content), "suspicious")))
      
      DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    
    keyword_filtered_df <- reactive({
      req(comm_df(), input$keyword_selected)
      keyword <- input$keyword_selected
      
      comm_df() %>%
        filter(str_detect(tolower(content), tolower(keyword))) %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          wrapped_content = str_wrap(content, 60),
          tooltip_text = paste0(
            "<b>Date:</b> ", comm_date, "<br>",
            "<b>Time:</b> ", format(timestamp, "%H:%M:%S"), "<br>",
            "<b>From:</b> ", sender_name, "<br>",
            "<b>To:</b> ", recipient_name, "<br><br>",
            "<b>Content:</b><br>", wrapped_content
          )
        )
    })
    
    # ======================
    # 4b.keyword_timeline_plot
    # ======================
    output$keyword_timeline_plot <- renderPlotly({
      df <- keyword_filtered_df()
      
      if (nrow(df) == 0) {
        return(ggplotly(
          ggplot() +
            theme_void() +
            ggtitle("No matching keyword communications found")
        ))
      }
      
      ggplotly(
        ggplot(df, aes(x = comm_date, y = comm_time_of_day)) +
          geom_point(aes(color = sender_name, shape = sender_sub_type, text = tooltip_text),
                     size = 2, alpha = 0.7) +
          scale_shape_manual(values = c(
            "Person" = 16, "Vessel" = 17, "Organization" = 15,
            "Location" = 18, "Group" = 8
          )) +
          facet_wrap(~sender_sub_type, ncol = 1) +
          scale_y_time(
            limits = hms::as_hms(c("08:00:00", "14:00:00")),
            breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
            labels = sprintf("%02d:00", 8:14)
          ) +
          scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
          labs(title = "📅 Timeline of Matching Communications",
               x = "Date", y = "Time of Day", color = "Sender") +
          theme_minimal(base_size = 9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)),
        tooltip = "text"
      )
    })
    
    output$debug_keyword_plot_data <- renderPrint({
      df <- keyword_filtered_df()
      cat("Selected keyword:", input$keyword_selected, "\n")
      cat("Rows found:", nrow(df), "\n")
      print(head(df))
    })
    
    
    output$keyword_entity_table <- reactable::renderReactable({
      req(comm_df())
      
      if (is.null(input$keyword_selected) || input$keyword_selected == "") {
        return(reactable::reactable(
          tibble(Message = "No keyword selected."),
          bordered = TRUE,
          highlight = TRUE
        ))
      }
      
      keyword_df <- comm_df() %>%
        filter(str_detect(tolower(content), tolower(input$keyword_selected))) %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          Date = as.Date(timestamp),
          Time = format(timestamp, "%H:%M:%S")
        ) %>%
        select(
          event_id,
          Date,
          Time,
          sender_name,
          recipient_name,
          content
        ) %>%
        arrange(desc(Date), desc(Time))
      
      if (nrow(keyword_df) == 0) {
        return(reactable::reactable(
          tibble(Message = "No matching messages found for selected keyword."),
          bordered = TRUE,
          highlight = TRUE,
          defaultPageSize = 1
        ))
      }
      
      reactable::reactable(
        keyword_df,
        columns = list(
          event_id = colDef(name = "Event ID"),
          Date = colDef(name = "Date"),
          Time = colDef(name = "Time"),
          sender_name = colDef(name = "Sender"),
          recipient_name = colDef(name = "Recipient"),
          content = colDef(name = "Message", minWidth = 300)
        ),
        searchable = TRUE,
        pagination = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultPageSize = 10
      )
    })
    
    
    
    
    output$debug_keyword_table <- renderPrint({
      input$keyword_selected
      head(comm_df())
    })
    
    
    output$final_findings_text <- renderUI({
      req(final_findings_text)
      HTML(final_findings_text)
    })
    
    observeEvent(input$susp_select_people, {
      df <- comm_df()
      people <- unique(c(
        df$sender_name[df$sender_sub_type == "Person"],
        df$recipient_name[df$recipient_sub_type == "Person"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(people))
    })
    
    observeEvent(input$susp_select_vessels, {
      df <- comm_df()
      vessels <- unique(c(
        df$sender_name[df$sender_sub_type == "Vessel"],
        df$recipient_name[df$recipient_sub_type == "Vessel"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(vessels))
    })
    
    observeEvent(input$susp_select_orgs, {
      df <- comm_df()
      orgs <- unique(c(
        df$sender_name[df$sender_sub_type == "Organization"],
        df$recipient_name[df$recipient_sub_type == "Organization"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(orgs))
    })
    
    
    observeEvent(input$susp_select_groups, {
      df <- comm_df()
      groups <- unique(c(
        df$sender_name[df$sender_sub_type == "Group"],
        df$recipient_name[df$recipient_sub_type == "Group"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(groups))
    })
    
    observeEvent(input$susp_select_locations, {
      df <- comm_df()
      locations <- unique(c(
        df$sender_name[df$sender_sub_type == "Location"],
        df$recipient_name[df$recipient_sub_type == "Location"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(locations))
    })
    
    observeEvent(input$susp_reset, {
      updatePickerInput(session, "suspicious_selected_name", selected = character(0))
    })
    
  
    # ======================
    # 4a. Ego Network Plots
    # ======================
    output$nadia_ego_plot <- renderPlot({
      req(nadia_ego_plot)
      nadia_ego_plot
    })
    
    output$nadia_sent_plot <- renderVisNetwork({
      req(nadia_sent_plot)
      nadia_sent_plot
    })
    
    output$nadia_received_plot <- renderVisNetwork({
      req(nadia_received_plot)
      nadia_received_plot
    })
    
    output$nadia_3hop_plot <- renderVisNetwork({
      req(nodes_raw, edges_raw)
      
      event_subtypes <- c("Communication", "Monitoring", "VesselMovement", "Assessment", "Collaborate", "Endorsement", "TourActivity", "TransponderPing", "Harbor Report", "Fishing", "Criticize")
      relationship_subtypes <- c("Coordinates", "AccessPermission", "Operates", "Colleagues", "Suspicious", "Reports", "Jurisdiction", "Unfriendly", "Friends")
      
      node_legend_colors_plot <- c(
        "Person" = "#88CCEE", "Vessel" = "#D55E00", "Organization" = "#117733",
        "Location" = "#AA4499", "Group" = "#CC79A7", "Event" = "#DDCC77", "Relationship" = "#AF8DC3"
      )
      node_legend_shapes_plot <- c(
        "Person" = "dot", "Vessel" = "triangle", "Organization" = "square",
        "Location" = "diamond", "Group" = "circle", "Event" = "star", "Relationship" = "square"
      )
      STYLES <- list(node_label_dark = "black", font_family = "Roboto Condensed")
      
      nodes_dedup <- nodes_raw %>% distinct(id, .keep_all = TRUE)
      g <- igraph::graph_from_data_frame(edges_raw, directed = TRUE, vertices = nodes_dedup)
      nadia_id <- nodes_dedup$id[nodes_dedup$name == "Nadia Conti"]
      if (length(nadia_id) == 0) return(NULL)
      
      ego_graph <- igraph::make_ego_graph(g, order = 3, nodes = nadia_id, mode = "all")[[1]]
      node_ids <- igraph::V(ego_graph)$name
      edge_df <- igraph::as_data_frame(ego_graph, what = "edges")
      
      ego_nodes <- nodes_raw %>%
        filter(id %in% node_ids) %>%
        mutate(
          label = ifelse(is.na(name), id, name),
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
          group = case_when(
            sub_type %in% names(node_legend_colors_plot) ~ sub_type,
            type %in% names(node_legend_colors_plot) ~ type,
            TRUE ~ "Other"
          )
        ) %>%
        select(id, label, group, title) %>%
        distinct()
      
      edges <- edge_df %>% filter(from %in% ego_nodes$id, to %in% ego_nodes$id)
      
      net <- visNetwork(ego_nodes, edges, width = "100%", height = "600px") %>%
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1.5))) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visIgraphLayout(layout = "layout_with_fr") %>%
        visNodes(font = list(size = 14, color = STYLES$node_label_dark, face = STYLES$font_family))
      
      used_groups <- intersect(unique(ego_nodes$group), names(node_legend_colors_plot))
      for (g in used_groups) {
        net <- net %>% visGroups(
          groupname = g,
          color = node_legend_colors_plot[[g]],
          shape = node_legend_shapes_plot[[g]]
        )
      }
      
      legend_df <- tibble::tibble(
        label = used_groups,
        shape = node_legend_shapes_plot[used_groups],
        color = node_legend_colors_plot[used_groups]
      ) %>%
        distinct(label, .keep_all = TRUE)
      
      net %>% visLegend(
        addNodes = legend_df,
        ncol = 2,
        position = "left",
        main = "Entity (Sub)Types",
        useGroups = FALSE
      )
      

    })
    #Nadia Summary Table Info Code.
    output$nadia_summary_text <- renderUI({
      req(comm_df())
      df <- comm_df()
      
      sent_msgs <- df %>% filter(sender_name == "Nadia Conti")
      received_msgs <- df %>% filter(recipient_name == "Nadia Conti")
      
      total_sent <- nrow(sent_msgs)
      total_received <- nrow(received_msgs)
      
      unique_sent_to <- length(unique(sent_msgs$recipient_name))
      unique_received_from <- length(unique(received_msgs$sender_name))
      
      top_sent <- sent_msgs %>%
        count(recipient_name, sort = TRUE) %>%
        slice_head(n = 3)
      
      top_received <- received_msgs %>%
        count(sender_name, sort = TRUE) %>%
        slice_head(n = 3)
      
      HTML(paste0(
        "<p><b>📨 Sent by Nadia Conti:</b> ", total_sent, " messages<br/>",
        "&nbsp;&nbsp;&nbsp;&nbsp;<b>👥 To:</b> ", unique_sent_to, " unique recipients</p>",
        
        "<ul>",
        paste0("<li>", top_sent$recipient_name, " (", top_sent$n, " messages)</li>", collapse = ""),
        "</ul>",
        
        "<p><b>📥 Received by Nadia Conti:</b> ", total_received, " messages<br/>",
        "&nbsp;&nbsp;&nbsp;&nbsp;<b>👤 From:</b> ", unique_received_from, " unique senders</p>",
        
        "<ul>",
        paste0("<li>", top_received$sender_name, " (", top_received$n, " messages)</li>", collapse = ""),
        "</ul>"
      ))
    })
    

    
    
    output$nadia_bar_plot <- renderPlot({
      req(comm_df())
      
      df <- comm_df()
      
      bar_data <- tibble(
        Direction = c("Sent", "Received"),
        Count = c(
          nrow(df %>% filter(sender_name == "Nadia Conti")),
          nrow(df %>% filter(recipient_name == "Nadia Conti"))
        )
      )
      
      ggplot(bar_data, aes(x = Direction, y = Count, fill = Direction)) +
        geom_bar(stat = "identity", width = 0.5) +
        labs(title = NULL, y = "Messages", x = "") +
        theme_minimal() +
        theme(legend.position = "none")
    })
    
    output$nadia_breakdown_table <- renderTable({
      req(comm_df())
      
      df <- comm_df()
      
      df %>%
        filter(sender_name == "Nadia Conti") %>%
        count(recipient_sub_type, name = "Messages_Sent") %>%
        arrange(desc(Messages_Sent))
    })
    
    

    
    # ======================
    # 4b. Timeline & Keywords
    # ======================
    
    # ======================
    # 4b. suspicious_timeline_plot
    # ======================
    output$suspicious_timeline_plot <- renderPlotly({
      req(comm_df(), input$susp_mode, input$susp_role)
      
      df <- comm_df() %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          suspicious_flag = if_else(str_detect(tolower(content), "suspicious"), "Yes", "No"),
          wrapped_content = str_wrap(content, 60),
          tooltip_text = paste0(
            "<b>Date:</b> ", comm_date, "<br>",
            "<b>Time:</b> ", format(timestamp, "%H:%M:%S"), "<br>",
            "<b>From:</b> ", sender_name, "<br>",
            "<b>To:</b> ", recipient_name, "<br><br>",
            "<b>Content:</b><br>", wrapped_content
          )
        )
      
      # Apply entity selection filter by sender/recipient/either
      if (!is.null(input$suspicious_selected_name) && length(input$suspicious_selected_name) > 0) {
        df <- df %>%
          filter(
            (input$susp_role == "either" & (sender_name %in% input$suspicious_selected_name | recipient_name %in% input$suspicious_selected_name)) |
              (input$susp_role == "sender" & sender_name %in% input$suspicious_selected_name) |
              (input$susp_role == "recipient" & recipient_name %in% input$suspicious_selected_name)
          )
      }
      
      # Apply suspicious message filter
      df <- switch(input$susp_mode,
                   "all" = df,
                   "only" = df %>% filter(suspicious_flag == "Yes"),
                   "non" = df %>% filter(suspicious_flag == "No"))
      
      if (nrow(df) == 0) {
        return(ggplotly(
          ggplot() +
            theme_void() +
            ggtitle("No matching communications found.")
        ))
      }
      
      # Sort by sender_sub_type to ensure consistent shapes and colors
      df_sorted <- df %>%
        arrange(sender_sub_type)
      
      # Plot timeline, now sorted by sender_sub_type
      ggplotly(
        ggplot(df_sorted, aes(x = comm_date, y = comm_time_of_day)) +
          geom_point(aes(color = sender_name, shape = sender_sub_type, text = tooltip_text),
                     size = 2, alpha = 0.7) +
          scale_shape_manual(values = c(
            "Person" = 16,  # Circle for Person
            "Vessel" = 17,  # Triangle for Vessel
            "Organization" = 15, # Square for Organization
            "Location" = 18,  # Diamond for Location
            "Group" = 8      # Circle for Group
          )) +
          facet_wrap(~sender_sub_type, ncol = 1, scales = "free_y") +  # Facet by recipient_sub_type
          scale_y_time(
            limits = hms::as_hms(c("08:00:00", "14:00:00")),
            breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
            labels = sprintf("%02d:00", 8:14)
          ) +
          scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
          labs(title = "🕒 Communication Timeline",
               x = "Date", y = "Time of Day", color = "Sender") +
          theme_minimal(base_size = 9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)),
        tooltip = "text"
      )
    })
    
    
    
    
    # ======================
    # 4b.suspicious_entity_table
    # ======================
    output$suspicious_entity_table <- DT::renderDataTable({
      req(comm_df(), input$susp_mode)
      
      df <- comm_df() %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          Date = as.Date(timestamp),
          Time = format(timestamp, "%H:%M:%S"),
          suspicious_flag = if_else(str_detect(tolower(content), "suspicious"), "Yes", "No")
        ) %>%
        select(event_id, Date, Time, sender_name, recipient_name, content)
      
      df <- switch(input$susp_mode,
                   "all" = df,
                   "only" = df %>% filter(str_detect(tolower(content), "suspicious")),
                   "non" = df %>% filter(!str_detect(tolower(content), "suspicious")))
      
      DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
    })
    
    
    keyword_filtered_df <- reactive({
      req(comm_df(), input$keyword_selected)
      keyword <- input$keyword_selected
      
      comm_df() %>%
        filter(str_detect(tolower(content), tolower(keyword))) %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          wrapped_content = str_wrap(content, 60),
          tooltip_text = paste0(
            "<b>Date:</b> ", comm_date, "<br>",
            "<b>Time:</b> ", format(timestamp, "%H:%M:%S"), "<br>",
            "<b>From:</b> ", sender_name, "<br>",
            "<b>To:</b> ", recipient_name, "<br><br>",
            "<b>Content:</b><br>", wrapped_content
          )
        )
    })
    
    # ======================
    # 4b.keyword_timeline_plot
    # ======================
    output$keyword_timeline_plot <- renderPlotly({
      df <- keyword_filtered_df()
      
      if (nrow(df) == 0) {
        return(ggplotly(
          ggplot() +
            theme_void() +
            ggtitle("No matching keyword communications found")
        ))
      }
      
      ggplotly(
        ggplot(df, aes(x = comm_date, y = comm_time_of_day)) +
          geom_point(aes(color = sender_name, shape = sender_sub_type, text = tooltip_text),
                     size = 2, alpha = 0.7) +
          scale_shape_manual(values = c(
            "Person" = 16, "Vessel" = 17, "Organization" = 15,
            "Location" = 18, "Group" = 8
          )) +
          facet_wrap(~sender_sub_type, ncol = 1) +
          scale_y_time(
            limits = hms::as_hms(c("08:00:00", "14:00:00")),
            breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
            labels = sprintf("%02d:00", 8:14)
          ) +
          scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
          labs(title = "📅 Timeline of Matching Communications",
               x = "Date", y = "Time of Day", color = "Sender") +
          theme_minimal(base_size = 9) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)),
        tooltip = "text"
      )
    })
    
    output$debug_keyword_plot_data <- renderPrint({
      df <- keyword_filtered_df()
      cat("Selected keyword:", input$keyword_selected, "\n")
      cat("Rows found:", nrow(df), "\n")
      print(head(df))
    })
    
    
    output$keyword_entity_table <- reactable::renderReactable({
      req(comm_df())
      
      if (is.null(input$keyword_selected) || input$keyword_selected == "") {
        return(reactable::reactable(
          tibble(Message = "No keyword selected."),
          bordered = TRUE,
          highlight = TRUE
        ))
      }
      
      keyword_df <- comm_df() %>%
        filter(str_detect(tolower(content), tolower(input$keyword_selected))) %>%
        mutate(
          timestamp = suppressWarnings(lubridate::ymd_hms(timestamp)),
          Date = as.Date(timestamp),
          Time = format(timestamp, "%H:%M:%S")
        ) %>%
        select(
          event_id,
          Date,
          Time,
          sender_name,
          recipient_name,
          content
        ) %>%
        arrange(desc(Date), desc(Time))
      
      if (nrow(keyword_df) == 0) {
        return(reactable::reactable(
          tibble(Message = "No matching messages found for selected keyword."),
          bordered = TRUE,
          highlight = TRUE,
          defaultPageSize = 1
        ))
      }
      
      reactable::reactable(
        keyword_df,
        columns = list(
          event_id = colDef(name = "Event ID"),
          Date = colDef(name = "Date"),
          Time = colDef(name = "Time"),
          sender_name = colDef(name = "Sender"),
          recipient_name = colDef(name = "Recipient"),
          content = colDef(name = "Message", minWidth = 300)
        ),
        searchable = TRUE,
        pagination = TRUE,
        highlight = TRUE,
        striped = TRUE,
        defaultPageSize = 10
      )
    })
    
    
    
    
    output$debug_keyword_table <- renderPrint({
      input$keyword_selected
      head(comm_df())
    })
    
    
    output$final_findings_text <- renderUI({
      req(final_findings_text)
      HTML(final_findings_text)
    })
    
    observeEvent(input$susp_select_people, {
      df <- comm_df()
      people <- unique(c(
        df$sender_name[df$sender_sub_type == "Person"],
        df$recipient_name[df$recipient_sub_type == "Person"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(people))
    })
    
    observeEvent(input$susp_select_vessels, {
      df <- comm_df()
      vessels <- unique(c(
        df$sender_name[df$sender_sub_type == "Vessel"],
        df$recipient_name[df$recipient_sub_type == "Vessel"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(vessels))
    })
    
    observeEvent(input$susp_select_orgs, {
      df <- comm_df()
      orgs <- unique(c(
        df$sender_name[df$sender_sub_type == "Organization"],
        df$recipient_name[df$recipient_sub_type == "Organization"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(orgs))
    })
    
    
    observeEvent(input$susp_select_groups, {
      df <- comm_df()
      groups <- unique(c(
        df$sender_name[df$sender_sub_type == "Group"],
        df$recipient_name[df$recipient_sub_type == "Group"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(groups))
    })
    
    observeEvent(input$susp_select_locations, {
      df <- comm_df()
      locations <- unique(c(
        df$sender_name[df$sender_sub_type == "Location"],
        df$recipient_name[df$recipient_sub_type == "Location"]
      ))
      updatePickerInput(session, "suspicious_selected_name", selected = sort(locations))
    })
    
    observeEvent(input$susp_reset, {
      updatePickerInput(session, "suspicious_selected_name", selected = character(0))
    })
    
  
    # ======================
    # Knowledge Graph (by hop level)
    # ======================
    
    observe({
      req(input$nadia_ego_hop, mc3_nodes_cleaned, mc3_edges_final)
      
      ego_name <- "Nadia Conti"
      hop <- switch(input$nadia_ego_hop,
                    "1-hop" = 1,
                    "2-hop" = 2,
                    "3-hop" = 3,
                    1)
      
      # Ensure unique vertex names
      nodes_dedup <- mc3_nodes_cleaned %>% distinct(id, .keep_all = TRUE)
      
      g_full <- igraph::graph_from_data_frame(
        d = mc3_edges_final,
        directed = TRUE,
        vertices = nodes_dedup
      )
      
      nadia_id <- nodes_dedup$id[nodes_dedup$name == ego_name]
      if (length(nadia_id) == 0) return(NULL)
      
      ego_subgraph <- igraph::make_ego_graph(g_full, order = hop, nodes = nadia_id, mode = "all")[[1]]
      ego_nodes <- igraph::as_data_frame(ego_subgraph, what = "vertices")
      ego_edges <- igraph::as_data_frame(ego_subgraph, what = "edges")
      
      legend_colors <- c(
        "Person" = "#88CCEE", "Vessel" = "#D55E00", "Organization" = "#117733",
        "Location" = "#AA4499", "Group" = "#CC79A7", "Event" = "#DDCC77", "Relationship" = "#AF8DC3"
      )
      legend_shapes <- c(
        "Person" = "dot", "Vessel" = "triangle", "Organization" = "square",
        "Location" = "diamond", "Group" = "circle", "Event" = "star", "Relationship" = "square"
      )
      
      vis_nodes <- ego_nodes %>%
        mutate(
          label = if_else(is.na(name), id, name),
          group = if_else(sub_type %in% names(legend_colors), sub_type, type),
          tooltip_extra = case_when(
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
          )
        ) %>%
        select(id, label, group, title) %>%
        distinct(id, .keep_all = TRUE)
      
      vis_edges <- ego_edges %>% rename(from = from, to = to)
      
      output$knowledge_graph <- renderVisNetwork({
        visNetwork(vis_nodes, vis_edges, width = "100%", height = "700px") %>%
          visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1.5))) %>%
          visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
          visIgraphLayout(layout = "layout_with_fr") %>%
          visNodes(font = list(size = 14, color = "black", face = "Roboto Condensed")) %>%
          visLegend(addNodes = tibble::tibble(
            label = names(legend_colors),
            shape = legend_shapes[names(legend_colors)],
            color = legend_colors
          ), useGroups = FALSE, position = "left", ncol = 2)
      })
    })
    
    
    
    
    
  })
}
    
    
    
