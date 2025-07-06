question1_server <- function(id, mc3_nodes_raw, mc3_edges_raw) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Prepare nodes
    nodes <- reactive({
      mc3_nodes_raw %>%
        mutate(id = as.character(id)) %>%
        filter(!is.na(id)) %>%
        distinct(id, .keep_all = TRUE) %>%
        rename(supertype = type) %>%
        select(id, name, sub_type, supertype, content, timestamp)
    })
    
    # Prepare edges
    edges <- reactive({
      mc3_edges_raw %>%
        rename(from_id = source, to_id = target, edge_type = type) %>%
        mutate(across(c(from_id, to_id), as.character)) %>%
        filter(from_id %in% nodes()$id, to_id %in% nodes()$id)
    })
    
    # Communications dataframe
    comm_df <- reactive({
      sent <- edges() %>%
        filter(edge_type == "sent") %>%
        mutate(sender_id_actual = from_id) %>%
        left_join(nodes() %>% select(id, content, timestamp), by = c("to_id" = "id")) %>%
        rename(event_id = to_id, event_content = content, event_timestamp = timestamp) %>%
        left_join(edges() %>%
                    filter(edge_type == "received") %>%
                    select(event_id_match = from_id, recipient_id = to_id),
                  by = c("event_id" = "event_id_match")) %>%
        left_join(nodes() %>% select(id, name, sub_type), by = c("sender_id_actual" = "id")) %>%
        rename(sender_name = name, sender_sub_type = sub_type) %>%
        left_join(nodes() %>% select(id, name, sub_type), by = c("recipient_id" = "id")) %>%
        rename(recipient_name = name, recipient_sub_type = sub_type) %>%
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
          timestamp = event_timestamp
        ) %>%
        mutate(
          timestamp = as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
          date = as.Date(timestamp),
          time = format(timestamp, "%H:%M:%S")
        )
      sent
    })
    
    # Update input choices for timeline filters
    observe({
      req(comm_df())
      df <- comm_df()
      
      updateDateRangeInput(session, "timeline_date_range",
                           start = min(df$date, na.rm = TRUE),
                           end = max(df$date, na.rm = TRUE))
      
      updatePickerInput(session, "sender_filter", choices = sort(unique(df$sender_name)))
      updatePickerInput(session, "recipient_filter", choices = sort(unique(df$recipient_name)))
    })
    
    # Handle reset & people-only filters
    observeEvent(input$reset_timeline_filters, {
      df <- comm_df()
      updateDateRangeInput(session, "timeline_date_range",
                           start = min(df$date), end = max(df$date))
      updatePickerInput(session, "entity_type_filter_timeline",
                        selected = c("Person", "Vessel", "Organization", "Group", "Location"))
      updatePickerInput(session, "sender_filter", selected = character(0))
      updatePickerInput(session, "recipient_filter", selected = character(0))
    })
    
    observeEvent(input$select_all_people, {
      df <- comm_df()
      people <- sort(unique(
        c(
          df$sender_name[df$sender_sub_type == "Person"],
          df$recipient_name[df$recipient_sub_type == "Person"]
        )
      ))
      updatePickerInput(session, "sender_filter", selected = people)
      updatePickerInput(session, "recipient_filter", selected = people)
    })
    
    # Filtered timeline dataframe
    filtered_timeline_df <- reactive({
      req(comm_df(), input$timeline_date_range, input$entity_type_filter_timeline)
      df <- comm_df()
      
      df <- df %>%
        filter(
          date >= input$timeline_date_range[1],
          date <= input$timeline_date_range[2],
          sender_sub_type %in% input$entity_type_filter_timeline |
            recipient_sub_type %in% input$entity_type_filter_timeline
        )
      
      if (!is.null(input$sender_filter) && length(input$sender_filter) > 0) {
        df <- df %>% filter(sender_name %in% input$sender_filter)
      }
      
      if (!is.null(input$recipient_filter) && length(input$recipient_filter) > 0) {
        df <- df %>% filter(recipient_name %in% input$recipient_filter)
      }
  
  df
 })

    
    # Timeline table (filtered)
    output$timeline_table <- DT::renderDT({
      req(filtered_timeline_df())
      DT::datatable(
        filtered_timeline_df() %>%
          select(date, time, sender_name, recipient_name, content, event_id) %>%
          arrange(date, time),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    
    # Time dropdown UI
    output$time_slider_ui <- renderUI({
      req(comm_df())
      all_times <- sort(unique(floor_date(comm_df()$timestamp, unit = "1 hours")))
      selectInput(ns("time_point"), "Select Time Slice:",
                  choices = format(all_times, "%Y-%m-%d %H:%M:%S"),
                  selected = format(all_times[1], "%Y-%m-%d %H:%M:%S"))
    })
    
    # Time-filtered comm_df for selected -hour slice
    comm_filtered <- reactive({
      req(input$time_point)
      selected_time <- as.POSIXct(input$time_point, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      comm_df() %>%
        filter(timestamp >= selected_time,
               timestamp < selected_time + lubridate::hours(1))
    })
    
    # Chord diagram
    output$chord_plot <- renderUI({
      df <- comm_filtered()
      
      chord_df <- df %>%
        count(sender_name, recipient_name, name = "count") %>%
        filter(!is.na(sender_name), !is.na(recipient_name))
      
      entities <- unique(c(chord_df$sender_name, chord_df$recipient_name))
      matrix_df <- matrix(0, nrow = length(entities), ncol = length(entities),
                          dimnames = list(entities, entities))
      
      for (i in seq_len(nrow(chord_df))) {
        matrix_df[chord_df$sender_name[i], chord_df$recipient_name[i]] <- chord_df$count[i]
      }
      
      entity_colors <- setNames(
        grDevices::rainbow(length(entities), s = 0.6, v = 0.85),
        entities
      )
      
      temp_file <- tempfile(fileext = ".png")
      png(temp_file, width = 800, height = 600)
      circlize::circos.clear()
      circlize::chordDiagram(
        x = matrix_df,
        grid.col = entity_colors,
        transparency = 0.3,
        annotationTrack = "grid",
        preAllocateTracks = list(track.height = 0.1)
      )
      circlize::circos.trackPlotRegion(
        track.index = 1,
        bg.border = NA,
        panel.fun = function(x, y) {
          sector_name <- circlize::get.cell.meta.data("sector.index")
          xlim <- circlize::get.cell.meta.data("xlim")
          ylim <- circlize::get.cell.meta.data("ylim")
          circlize::circos.text(
            x = mean(xlim),
            y = ylim[1] + circlize::mm_y(2),
            labels = sector_name,
            facing = "bending.inside",
            niceFacing = TRUE,
            cex = 0.8
          )
        }
      )
      dev.off()
      
      tags$img(src = base64enc::dataURI(file = temp_file, mime = "image/png"), width = "100%")
    })
    
    # Filtered timeline table (based on selected time slice)
    output$filtered_timeline_table <- DT::renderDT({
      df <- comm_filtered()
      DT::datatable(
        df %>%
          select(event_id, date, time, sender_name, recipient_name, content) %>%
          arrange(date, time),
        options = list(pageLength = 10),
        rownames = FALSE,
        colnames = c("Event ID", "Date", "Time", "Sender", "Recipient", "Content")
      )
    })
    
    
    # Communication Heatmap (All Time)
    output$heatmap_plot <- plotly::renderPlotly({
      df <- comm_df() %>%
        count(sender_name, recipient_name, name = "count") %>%
        mutate(
          sender_name = factor(sender_name, levels = rev(sort(unique(sender_name)))),
          recipient_name = factor(recipient_name, levels = sort(unique(recipient_name)))
        )
      
      p <- ggplot(df, aes(x = recipient_name, y = sender_name, fill = count)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "white", high = "navyblue") +
        labs(
          title = "Sender-Recipient Communication Heatmap",
          x = "Recipient", y = "Sender", fill = "Messages"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    
    output$heatmap_daily_plot <- renderPlotly({
      df <- comm_df()
      
      # Prepare datetime breakdown
      heatmap_data <- df %>%
        mutate(
          ts = timestamp,
          date = as.Date(ts),
          hour = lubridate::hour(ts)
        ) %>%
        count(date, hour) %>%
        ungroup() %>%                         # <--- this is often needed
        complete(
          date = seq(min(date), max(date), by = "1 day"),
          hour = 0:23,
          fill = list(n = 0)
        )
      
      p <- ggplot(heatmap_data, aes(x = hour, y = date, fill = n)) +
        geom_tile(color = "white") +
        scale_x_continuous(
          breaks = 0:23,
          expand = c(0, 0)
        ) +
        scale_y_date(
          date_breaks = "1 day",
          date_labels = "%b %d",
          expand = c(0, 0)
        ) +
        scale_fill_distiller(
          name = "Messages",
          palette = "Spectral",
          direction = 1
        ) +
        labs(
          title = "Daily Communication Patterns",
          x = "Hour of Day",
          y = "Date"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(hjust = 0.5),
          axis.text.y = element_text(size = 8),
          panel.grid = element_blank()
        )
      
      plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
    })
    
    # Chord pair summary table (used as hover info alternative)
    output$chord_table <- DT::renderDataTable({
      df <- comm_filtered()
      
      chord_df <- df %>%
        count(sender_name, recipient_name, name = "count") %>%
        filter(!is.na(sender_name), !is.na(recipient_name)) %>%
        arrange(desc(count))
      
      DT::datatable(
        chord_df,
        options = list(
          pageLength = 10,
          order = list(list(2, 'desc'))  # sort by count
        ),
        rownames = FALSE,
        colnames = c("Sender", "Recipient", "Messages")
      )
    })
    
    output$week_comparison_plot <- renderPlot({
      req(comm_df(), input$entity_type_filter)
      
      df <- comm_df() %>%
        filter(
          sender_sub_type %in% input$entity_type_filter |
            recipient_sub_type %in% input$entity_type_filter
        )
      
      week_patterns <- df %>%
        mutate(week = if_else(date <= min(date) + lubridate::days(6), "Week 1", "Week 2")) %>%
        count(week, hour = lubridate::hour(timestamp)) %>%
        group_by(week) %>%
        mutate(proportion = n / sum(n))
      
      hour_breaks <- seq(min(week_patterns$hour), max(week_patterns$hour), by = 1)
      prop_breaks <- seq(0, ceiling(max(week_patterns$proportion) * 100) / 100, by = 0.02)
      
      ggplot(week_patterns, aes(x = hour, y = proportion, color = week)) +
        geom_line(size = 1.2) +
        geom_point(size = 3) +
        scale_x_continuous(breaks = hour_breaks) +
        scale_y_continuous(
          breaks = prop_breaks,
          labels = scales::percent_format(accuracy = 1)
        ) +
        labs(
          title = "Hourly Communication Patterns: Week 1 vs Week 2",
          x = "Hour of Day",
          y = "Percent of Total Messages"
        ) +
        theme_light(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
          legend.position = "top",
          legend.title = element_blank(),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          axis.text.x = element_text(vjust = 0.5),
          axis.text.y = element_text(size = 8)
        )
    })
    
    
    # Update choices for dropdown
    observe({
      req(comm_df())
      all_names <- sort(unique(c(comm_df()$sender_name, comm_df()$recipient_name)))
      updateSelectInput(session, "selected_person", choices = all_names)
    })
    
    # Render top contacts table for selected person
    output$top_contacts_table <- DT::renderDataTable({
      req(input$selected_person, comm_df())
      
      selected <- input$selected_person
      df <- comm_df()
      
      # Filter communication involving the selected person
      related_df <- df %>%
        filter(sender_name == selected | recipient_name == selected)
      
      contact_summary <- related_df %>%
        mutate(
          contact = if_else(sender_name == selected, recipient_name, sender_name),
          contact_subtype = if_else(sender_name == selected, recipient_sub_type, sender_sub_type)
        ) %>%
        filter(!is.na(contact), contact != selected) %>%
        count(contact, contact_subtype, name = "message_count") %>%
        arrange(desc(message_count))
      
      DT::datatable(
        contact_summary,
        colnames = c("Contact", "Subtype", "Messages"),
        options = list(pageLength = 6),
        rownames = FALSE
      )
    })
    
    # Export
    return(list(comm_df = comm_df))
  })
}
