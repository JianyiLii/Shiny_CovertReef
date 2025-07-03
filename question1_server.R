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
        left_join(nodes() %>% select(id, content, timestamp), by = c("to_id" = "id")) %>%
        rename(event_id = to_id, event_content = content, event_timestamp = timestamp) %>%
        left_join(edges() %>% filter(edge_type == "received") %>%
                    select(event_id_match = from_id, recipient_id = to_id),
                  by = c("event_id" = "event_id_match")) %>%
        left_join(nodes() %>% select(id, name, sub_type), by = c("from_id" = "id")) %>%
        rename(sender_id_actual = from_id, sender_name = name, sender_sub_type = sub_type) %>%
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
    
    # Timeline table
    output$timeline_table <- DT::renderDT({
      DT::datatable(
        comm_df() %>%
          select(date, time, sender_name, recipient_name, content) %>%
          arrange(date, time),
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # Time dropdown UI
    output$time_slider_ui <- renderUI({
      req(comm_df())
      all_times <- sort(unique(floor_date(comm_df()$timestamp, unit = "2 hours")))
      selectInput(ns("time_point"), "Select Time Slice:",
                  choices = format(all_times, "%Y-%m-%d %H:%M:%S"),
                  selected = format(all_times[1], "%Y-%m-%d %H:%M:%S"))
    })
    
    # Time-filtered comm_df for selected 2-hour slice
    comm_filtered <- reactive({
      req(input$time_point)
      selected_time <- as.POSIXct(input$time_point, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      comm_df() %>%
        filter(timestamp >= selected_time,
               timestamp < selected_time + lubridate::hours(2))
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
    

    
    
    # Heatmap
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
    
    # Export
    return(list(comm_df = comm_df))
  })
}
