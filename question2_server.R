question2_server <- function(id, comm_df) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- TAB 1: Timeline UI Elements ---
    person_vessel_df <- reactive({
      req(comm_df())
      df <- comm_df() %>%
        filter(
          sender_sub_type %in% c("Person", "Vessel"),
          recipient_sub_type %in% c("Person", "Vessel")
        )
      
      shinyWidgets::updatePickerInput(
        session, "pv_selected_name",
        choices = sort(unique(c(df$sender_name, df$recipient_name))),
        selected = NULL
      )
      
      df
    })
    
    observeEvent(input$pv_reset, {
      shinyWidgets::updatePickerInput(session, "pv_selected_name", selected = character(0))
    })
    
    observeEvent(input$pv_select_people, {
      df <- person_vessel_df()
      people <- unique(c(
        df$sender_name[df$sender_sub_type == "Person"],
        df$recipient_name[df$recipient_sub_type == "Person"]
      ))
      shinyWidgets::updatePickerInput(session, "pv_selected_name", selected = sort(people))
    })
    
    observeEvent(input$pv_select_vessels, {
      df <- person_vessel_df()
      vessels <- unique(c(
        df$sender_name[df$sender_sub_type == "Vessel"],
        df$recipient_name[df$recipient_sub_type == "Vessel"]
      ))
      shinyWidgets::updatePickerInput(session, "pv_selected_name", selected = sort(vessels))
    })
    
    output$filtered_timeline_table <- DT::renderDT({
      df <- plot_data1()

  df %>%
    mutate(
      comm_date = as.Date(timestamp),
      comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S"))
    ) %>%
    select(
      `Event ID` = event_id,
      Date = comm_date,
      Time = comm_time_of_day,
      Sender = sender_name,
      Recipient = recipient_name,
      Content = content
    ) %>%
    arrange(Date, Time) %>%
    DT::datatable(
      options = list(pageLength = 10),
      rownames = FALSE
    )
})

    
    plot_data1 <- reactive({
      df <- person_vessel_df()
      selected <- input$pv_selected_name
      
      if (!is.null(selected) && length(selected) > 0) {
        role <- input$pv_name_role
        if (!is.null(selected) && length(selected) > 0) {
          role <- input$pv_name_role
          
          if (role == "sender") {
            df <- df %>% filter(sender_name %in% selected)
          } else if (role == "recipient") {
            df <- df %>% filter(recipient_name %in% selected)
          } else {
            df <- df %>% filter(sender_name %in% selected | recipient_name %in% selected)
          }
        }
        
      }
      
      
      df %>%
        mutate(
          timestamp = as.POSIXct(timestamp),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          sender_sub_type = factor(sender_sub_type, levels = c("Person", "Vessel")),
          communicating_pair_sorted = paste(pmin(sender_name, recipient_name), pmax(sender_name, recipient_name), sep = " & "),
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
    })
    
    output$pv_timeline_table <- reactable::renderReactable({
      df <- plot_data1()
      
      if (nrow(df) == 0) {
        return(reactable::reactable(data.frame(Message = "No communications found for the selected filter.")))
      }
      
      reactable::reactable(
        df %>%
          select(
            Date = comm_date,
            Time = comm_time_of_day,
            Sender = sender_name,
            Recipient = recipient_name,
            `Event ID` = event_id,
            Content = content
          ),
        searchable = TRUE,
        filterable = TRUE,
        pagination = TRUE,
        defaultPageSize = 10,
        columns = list(
          Content = reactable::colDef(html = TRUE, minWidth = 300)
        )
      )
    })
    
    
    # --- TAB 2: Communities UI Elements ---
    plot_data2 <- reactive({
      person_vessel_df() %>%
        mutate(
          timestamp = as.POSIXct(timestamp),
          comm_date = as.Date(timestamp),
          comm_time_of_day = hms::as_hms(format(timestamp, "%H:%M:%S")),
          sender_sub_type = factor(sender_sub_type, levels = c("Person", "Vessel")),
          communicating_pair_sorted = paste(pmin(sender_name, recipient_name), pmax(sender_name, recipient_name), sep = " & "),
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
    })
    
    output$pv_timeline_plot <- renderPlotly({
      df <- plot_data1()
      p <- ggplot(df, aes(x = comm_date, y = comm_time_of_day)) +
        geom_point(aes(color = sender_id, shape = sender_sub_type, text = tooltip_text),
                   size = 2, alpha = 0.7, show.legend = c(color = TRUE, shape = FALSE)) +
        scale_shape_manual(values = c("Person" = 16, "Vessel" = 17)) +
        facet_wrap(~ sender_sub_type, ncol = 1) +
        scale_y_time(
          limits = hms::as_hms(c("08:00:00", "14:00:00")),
          breaks = hms::as_hms(sprintf("%02d:00:00", 8:14)),
          labels = sprintf("%02d:00", 8:14)
        ) +
        scale_x_date(date_breaks = "1 day", date_labels = "%d %b") +
        labs(title = "Communication Events Over Time (Sender's Perspective)",
             x = "Date", y = "Time of Day", color = "Sender") +
        theme_minimal(base_size = 9) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p, tooltip = "text")
    })
    
    g_pv <- reactive({
      tryCatch({
        # Set seed for reproducibility
        set.seed(1234)
        
        edge_tbl <- plot_data2() %>%
          select(from = sender_name, to = recipient_name) %>%
          count(from, to)
        
        g_raw <- as_tbl_graph(edge_tbl, directed = FALSE)
        
        if (nrow(as_tibble(g_raw, what = "nodes")) == 0) {
          stop("Graph has no nodes")
        }
        
        g_raw %>%
          mutate(
            community = group_louvain(),
            pagerank = centrality_pagerank(),
            degree = centrality_degree(),
            betweenness = centrality_betweenness(),
            closeness = centrality_closeness()
          )
      }, error = function(e) {
        showNotification(paste("Louvain/PageRank error:", e$message), type = "error")
        return(tbl_graph(nodes = tibble(name = character()), edges = tibble(from = character(), to = character())))
      })
    })
    
    output$pv_centrality_table <- renderTable({
      g <- g_pv()
      if (nrow(as_tibble(g)) == 0) return(tibble(name = "No Data"))
      
      g %>%
        as_tibble() %>%
        select(Community = community, Name = name, PageRank = pagerank, Degree = degree, Betweenness = betweenness, Closeness = closeness) %>%
        arrange(desc(PageRank)) %>%
        head(10) %>%
        mutate(across(c(PageRank, Degree, Betweenness, Closeness), ~ round(.x, 4)))
    })
    
    output$pv_network_plot <- renderPlot({
      g <- g_pv()
      if (nrow(as_tibble(g)) == 0) {
        plot.new(); title("No network data available"); return()
      }
      ggraph(g, layout = "fr") +
        geom_edge_link(aes(width = ..index..), alpha = 0.3) +
        geom_node_point(aes(size = pagerank, color = as.factor(community))) +
        geom_node_text(aes(label = name), repel = TRUE, size = 7) +
        scale_size_continuous(range = c(2, 6)) +
        theme_void() +
        labs(title = "Network Graph: Louvain Communities & Centralities")
    })
    
    output$pv_wordcloud_plot <- renderPlot({
      if (!exists("bigrams") || !"community" %in% names(bigrams)) {
        plot.new(); title("Missing or invalid `bigrams` dataset"); return()
      }
      g <- g_pv()
      par(mfrow = c(2, 3))
      communities <- unique(as_tibble(g)$community)
      for (i in sort(communities)) {
        words <- bigrams %>% filter(community == i)
        if (nrow(words) == 0) next
        wordcloud(words = words$bigram, freq = words$n, max.words = 30,
                  scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), random.order = FALSE)
        mtext(paste("Community", i), side = 3, line = 1, adj = 0.5, cex = 1.2)
      }
    })
    
    output$pv_bigram_circular <- renderPlot({
      if (!exists("bigrams") || !"community" %in% names(bigrams)) {
        plot.new(); title("Missing or invalid `bigrams` dataset"); return()
      }
      
      top_bigrams <- bigrams %>%
        group_by(community) %>%
        slice_max(n, n = 10) %>%
        ungroup() %>%
        mutate(
          bigram = str_to_title(bigram),
          bigram = reorder_within(bigram, n, community)
        )
      
      ggplot(top_bigrams, aes(x = bigram, y = n, fill = as.factor(community))) +
        geom_col(show.legend = TRUE) +
        coord_polar() +
        facet_wrap(~ community, scales = "free") +
        scale_x_reordered() +
        labs(
          title = "Top 10 Bigrams per Louvain Community",
          x = NULL, y = "Frequency", fill = "Community"
        ) +
        theme_minimal(base_size = 10) +
        theme(axis.text.x = element_text(angle = 90, size = 6))
    })
    
    
    output$pv_summary_table <- renderTable({
      g <- g_pv()
      if (nrow(as_tibble(g)) == 0) {
        return(tibble(`Group Number` = NA, `Group Name` = "No Data", `Topic Area` = "", Members = ""))
      }
      
      # Define community label mapping (outside of pipeline)
      community_labels <- c(
        "1" = "Sailor Shift Team",
        "2" = "Suspicious",
        "3" = "Conservationist Group",
        "4" = "Pseudonym",
        "5" = "Hacklee Herald"
      )
      
      # Get members per community
      observed <- g %>%
        as_tibble() %>%
        select(name, community) %>%
        group_by(community) %>%
        summarise(Members = paste(sort(name), collapse = ", "), .groups = "drop")
      
      # Use globally precomputed bigrams (from global.R wordcloud)
      # These are filtered from comm_df_static for Person/Vessel only
      bigram_topics <- bigrams %>%
        group_by(community) %>%
        slice_max(n, n = 5) %>%
        summarise(`Topic Area` = paste(str_to_title(bigram), collapse = ", "), .groups = "drop")
      
      # Merge and assign human-readable names
      observed %>%
        left_join(bigram_topics, by = "community") %>%
        mutate(
          `Group Number` = community,
          `Group Name` = community_labels[as.character(community)]
        ) %>%
        select(`Group Number`, `Group Name`, `Topic Area`, Members)
    })
    
    output$pv_findings_pplvessel <- renderUI({
      HTML(paste0(
        "<p><strong>Core logic on uncovering pseudonyms, through interactions and relationships (Ans Qn 3 as well):</strong></p>",
        "<ul>",
        "<li>If two names appear as sender and recipient in the same message, they cannot belong to the same person — i.e., they’re not aliases of each other.</li>",
        "<li>If two names sent a message at the exact time, they cannot belong to the same person.</li>",
        "<li>For instance, if Nadia sent a message to The Accountant, they would not be the same individual. ",
        "If Nadia sent a message at 10am to The Accountant and The Lookout also sent a message at 10am to The Intern, ",
        "Nadia and The Lookout cannot be the same person.</li>",
        "</ul>",
        
        "<p><strong>Alias Analysis (select these names in the filter and read the conversations):</strong></p>",
        "<ul>",
        "<li><strong>The Accountant, Mrs. Money, Elise:</strong> Close timings between Mrs. Money and Elise on 8 and 10 Oct suggest they are different people. ",
        "However, Elise disappears on 10 Oct and reappears as The Accountant and Mrs. Money on 11 Oct, continuing only as The Accountant till 14 Oct — possibly the same person.</li><br/>",
        
        "<li><strong>Liam and The Middleman:</strong> The Middleman disappeared on 7 Oct and reappeared as Liam on 8 Oct. ",
        "On 11 Oct, Mrs. Money asked The Middleman if anything was found by conservation vessels. That same day, Liam replied Elise saying nothing was found. Likely the same person.</li><br/>",
        
        "<li><strong>The Boss and Nadia:</strong> The Boss disappeared on 5 Oct and reappeared as Nadia on 8 Oct. Likely the same person.</li><br/>",
        
        "<li><strong>Small Fry and Rodriguez:</strong> On 2 Oct Rodriguez corresponded with Remora and Mako about meeting at slip #14. ",
        "On 14 Oct, Rodriguez responded to the same message under different names. Likely the same person.</li><br/>",
        
        "<li><strong>The Lookout and Sam:</strong> On 7 Oct Sam asked Kelly who authorized the permit. Two minutes later, The Lookout (Kelly) responded to The Intern (Sam) that it was signed by Jensen from City Council.</li><br/>",
        
        "<li><strong>Seawatch alias hypothesis:</strong> Seawatch only appeared on 10 Oct, but Horizon talked to Seawatch on 8 Oct. ",
        "Therefore, someone else acted as Seawatch earlier. On 3 Oct, Defender told Seawatch to maintain vigilance. ",
        "The Lookout (Seawatch) responded to Sentinel (Defender) at 8:41am confirming.</li>",
        "</ul>"
      ))
    })
    
    
    output$pv_findings_louvain1 <- renderUI({
      HTML(paste(
        "<p>There were <strong>5 closely associated groups</strong>. <strong>Community 5</strong> (Clepper and Miranda) appeared to be segmented from the central group, likely due to the investigative nature of their work and lack of operational entanglement.</p>",
        
        "<p>From the network graph, we extracted <strong>10 influential nodes</strong> to focus on:</p>",
        
        "<ul style='padding-left: 20px;'>",
        
        "<li><strong>Community 1:</strong> Mako, Neptune, Davis, Remora, Nadia</li>",
        "<li><strong>Community 2:</strong> Boss, Mrs Money</li>",
        "<li><strong>Community 3:</strong> Reef Guardian, Sentinel, Horizon</li>",
        "<li><strong>Community 4:</strong> N/A – not influential at global level</li>",
        "<li><strong>Community 5:</strong> N/A – not influential at global level</li>",
        
        "</ul>"
      ))
    })
    
    output$pv_findings_louvain2 <- renderUI({
      HTML(paste(
        "<ul style='padding-left: 20px;'>",
        
        "<li><strong>Sailor Shift Team (Comm. 1):</strong> Neptune, Remora, Nadia, and Davis were working on Nemo Reef operation. This referred to the Music Video Production for Sailor Shift on 14 Oct.</li><br/>",
        
        "<li><strong>Suspicious (Comm. 2):</strong> The Middleman had access to Council documents. Mrs. Money received funding through channels not visible to regulators. On 5 Oct, Boss instructed Mrs. Money to disguise financial trails via tourism and to destroy evidence of Nemo Reef operations.</li><br/>",
        
        "<li><strong>Conservationist Group (Comm. 3):</strong> Samantha Blake informed Mako to stop operations on 8 and 10 Oct. Serenity is a private luxury yacht. Osprey was likely a tourism vessel looking for charter from Mako for their tourists.</li><br/>",
        
        "<li><strong>Pseudonym (Comm. 4):</strong> Other than communicating among themselves, The Lookout also corresponded externally with Sentinel, Reef Guardian, and Horizon (conservation-based topics), while The Intern also contacted Mrs. Money.</li><br/>",
        
        "<li><strong>Hacklee Herald (Comm. 5):</strong> Conversations between Clepper and his intern Miranda ended on 11 Oct. Miranda mentioned an Oceanus City Council Member meeting with unmarked vessels at night.</li>",
        
        "</ul>"
      ))
    })
    
  })
}
