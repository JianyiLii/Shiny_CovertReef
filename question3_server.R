question3_server <- function(id, name_mapping_df = tibble()) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Build full alluvial data
    alluvial_data <- reactive({
      multi_members <- tibble::tibble(
        observed_name = c(
          "Commissioner Blake", "Commissioner Torres", "Council Knowles", "The Middleman", "Jensen from City Council", "Liam Thorne",
          "Boss", "Council Knowles", "Davis", "Glitters Team", "Liam Thorne", "Mako", "Mrs. Money", "Nadia Conti", "Neptune", 
          "V. Miesel Shipping", "Remora", "Rodriguez", "Sam", "Samantha Blake", "Small Fry", "The Accountant", "The Intern", "The Middleman", "Elise",
          "Council Knowles", "V. Miesel Shipping", 
          "Defender", "EcoVigil", "Green Guardians", "Horizon", "Kelly", "Reef Guardian", "The Lookout", "Defender","Seawatch"
        ),
        real_identity = c(
          "Commissioner Blake", "Commissioner Torres", "Council Knowles", "Liam Thorne", "Clepper Jensen", "Liam Thorne",
          "Nadia Conti", "Council Knowles", "Captain Davis", "Sailor Shift Team", "Liam Thorne", "Mako", "Elise", "Nadia Conti", "Neptune", 
          "V. Miesel Shipping", "Remora", "Rodriguez", "Sam", "Samantha Blake", "Rodriguez", "Elise", "Sam", "Liam Thorne", "Elise",
          "Council Knowles", "V. Miesel Shipping",
          "Sentinel", "EcoVigil", "Green Guardians", "Horizon", "Kelly", "Reef Guardian", "Kelly", "Sentinel","Kelly"
        ),
        community = c(
          rep("City Council", 6), 
          rep("Sailor Shift Team", 19), 
          rep("Influential Families", 2), 
          rep("Local Conservationist Group", 9)
        )
      )
      
      name_mapping_df %>%
        bind_rows(multi_members) %>%
        distinct() %>%
        filter(!is.na(real_identity) & !is.na(observed_name) & !is.na(community)) %>%
        count(real_identity, observed_name, community, name = "value")
    })
    
    # Update dropdown after data is ready
    observe({
      updateSelectInput(session, "person", choices = c("All", sort(unique(alluvial_data()$real_identity))))
    })
    
    # Filtered data
    filtered_data <- reactive({
      if (input$person == "All") {
        alluvial_data()
      } else {
        dplyr::filter(alluvial_data(), real_identity == input$person)
      }
    })
    
    # Plot
    
    output$alluvialPlot <- renderPlot({
      df <- filtered_data()
      
      # Wrap long labels
      df_long <- df %>%
        mutate(
          real_identity = str_wrap(real_identity, width = 12),
          observed_name = str_wrap(observed_name, width = 12),
          community = str_wrap(community, width = 12)
        )
      
      ggplot(df_long,
             aes(axis1 = real_identity, axis2 = observed_name, axis3 = community, y = value)) +
        geom_alluvium(aes(fill = community), width = 1/12, alpha = 0.8) +
        geom_stratum(width = 1/12, fill = "grey90", color = "black") +
        geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5, hjust = 0.5, vjust = 0.5, lineheight = 0.9) +
        scale_x_discrete(
          limits = c("Real Identity", "Observed Name", "Community"),
          expand = c(.05, .05)
        ) +
        labs(
          title = if (input$person == "All") "All Identity Flows" else paste(input$person, "'s Identity Flow"),
          x = NULL, y = "Link Count"
        ) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        )
    })
    
  })
}
