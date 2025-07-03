wordcloudServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Sample wordcloud data
    word <- c("vessels", "mining", "funding", "music", "reef", "conservation", "turtles",
              "fuel", "permit", "?", "suspicious", "tourism", "lighting", "meetings",
              "council", "harbor", "relationships", "communications", "operation", "underwater")
    freq <- c(5, 8, 7, 8, 9, 5, 6, 10, 8, 4, 3, 3, 5, 2, 3, 2, 5, 8, 3, 4)
    word_data_wc <- data.frame(word = word, freq = freq)
    
    ocean_colors_lp <- c(
      "#1B1B3A", "#0072B2", "#009E73", "#D55E00", "#CC79A7",
      "#882255", "#AA4499", "#004D40", "#333333"
    )
    
    output$wordcloudPlot <- renderPlot({
      par(bg = "white")
      wordcloud(
        words = word_data_wc$word,
        freq = word_data_wc$freq,
        min.freq = 1,
        max.words = 200,
        random.order = FALSE,
        colors = ocean_colors_lp,
        rot.per = 0.20,
        scale = c(4, 0.8)
      )
    })
  })
}
