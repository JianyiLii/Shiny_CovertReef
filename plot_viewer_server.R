plot_viewer_server <- function(input, output, session) {
  rds_path <- "rds_objects"
  
  # Load list of plot RDS files
  rds_files <- reactive({
    files <- list.files(rds_path, pattern = "\\.rds$", full.names = FALSE)
    names(files) <- files
    files
  })
  
  # Populate dropdown
  observe({
    updateSelectInput(session, "plot_selector", choices = rds_files())
  })
  
  # Render the appropriate plot based on type
  output$plot_output <- renderUI({
    req(input$plot_select)
    obj <- readRDS(file.path(rds_path, input$plot_select))
    
    if (inherits(obj, "gg") || inherits(obj, "ggraph")) {
      renderPlot({ obj })
    } else if (inherits(obj, "plotly")) {
      renderPlotly({ obj })
    } else if (inherits(obj, "visNetwork")) {
      renderVisNetwork({ obj })
    } else if (inherits(obj, "leaflet")) {
      renderLeaflet({ obj })
    } else if (inherits(obj, "highchart")) {
      renderHighchart({ obj })
    } else {
      renderPrint("Unsupported object type.")
    }
  })
}
