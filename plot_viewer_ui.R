# universal_plot_viewer_ui.R
plot_viewer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("plot_selector"), "Choose a Plot:", choices = NULL),
    uiOutput(ns("plot_output"))
  )
}


# universal_plot_viewer_server.R
plot_viewer_server <- function(id, rds_path = "rds_objects") {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load RDS files
    rds_files <- list.files(rds_path, pattern = "\\.rds$", full.names = TRUE)
    plot_objects <- list()
    
    for (f in rds_files) {
      name <- tools::file_path_sans_ext(basename(f))
      tryCatch({
        obj <- readRDS(f)
        plot_objects[[name]] <- obj
      }, error = function(e) {
        message(sprintf("Failed to read %s: %s", name, e$message))
      })
    }
    
    # Update selectInput choices
    updateSelectInput(session, "plot_selector", choices = names(plot_objects))
    
    # Dynamically render plot based on type
    output$plot_output <- renderUI({
      req(input$plot_selector)
      obj <- plot_objects[[input$plot_selector]]
      
      if (inherits(obj, "gg") || inherits(obj, "ggraph")) {
        plotname <- ns("ggplot")
        output[["ggplot"]] <- renderPlot({ obj })
        plotOutput(plotname, height = "700px")
      } else if (inherits(obj, "plotly")) {
        plotname <- ns("plotly")
        output[["plotly"]] <- plotly::renderPlotly({ obj })
        plotly::plotlyOutput(plotname, height = "700px")
      } else if (inherits(obj, "visNetwork")) {
        plotname <- ns("visnet")
        output[["visnet"]] <- visNetwork::renderVisNetwork({ obj })
        visNetwork::visNetworkOutput(plotname, height = "700px")
      } else if (inherits(obj, "leaflet")) {
        plotname <- ns("leaf")
        output[["leaf"]] <- leaflet::renderLeaflet({ obj })
        leaflet::leafletOutput(plotname, height = "700px")
      } else if (inherits(obj, "highchart")) {
        plotname <- ns("hchart")
        output[["hchart"]] <- highcharter::renderHighchart({ obj })
        highcharter::highchartOutput(plotname, height = "700px")
      } else {
        tags$p("This object is not a recognized plot type.")
      }
    })
  })
}
