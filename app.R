library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "purple",  
  dashboardHeader(title = span("🌊 COVERT REEF", style = "font-weight: bold")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Proposal", tabName = "proposal", icon = icon("file-alt")),
      menuItem("Methodology", tabName = "methodology", icon = icon("project-diagram")),
      menuItem("Findings", tabName = "findings", icon = icon("book")),
      menuItem("Poster", tabName = "poster", icon = icon("file")),
      menuItem("Meeting Minutes", tabName = "minutes", icon = icon("calendar-alt")),
      menuItem("The Team", tabName = "team", icon = icon("users")),
      menuItem("Shiny App", tabName = "shinyapp", icon = icon("chart-bar")),
      menuItem("GitHub", icon = icon("github"),
               href = "https://github.com", newtab = TRUE)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        h1#titleText {
          color: #8e44ad;
          font-weight: bold;
          text-align: center;
          margin-top: 40px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "proposal", h2("Proposal content here...")),
      tabItem(tabName = "methodology", h2("Methodology content here...")),
      tabItem(tabName = "findings", h2("Findings content here...")),
      tabItem(tabName = "poster", h2("Poster content here...")),
      tabItem(tabName = "minutes", h2("Meeting Minutes content here...")),
      tabItem(tabName = "team", h2("Our Amazing Team")),
      tabItem(tabName = "shinyapp", h2("Shiny App Content Here")),
      
      # Homepage or default
      tabItem(tabName = "dashboard",
              h1("Covert Reef", id = "titleText"))
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
