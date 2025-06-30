library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  skin = "purple",  
  dashboardHeader(title = span("🌊 COVERT REEF 🪸", style = "font-weight: bold")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Proposal", tabName = "proposal", icon = icon("file-alt")),
      menuItem("Data Preparation", tabName = "datapre", icon = icon("project-diagram")),
      menuItem("Tasks", tabName = "tasks", icon = icon("tasks")),
      menuItem("Team", tabName = "about", icon = icon("users")),
      menuItem("GitHub", icon = icon("github"),
               href = "https://github.com/JianyiLii/Shiny_CovertReef", newtab = TRUE)
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .skin-purple .main-header .navbar {
          background-color: #ffb6c1 !important; /* light pink */
        }
        .skin-purple .main-header .logo {
          background-color: #ffb6c1 !important; /* light pink */
          color: #222 !important; /* dark text for readability */
        }
        .skin-purple .main-header .navbar .sidebar-toggle {
          color: #222 !important;
        }
        h1#titleText {
          color: #8e44ad;
          font-weight: bold;
          text-align: center;
          margin-top: 40px;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "intro",
        div(style = "padding: 20px; max-width: 900px; margin: auto;",
          h2("Intro")
        )
      ),
      tabItem(tabName = "proposal",
        div(style = "padding: 20px; max-width: 900px; margin: auto;",
          h2("Proposal")
        )
      ),
      tabItem(tabName = "datapre",
        div(style = "padding: 20px; max-width: 900px; margin: auto;",
          h2("Data Preparation")
        )
      ),
      tabItem(tabName = "tasks",
        div(style = "padding: 20px; max-width: 900px; margin: auto;",
          h2("Tasks")
        )
      ),
      tabItem(tabName = "about",
        div(style = "padding: 20px; max-width: 900px; margin: auto;",
          h2("Team")
        )
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
