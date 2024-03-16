
#install.packages("shiny")
#install.packages("shinydasboard")
#install.packages("shinydashboard")

## app.R ##
library(shiny)
library(shinydashboard)

#UI setup

#header
header <- dashboardHeader(
  title = "Tiny Earth"
)



#sidebare

sidebar <- dashboardSidebar(
  menuItem("Sekventerings performens", tabName = "sekvens"),
  menuItem("Fylogentisk forhold", tabName = "fylogen"),
  menuItem("Genomisk annotering", tabName = "genomAno"),
  menuItem("Antismash", tabName = "antimash"),
  menuItem("Report", tabName = "report")
  
)



#body

body <- dashboardBody(
  
)




ui <- dashboardPage(
  header,
  sidebar,
  body
)

server <- function(input, output) { }

shinyApp(ui, server)
