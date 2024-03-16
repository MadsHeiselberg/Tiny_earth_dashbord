library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Opening Web Pages"),
  dashboardSidebar(
    selectInput(inputId = 'test', label = "Select a webpage", choices = c("IOT" = 1, "OneZoom" = 2))
  ),
  dashboardBody(
    fluidRow(box(selectInput(inputId = 'test', label = "Select a webpage", choices = c("IOT" = 1, "OneZoom" = 2)))), 
    uiOutput("inc")
  )
)

server <- function(input, output) {
  getPage <- function() {
    cat("Selected option:", input$test, "\n")
    switch(input$test,
           "1" = tags$iframe(src = "https://itol.embl.de/itol.cgi",
                             style = "width:100%;", frameborder = "0",
                             id = "iframe",
                             height = "1000px"),
           "2" = tags$iframe(src = "https://www.onezoom.org/life.html/@biota=93302?img=best_any&anim=flight#x820,y622,w0.7403",
                             style = "width:100%;", frameborder = "0",
                             id = "iframe",
                             height = "1000px"),
           tags$span("Please select a webpage.")
    )
  }
  
  output$inc <- renderUI({
    getPage()
  })
}

shinyApp(ui, server)
