# set working directory
#setwd("~/GitHub/test_shinydashbord")


#install.packages("shiny")
#install.packages("shinydasboard")
#install.packages("shinydashboard")
#install.packages("plotly")

## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(readr)
library(ggplot2)
library(plotly)
library(patchwork)

#Function load
source("Functions/Data_import_and_cleaning.R")
source("Functions/Plots_function.R")

#UI setup

#header
header <- dashboardHeader(
  title = "Tiny Earth"
)



#sidebare

sidebar <- dashboardSidebar(
  sidebarMenu(
  menuItem("Sekventering", tabName = "sekvens", icon = icon("dashboard")),
  menuItem("Fylogentisk", tabName = "fylogen", icon = icon("dashboard")),
  menuItem("Genomisk", tabName = "genomAno", icon = icon("dashboard")),
  menuItem("Antismash", tabName = "antimash", icon = icon("dashboard")),
  menuItem("Report", tabName = "report", icon = icon("dashboard"))
  )
)



#body


  # Sekventing body
  body <- dashboardBody(
    tabItems(
      # Tab for "Sekventerings performens"
      tabItem(tabName = "sekvens",
              h2("Her plottes kvailiet af sekventerings data"),
              fluidRow(
                box(title = "Scatterplot over reads", width = 6, 
                    plotOutput('reads', height = 550)),
                box(title = "scatterplot over contigs", width = 6, 
                    plotOutput('contigs', height = 550)),
                box(title = "Søjleplot over genome kval", width = 12, 
                    plotOutput('genom_kval', height = 550)),
                box(title = "Report skrivnings felt, hvad obseveres der?", width = 12,
                    textInput("text_sekventering_in", "Text input:"))
              
                )
      ),
      
      # Tab for "Fylogentisk forhold"
      tabItem(tabName = "fylogen",
              h2("Her indsættes det fylogenetiske forhold mellem fundet bakterier"),
              fluidRow(
                box(title = "Fylum genetic tree of all species", width=12, 
                    selectInput(inputId = 'test', label = "Select a webpage", choices = c("IOT" = 1, "OneZoom" = 2)),
                uiOutput("inc")),
                box(selectInput(inputId = 'phylum',label = "Select phylum", choices = c("All", "Phylum1","Phylum2" ))),
                box(title = "Fylum plot (tree or heatmap(Fastani)", width = 12, 
                    plotOutput(outputId = 'phylum_plot', height = 500)),
                box(title = "Report skrivnings felt, hvad obseveres der?", width = 12,
                    textInput("text_phylum_in", "Text input:"))
               )
              ),
      
      # Add tab items for other menu items if needed
      tabItem(tabName = "genomAno",
              h2("Content for Genomisk annotering tab"),
              fluidRow(
                box(plotlyOutput(outputId = 'Annotation_plot', height = 800), width = 12)),
            
              
              
              
              fluidRow(
                # Note: Koden skal være så man kan vælge nogen bakterier i være fylum og ud fra dette laves der x antal box med annoterings information om være bakterie 
                box(selectInput("options", "Select Options:",
                                       choices = unique(read.csv("data/Annotation_data.csv")$Assembly), #Skal være bakteriene man har
                                       selected = NULL,
                                multiple = TRUE), width = 12),
                # Laver de forskellige box's 
                uiOutput("dynamicBoxes"),
                
                box(title = "Report skrivnings felt, hvad obseveres der?", width = 12,
                    textInput("text_genome_in", "Text input:"))
                
                
              )
              
              ),
# Antismash tab      
      tabItem(tabName = "antimash",
              h2("Content for Antismash tab"),
           fluidRow(
             
             box(title = "Total mængde af BGC fundet i sekventeret bakterier",
                 plotOutput(outputId = 'BGC_bar', height = 500), width = 12)),
            fluidRow(
             # Slider for at filtrere BGC for similarity
              box(sliderInput(inputId = "similarity_antismash",label = "Filter BGC for similarity", min = 0, max = 100, value = c(0,100))),
            # Dropdown for at vælge type af BGC fra MiBIG
             box(selectInput(inputId = "type_antismash", label = "Select type of BGC", 
                             choices = c("All", unique(read_csv("data/antismash_HQ_2_0.csv")$Most_smimilar_known_cluster_type)), selected = "All"))),
             
             
             
             fluidRow(
             box(title = "Type af BGC antismash har fundet",
                 plotlyOutput(outputId = "BGC_heatmap", height=1000), width = 12),
             
             box(title = "Report skrivnings felt, hvad obseveres der?", width = 12,
                 textInput("text_antismash_in", "Text input:"))
           )
      ),
      
      tabItem(tabName = "report",
              h2("Content for Report tab")
              
              
      )
    )
  )
  





ui <- dashboardPage(
  
  header,
  sidebar,
  body
)

server <- function(input, output) { 
  
# ---- Sequnceing data ----
  output$reads <- renderPlot({
    QC_data <- read.csv("data/QC_data.csv")
    QC1_plot_func(data = QC_data) + QC2_plot_func(data = QC_data)
  })
  
  
  
  #---- Contigs plot ----
  output$contigs <- renderPlot({
    assembly_data <- read.csv("data/Quality_assembly.csv")
    Contig_function(data = assembly_data)
  })
  
  
  #---- Genom kvalitet plot ----
  output$genom_kval <- renderPlot({
    assembly_data <- read.csv("data/Quality_assembly.csv")
    Assebly_quality_plot(data = assembly_data)
  })
  

#---- Fylogentisk data ----
  
  output$inc <- renderUI({
    getPage(input = input$test)
  })
  
  


#----- Genomisk annotation server ------
    #Dannelse af boxes + plots og tabel for være bakterie
  output$dynamicBoxes <- renderUI({
    selectedOptions <- input$options
    
    # Create a list to store box elements
    boxList <- lapply(selectedOptions, function(option) {
      boxTitle <- paste("Box for", option)
      box(id = paste0("box_", gsub(" ", "_", option)),
          title = boxTitle,
          "This is a dynamically generated box.",
          tabsetPanel(
            tabPanel('Plot', plotlyOutput(outputId =  paste0("plot_", gsub(" ", "_", option))), Annotation_type_function(data = read.csv("data/Annotation_data.csv"), 
                                                                                                                         Assembly_name = option)),
            tabPanel('Table', tableOutput(outputId =  paste0("table_", gsub(" ", "_", option))), Annotation_table_function(data = read.csv("data/Annotation_data.csv"),
                                                                                                                           Assembly_name = option))
          )
      )
    })
    
    # Return the list of box elements
    do.call(tagList, boxList)
    
    
  })
  
  # Total number of gene
  output$Annotation_plot <- renderPlotly({
    Annotation_function(data = read.csv("data/Annotation_data.csv"))
  })
  
  
  
#---- Antismash data ----
   
  #---- Total BGC bar plot ----  
  
  output$BGC_bar <- renderPlot({
    BGC_bar_plot(data = read_csv("data/antismash_HQ_2_0.csv"))
  })
  
  #---- Type af BGC antismash har fundet ----
  
  output$BGC_heatmap <- renderPlotly({
    if(input$type_antismash == "All"){
      BGC_heatmap_plot(data = read_csv("data/antismash_HQ_2_0.csv"), low = input$similarity_antismash[1], high = input$similarity_antismash[2])
    } else {
      BGC_heatmap_plot2(data = read_csv("data/antismash_HQ_2_0.csv"), low = input$similarity_antismash[1], high = input$similarity_antismash[2], type = input$type_antismash)
    }
    
    #BGC_heatmap_plot2(data = read_csv("data/antismash_HQ_2_0.csv"), input$similarity_antismash[1], input$similarity_antismash[2])
    
    })

  

  
  
  
  
  
  }

shinyApp(ui, server)
