# description -------------------------------------------------------------

# A Shiny app ...
# Author: Moritz Gold
# Date: 2020-07-16
# License: MIT License

# libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(openxlsx)
library(tidyverse)
library(ggpubr)
library(FactoMineR)
library(factoextra)


# load data ---------------------------------------------------------------

biowaste_nutrients <- read.xlsx(xlsxFile = here::here("data/waste_sum.xlsx"), sheet = 1) 

performance <- read.xlsx(xlsxFile = here::here("data/performance_sum.xlsx"), sheet = 1)

# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Give Me a Title!"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Tool 1: PCA Biplot", tabName = "tool1", icon = icon("map")),
      menuItem("Tool 2: ...", tabName = "tool2"),
      menuItem("Tool 3: ...", tabName = "tool3"),
      menuItem("Tool 4: ...", tabName = "tool4")
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # input introduction
      
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 12, 
                    h1("Introduction to App: 'NAME'"),
                    h2("Subtitle"),
                    
                    hr(),
                    
                    textOutput(outputId = "introtext")
                )
              )),
      
      
      # input tool 1
      tabItem(tabName = "tool1",
              fluidRow(
                box(width = 6, title = "Substrates",
                    checkboxGroupInput(inputId = "Substrate_groups", 
                                       label = "Select your input substrates for the PCA", 
                                       choices =  levels(as.factor(biowaste_nutrients$Diet_group)) , 
                                       selected = "Food waste")
                ),
                box(width = 6, title = "PCA - Biplot", plotOutput("PCA_substrate_groups"))
              )
              
              # Output of two tests written on server side
              # fluidRow(
              #   textOutput("txt"),
              #   tableOutput("table")
              # )
              
      ), # close tab item tool1
      
      # input tool 1
      tabItem(tabName = "tool2",
              fluidRow(
                box(width = 6, title = "Write a title",
                    
                ),
                box(width = 6, title = "Write a title",
                    
                )
              )
      ), # close tab item tool2
      
      # input tool 1
      tabItem(tabName = "tool3",
              fluidRow(
                box(width = 6, title = "Write a title",
                    
                ),
                box(width = 6, title = "Write a title",
                    
                )
              )
      ), # close tab item tool3
      
      # input tool 1
      tabItem(tabName = "tool4",
              fluidRow(
                box(width = 6, title = "Write a title",
                    
                ),
                
                box(width = 6, title = "Write a title",
                    
                )
              )
      ) # close tab item tool4
    ) # close tab items
  ) # close dashboard body
) # close dashbord page


# server ------------------------------------------------------------------

server <- function(input, output) { 
  
  # Introduction Page
  
  output$introtext <- renderText(
    "I suggest you write an introduction text to you app here. Cite relevant papers and add info that\n
  helps people understand how the app works and what the tools are. As you can see, I have no clue how to do that nicely,\n
  but search engines and StackOverflow will help you.")
  
  
  substr_groups <- reactive({ paste(input$Substrate_groups, collapse = ", ") })
  
  # A test to check if the selected checkboxes work correctly
  
  # output$txt <- renderText({
  #   paste("You chose", substr_groups())
  # })
  
  # select Substrate groups based on input
  
  biowaste_nutrients_subset <- reactive({
    biowaste_nutrients %>% 
      select(-Glucose,-Starch,-Ash_insoluable) %>% 
      filter(Diet_group %in% input$Substrate_groups)  ## here was an issue: access the input Id with "input$"
  })
  
  biowaste_nutrients_clean <- reactive({
    biowaste_nutrients_subset() %>% 
      select(7:11)
  })
  
  # run PCA
  
  pca_biowaste_nutrients <- reactive({
    PCA(biowaste_nutrients_clean(), scale.unit = TRUE)
  })
  
  
  # A test to see if the table renders as wanted
  
  # output$table <- renderTable({
  #   biowaste_nutrients_clean()
  # })
  
  
  # produce output based on manipulated data
  
  output$PCA_substrate_groups <- renderPlot({
    
    fviz_pca_biplot(pca_biowaste_nutrients(),
                    geom.ind = "point",
                    fill.ind = biowaste_nutrients_subset()$Diet_group,
                    pointshape = 21, pointsize = 4,
                    palette = "jco",
                    ggtheme = theme_minimal(),
                    arrowsize = 1,
                    labelsize = 4,
                    mean.point = FALSE,
                    legend.title = "Substrates",
                    alpha.var= 0.5,
                    repel = TRUE)
  })
}


# app ---------------------------------------------------------------------


shinyApp(ui, server)

