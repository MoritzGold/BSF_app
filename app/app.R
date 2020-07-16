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
  
  dashboardHeader(),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", icon = icon("info-circle")),
      menuItem("Tool 1: PCA Biplot", tabName = "Tool1", icon = icon("map")),
      menuItem("Tool 2: ...")
    )
  ),
  
  dashboardBody(
  
    # input tool 1
    tabItem(tabName = "Tool1",
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
            
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output) { 
  
  
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

