
# description -------------------------------------------------------------




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
    menuItem("Tool1", tabName = "Tool1")
  ),
  
  dashboardBody(
    
    # input tool 1
    tabItem(tabName = "Tool1",
            fluidRow(
              checkboxGroupInput(inputId = "Substrate_groups", 
                                 label = "Substrate groups", 
                                 choices =  levels(as.factor(biowaste_nutrients$Diet_group)) , 
                                 selected = "Food waste")
            ),
            fluidRow(
              plotOutput("PCA_substrate_groups")
            ),
            fluidRow(
              textOutput("txt"),
              tableOutput("table")
            )
            
    )
  )
)


# server ------------------------------------------------------------------

server <- function(input, output) { 
  
  
  substr_groups <- reactive({ paste(input$Substrate_groups, collapse = ", ") })
  
  output$txt <- renderText({
    paste("You chose", substr_groups())
  })
  
  # select Substrate groups based on input
  
  biowaste_nutrients_subset <- reactive({
    biowaste_nutrients %>% 
      select(-Glucose,-Starch,-Ash_insoluable) %>% 
      filter(Diet_group %in% input$Substrate_groups)  ## access the input Id with 'input$foo'
  })
  
  biowaste_nutrients_clean <- reactive({
    biowaste_nutrients_subset() %>% 
      select(7:11)
  })
  
  # run PCA
  
  pca_biowaste_nutrients <- reactive({
    PCA(biowaste_nutrients_clean(), scale.unit = TRUE)
  })
  
  output$table <- renderTable({
    biowaste_nutrients_clean()
  })
  
  
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

