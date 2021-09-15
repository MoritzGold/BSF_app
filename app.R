# Commented code for BSFL substrate explorer app -------------------------------------------------------------

# BSFL substrate explorer
# Author: Moritz Gold and Lars Schöbitz
# This is the version that was published with the peer-reviewed manuscript "Efficient and safe substrates
# for black soldier fly biowaste treatment along circular economy principles", Detritus Journal.
# License: MIT License

# libraries ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(openxlsx)
library(tidyverse)
library(ggpubr)
library(FactoMineR)
library(factoextra)
library(DT)


# load data ---------------------------------------------------------------

# Additonal data can be added by uploading a sheet with additional information in the same format

## data for tab "nutrient composition" 

biowaste_nutrients <- read.xlsx(xlsxFile = here::here("data/waste_sum.xlsx"), sheet = 1) %>% 
  
  rename("Insoluable ash" = "Ash_insoluable") %>%

  rename("Fibres (cellulose+lignin)" = "Cellulose_lignin") %>% 
  
  rename("Protein" = "crude_protein") %>% 
  
  gather(12:19,key = parameter,value = value) %>% 
  
  mutate(value=round(value,1)) %>% 
  
  spread(key = parameter,value = value)

unused_nutrients <- c("Glucose","Starch","Insoluable ash")

## data for tab "conversion performance"

performance <- read.xlsx(xlsxFile = here::here("data/performance_sum.xlsx"), sheet = 1)


biowaste_nutrients_narrow <- biowaste_nutrients %>% 
  
  gather(12:19,key = Nutrient_parameter,value = value) 

## data for tab "substrate mixture formulation"

performance_narrow <- read.xlsx(xlsxFile = here::here("data/performance_sum.xlsx"), sheet = 1) %>% 
  
  gather(12:17,key = Performance_indicator,value = value,na.rm = TRUE) %>% 
  
  mutate(Performance_indicator=case_when(Performance_indicator=="larval_wet_mg_wet"~"Larval weight (mg wet)",TRUE~Performance_indicator)) %>% 

  mutate(Performance_indicator=case_when(Performance_indicator=="larval_mg_DM"~"Larval weight (mg dry)",TRUE~Performance_indicator)) %>% 

  mutate(Performance_indicator=case_when(Performance_indicator=="Bioconversion_rate_perc_DM"~"Bioconversion rate (% dry)",TRUE~Performance_indicator)) %>% 
  
  mutate(Performance_indicator=case_when(Performance_indicator=="waste_reduction_perc_DM"~"Waste reduction (% dry)",TRUE~Performance_indicator))  %>% 

  mutate(Performance_indicator=case_when(Performance_indicator=="Bioconversion_rate_perc_wet"~"Bioconversion rate (% wet)",TRUE~Performance_indicator)) %>% 
  
  mutate(Performance_indicator=case_when(Performance_indicator=="waste_reduction_perc_wet"~"Waste reduction (% wet)",TRUE~Performance_indicator))  
  
  
# ui ----------------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "BSFL Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Nutrient composition", tabName = "tool2",icon=icon("chart-pie")),
      menuItem("Conversion performance", tabName = "tool3", icon=icon("chart-bar")),
      menuItem("Substrate mixture formulation", tabName = "tool4",icon=icon("mortar-pestle"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # input introduction (main/landing page)
      
      tabItem(tabName = "intro",
              fluidRow(
                box(width = 6, 
                    h2("'BSFL Substrate Explorer'"),
                    h3("Towards efficient black soldier fly larae production with different biowastes"),
                    
                    hr(),
                    
                    p("Welcome to the BSFL Substrate explorer! A main challenge for planners and operators of black soldier fly larvae 
                    biowaste conversion is knowledge on the performance of different biowastes. The three tools included in this app summarize and visualize 
                    data regarding the conversion of biowastes with black soldier fly larvae. It should help operators and planners to receive realistic performance metrics and formulate biowaste mixtures."),
                    
                    hr(),
                    
                    p("The first version of this tool was shared in the following publication (see for methodology, references and acknowledgements):"),
                    
                    p("Gold M., Ireri D., Zurbrügg C., Fowles T., Mathys A. (2021) Efficient and safe substrates for black soldier fly biowaste treatment along circular economy principles, Detritus."),
                    
                    p("All data can be found on the Eawag Research Data Institutional Collection (https://opendata.eawag.ch/) and GitHub (https://github.com/MoritzGold/BSF_app). Contact Moritz Gold
                      (moritz.gold@gmail.com, https://www.linkedin.com/in/moritzgold) for any questions.")
                    
                    # textOutput(outputId = "introtext")
                )
                
              )
              ),
      
      # input tab "nutrient composition"
      tabItem(tabName = "tool2",
              
              # first row
              fluidRow(
                
                box(width = 6, title = "Select substrate and nutrient parameter of your interest",
                    column(width = 6, checkboxGroupInput(inputId = "Substrate_groups", label = "Substrate groups",
                                                         choices = levels(as.factor(biowaste_nutrients_narrow$Diet_group)),selected = "Food waste")),  
                           
                          
                    
                    column(width = 6, checkboxGroupInput(inputId = "Nutrient_parameter",label = "Nutrient parameter",
                                                         choices = levels(as.factor(biowaste_nutrients_narrow$Nutrient_parameter)),selected = "Ash")),
                ),
                
                box(width = 6, title = "Two-dimensional representation of waste nutrient composition", 
                    plotOutput("PCA_substrate_groups"))
                
              ),
                    
              
                
              # second row
              
              fluidRow(  
                
                box(width = 12, title = "Nutrient composition (boxplot)",
                    plotOutput("Boxplot_substrate_groups"))
                
              ),
              
              # second row
              fluidRow(
                box(width = 12, title = "Nutrient composition (statistics, tabular form)",
                    dataTableOutput("Nutrient_composition_summary")
                )
              ),
              
              # third row
              fluidRow(
                box(width = 12, title = "Nutrient composition raw data",
                    dataTableOutput("Nutrient_composition_raw")
                )
              )
              
      ), # close tab item "nutrient composition"
      
      # input tab "substrate mixture formulation"
      
      tabItem(tabName = "tool4",
              
              #first row
              
              fluidRow(width=12,
                       
                       box(width=6,
                           
                           h4("Formulation of biowaste mixtures"),
                           
                           p("This tool calculates and plots the nutrient composition of a biowaste mixture. 
                             The purpose of this tool is to allow you to play around with different biowaste formulations, based on your waste availability"),
                           
                           p("Increasing and balancing digestible nutrients can increase growth performance and reduce variability. There is not one optimal nutrient content and ratio:
                           In general, high digestible nutrient contents (i.e. protein, digestible carbohydrates, lipids) and low fibres and ash contents should be preferred.")
                           
                           ), 
                       
                       box(width=6,
                           p(tags$b("Step1"),"Enter the number of ingredients of the waste formulation. These are the number of wastes you have available"),
                           
                           # select the number of ingredients (min 2 - maximum 6)
                           
                           selectInput("n", label=NULL, choices = c("2","3","4","5","6")),
                           
                           p(tags$b("Step2:"),"Select your wastes, and enter the quantity and moisture content per waste."),
                           
                           # Execute calculation
                           
                           p(tags$b("Step3:"),"Press the button below to calculate and plot the nutrient content of the waste formulation."),
                           actionButton("goButton", "Calculate!")
                           
                       )
                       
              ),
              
              
              # second row
              
              fluidRow(width=12,
                       
                       splitLayout(
                         
                         box(width=12,title = "Select formulation ingredients",  
                             uiOutput("col1")),
                         
                         box(width=12,title= "Enter waste quantity per ingredient (in kg)",
                             uiOutput("col2")),
                         
                         box(width=12,title= "Enter the moisture conten per ingredient (in % water)",
                             uiOutput("col3"))
                       )),
              
              
              #third row
              
              fluidRow(
                
                box(width = 12,
                    
                    # show the nutrient composition of the substrate
                    
                    plotOutput("Nutrients_formulation_plot")),
                
                dataTableOutput("Nutrients_formulation_dt")
                
              )
              
      
              
              
              
      ),# close tab "substrate mixture formulation"
              
      # input tab "conversion performance"
      tabItem(tabName = "tool3",
              
              # first row
              fluidRow(
                
                column(width = 6,
                
                box(width = 12, 
                    
                    h4("Process performance"),
                    
                    p("This tool summarizes and visualizes the conversion performance of BSFL with different biowastes. Firstly, select the substrate to display the available performance metrics. 
                      Secondly, select the performance metric of your interest."),
                    
                    p("The raw data and data references can be displayed by pressing the button below"),
                    
                    actionButton("goButton_raw_tool2","Show raw data!")),
                    
                    
                    
                  box(width = 12, 
                      
                      checkboxGroupInput(inputId = "Performance_indicator_tool4", label = "Performance indicator", 
                                         choices = levels(as.factor(performance_narrow$Performance_indicator)), 
                                         selected = "Bioconversion_rate_perc_DM"))
                  
                ),
                
                column(width=6,
                
              box(width = 12, checkboxGroupInput(inputId = "Substrate_groups_tool4", label = "Substrate groups", 
                                                         choices =  levels(as.factor(performance_narrow$Diet_group)), 
                                                         selected = "Food waste - Household"))
                )
              ),
                    
            
                # second row
                fluidRow(
                  box(width = 12, title = "Performance indicator (boxplot)",
                             plotOutput("Boxplot_substrate_groups_tool4")
                  )
                  
                ),
                  
                
                
                # second row
                fluidRow(
                  box(width = 12, title = "Performance indicator (statistics, tabular form)",
                      dataTableOutput("Performance_indicator_summary_tool4")
                  )
                ),
              
              # third row
              fluidRow(
                box(width = 12, title = "Performance indicator raw data",
                    dataTableOutput("Performance_indicator_summary_tool4_raw")
                )
              )
              
              
                
      
      ) # close tab "conversion performance"
    ) # close all tab items
  ) # close dashboard body
) # close dashbord page


# server ------------------------------------------------------------------

server <- function(input, output,session) { 
  
  
  # tab "nutrient composition" ------------------------------------------------------------------

  substr_groups <- reactive({ paste(input$Substrate_groups, collapse = ", ") })
  
  # A test to check if the selected checkboxes work correctly
  
  # output$txt <- renderText({
  #   paste("You chose", substr_groups())
  # })
  
  # select Substrate groups based on input
  
  biowaste_nutrients_subset <- reactive({
    biowaste_nutrients %>% 
      select(-Glucose,-Starch,-'Insoluable ash') %>% 
      filter(Diet_group %in% input$Substrate_groups) 
  })
  
  
  biowaste_nutrients_subset_raw <- eventReactive(input$goButton_raw_tool2,{
    biowaste_nutrients_subset() %>% 
      select(-Diet,-Replicate,-Diet_group_detailed,-Performance,-Journal,-Title,-Person) %>% 
      filter(Diet_group %in% input$Substrate_groups) %>% 
      
      unite("Reference",c("Author","Year"),sep = "-") %>% 
      
      rename("Substrate group" = "Diet_group")
  
  })
  
  biowaste_nutrients_clean <- reactive({
    biowaste_nutrients_subset() %>% 
      select(12:16)
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
    
    # Check for required value in Substrate_groups and Nutrient_parameter before calculating plot
    
    req(input$Substrate_groups != 0)
    req(input$Nutrient_parameter != 0)
    
    fviz_pca_biplot(pca_biowaste_nutrients(),
                    geom.ind = "point",
                    fill.ind = biowaste_nutrients_subset()$Diet_group,
                    pointshape = 21, pointsize = 5,
                    palette = "jco",
                    ggtheme = theme_minimal(),
                    arrowsize = 2,
                    labelsize = 6,
                    mean.point = FALSE,
                    legend.title = "Substrate group",
                    alpha.var= 0.5,
                    repel = TRUE) +
      
      theme(text = element_text(size=17)) +
      
      labs(title ="")
  })
  
    
  biowaste_nutrients_narrow_subset <-
    
    reactive({
      
      # select Substrate groups based on input
      
      biowaste_nutrients_narrow %>% 
        
        filter(Diet_group %in% input$Substrate_groups & Nutrient_parameter %in% input$Nutrient_parameter)
      
    })
  
  
  # calculate descriptive statistics
  
  biowaste_nutrients_stats <- reactive({
    biowaste_nutrients_narrow_subset() %>% 
      group_by(Diet_group,Nutrient_parameter) %>% 
      summarise(n = n(),
                mean = round(mean(value,na.rm = TRUE), 1),
                sd = round(mean(value,na.rm = TRUE), 1),
                median = round(median(value,na.rm = TRUE), 1),
                max = round(max(value,na.rm = TRUE), 1),
                min = round(min(value,na.rm = TRUE), 1)) %>% 
      
      rename("Substrate group" = "Diet_group") %>% 
      
      rename("Nutrient" = "Nutrient_parameter")
      
      
  })
  
  
  # produce boxplot output based on manipulated data
  
  output$Boxplot_substrate_groups <- renderPlot({
    
    # Check for required value in Substrate_groups and Nutrient_parameter before calculating plot
    
    req(input$Substrate_groups != 0)
    req(input$Nutrient_parameter != 0)
    
    biowaste_nutrients_narrow_subset()  %>% 
      ggplot(aes(Diet_group,value)) +
      geom_boxplot() +
      #geom_point() +
      ## Alternative which shows the overlying points a little nicer
      geom_jitter(width = 0.1,size=3) + 
      labs(y = "% dry waste", x="", title = "Biowaste nutrients") +
      facet_wrap(~Nutrient_parameter) +
      coord_flip() +
      
      theme_bw() +
      
      theme(text = element_text(size=17))
  })
  
  
  # produce summmary table 
  
  output$Nutrient_composition_summary <- renderDT({biowaste_nutrients_stats()})  
  
  # produce summmary table 
  
  output$Nutrient_composition_raw <- renderDT({biowaste_nutrients_subset_raw()})  
  
  
  # tab "substrate mixture formulation" ------------------------------------------------------------------
  
  # save input field data
  
  ingredients <- reactive(paste0("col1", seq_len(input$n)))
  amounts <- reactive(paste0("col2", seq_len(input$n)))
  MC <- reactive(paste0("col3", seq_len(input$n)))
  
  # ui: ingredients
  
  output$col1 <- renderUI({
    map(ingredients(), ~ selectInput(.x, label = NULL,choices =   levels(as.factor(biowaste_nutrients_narrow$Diet_group))))
    
  })
  
  # ui: amounts
  
  output$col2 <- renderUI({
    map(amounts(), ~ numericInput(.x, NULL,value = 1))
    
    
  })
  
  # ui: amounts
  
  output$col3 <- renderUI({
    map(MC(), ~ numericInput(.x, NULL,value = 1))
    
    
  })
  
  # retrieve vectors with amounts and ingredients
  
  ingredient_values <- reactive({map_chr(ingredients(), ~ input[[.x]])})
  amount_values <- reactive({map_chr(amounts(), ~ input[[.x]])})
  MC_values <- reactive({map_chr(MC(), ~ input[[.x]])})
  
  # save vectors as df
  
  ingredient_amount <- eventReactive(input$goButton, {
    
    bind_cols(ingredient_values(),amount_values(),MC_values()) %>% 
      
      setNames(c("Diet_group","Amount","MC")) %>% 
      
      mutate(Amount_DM=(as.numeric(Amount))*((100-as.numeric(MC))/100))
    
  })
  
  
  # calculate mean, min, max of nutrient paramaters based on the selected ingredients
  
  biowaste_formulation <- reactive({
    
    biowaste_nutrients_narrow %>% 
      
      filter(Diet_group %in% ingredient_amount()$Diet_group) %>% 
      
      filter(!Nutrient_parameter %in% unused_nutrients)  %>% 
      
      ungroup() %>% 
      
      group_by(Diet_group,Nutrient_parameter) %>% 
      
      summarise(mean = round(mean(value,na.rm = TRUE), 1),
                max = round(max(value,na.rm = TRUE), 1),
                min = round(min(value,na.rm = TRUE), 1)) %>% 
      
      # add the amount per diet group
      
      left_join(ingredient_amount(),by=("Diet_group")) %>% 
      
      gather(3:5,key = descriptive,value = value) %>% 
      
      mutate(contribution_DMnutrient_DMwaste=(value/100)*as.numeric(Amount_DM)) %>% 
      
      ungroup() %>%
      
      group_by(Nutrient_parameter,descriptive) %>%
      
      summarise(`% dry mass`=round((sum(contribution_DMnutrient_DMwaste)/sum(as.numeric(ingredient_amount()$Amount_DM))*100),1),
                `% wet mass`=round((sum(contribution_DMnutrient_DMwaste)/sum(as.numeric(ingredient_amount()$Amount))*100),1)) %>% 
      
      gather(3:4,key = Unit,value = formulation_value) 
    
  })

  # transform df in narrow form
  
  biowaste_formulation_wide <- reactive({
    
    biowaste_formulation() %>% 
      
      spread(key = descriptive,value = formulation_value)
    
  })  
  
  # calculate max value for dynamic y axis limit
  
  max_value <- reactive({
    
    biowaste_formulation_wide() %>% 
      
      ungroup() %>%   
      
      summarise(max=max(max)) %>% 
      
      pull() 
    
  })
  
  
  # make plot
  
  output$Nutrients_formulation_plot <- renderPlot({
    
    biowaste_formulation_wide() %>% 
      
      ggplot(aes(Nutrient_parameter,mean)) +
      
      geom_point(size=5,aes(color=Unit),position=position_dodge(width=0.4)) +
      
      geom_errorbar(aes(ymin=min,ymax=max,color=Unit),width=.1,linetype=5, position=position_dodge(width=0.4)) +
      
      # scale_y_continuous(limits = c(0, max)) +
      
      theme_bw() +
      
      xlab("Nutrient parameter") +
      
      ylab("Nutrient content") +
      
       theme(text = element_text(size=17))
    
    
  })
  
  # filter data based on selected ingredients
  
  output$Nutrients_formulation_dt <- renderDT({biowaste_formulation_wide()})
  
  
  
  # tab "conversion performance" ------------------------------------------------------------------
  
  observe({
    
    Performance_indicator_availability <- performance_narrow %>% filter(Diet_group  %in% input$Substrate_groups_tool4)
    
    updateCheckboxGroupInput(session, "Performance_indicator_tool4",
                             label = "Performance indicator",
                             choices = levels(as.factor(Performance_indicator_availability$Performance_indicator))
    )
  })
  
  
  performance_summary_narrow_subset <-
    
    reactive({
      # select Substrate groups based on input
      performance_narrow %>% 
        filter(
          Diet_group %in% input$Substrate_groups_tool4 & 
            Performance_indicator %in% input$Performance_indicator_tool4
          )
      
    })
  
  
  performance_summary_narrow_subset_raw <- eventReactive(input$goButton_raw_tool4,{
      # select Substrate groups based on input
      performance_summary_narrow_subset() %>% 
        select(Component_1,Diet_group,Author,Year,Performance_indicator,value) %>% 
        unite("Reference",c("Author","Year"),sep = "-")
        
    }
  
  )
  
  
  # calculate descriptive statistics
  
  performance_narrow_stats <- reactive({
    
    performance_summary_narrow_subset() %>% 
      group_by(Diet_group, Performance_indicator) %>% 
      summarise(n = n(),
                mean = round(mean(value, na.rm = TRUE), 1),
                sd = round(mean(value, na.rm = TRUE), 1),
                median = round(median(value, na.rm = TRUE), 1),
                max = round(max(value,na.rm = TRUE), 1),
                min = round(min(value,na.rm = TRUE), 1))
  })
  
  
  # produce boxplot output based on manipulated data
  
  output$Boxplot_substrate_groups_tool4 <- renderPlot({
    
    req(input$Performance_indicator_tool4 != 0)
    
    performance_summary_narrow_subset()  %>% 
      
      ggplot(aes(Diet_group, value)) +
      geom_boxplot() +
      geom_point() +
      labs(y = "% dry waste", x="", title = "Biowaste nutrients") +
      facet_wrap(~Performance_indicator,scales = "free_x") +
      
      coord_flip() +
      
      theme_bw() +
      
      theme(text = element_text(size=17))
  })
  
  
  # produce summmary table 
  
  output$Performance_indicator_summary_tool4 <- renderDT({ performance_narrow_stats() %>% 
      
      rename(`Substrate group`="Diet_group") %>% 
      
      rename(`Performance metric`="Performance_indicator")
      
      })
  
  # produce raw data
  
  output$Performance_indicator_summary_tool4_raw <- renderDT({ performance_summary_narrow_subset_raw() %>% 
      
      rename(`Substrate group`="Diet_group") %>% 
      
      rename(`Performance metric`="Performance_indicator") %>% 
      
      rename(`Substrate`="Component_1")
    
  })
  
  
  
}


# app ---------------------------------------------------------------------


shinyApp(ui, server)

