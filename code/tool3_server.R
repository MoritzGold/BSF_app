
# description -------------------------------------------------------------



# libraries ---------------------------------------------------------------

library(openxlsx)
library(magrittr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(tidyverse)

# load data

biowaste_nutrients_narrow <- read.xlsx(xlsxFile =here::here("data/waste_sum.xlsx"),sheet = 1) %>% 
  
  gather(7:14,key = parameter,value = value) 

unused_nutrients <- c("Glucose","Starch","Ash_insoluable")


# input selection -----

  # formulation ingredients

  ingredients <- levels(as.factor(biowaste_nutrients_narrow$Diet_group))
  
  # nutrient parameters
  
  levels(as.factor(biowaste_nutrients_narrow$parameter))

  
# save selected ingredients  

  selected_ingredients <- paste0(ingredients)             

  
# save amounts of ingredients
  
  # example
  
    # 150 kg "Faecal sludge"
    # 399 kg "Food waste"
    # 50 kg "Human faeces"
  
    # ingredients
  
    ingredients_example <- c("Animal manures","Food waste","Human faeces")
    
    selected_ingredients_example <- paste0(ingredients_example)
    
    # amounts of ingredients
    
    amounts_example <- c("150","300","50")
  
    selected_amounts_example <- as.numeric(amounts_example)
  
    
# merge ingrdients and amounts
    
    ingredient_amount <-
    
    bind_cols(selected_ingredients_example,selected_amounts_example) %>% 
      
      setNames(c("Diet_group","Amount"))
    

        
# calculate Nutrient_parameter value for the formulation (based on the selected ingredients and amounts)
    
    nutrient_content_formulation <- 
    
    biowaste_nutrients_narrow %>% 
      
      # filter Diet_group based in the ingredients selected
      
      filter(Diet_group %in% selected_ingredients_example) %>% 
      
      filter(!parameter %in% unused_nutrients) %>% 
      
      ungroup() %>% 
      
      group_by(Diet_group,parameter) %>% 
      
      summarise(mean=mean(value,na.rm = TRUE),
               min=min(value),
               max=max(value)) %>% 
      
      # consider the amount per diet group 
      
      left_join(ingredient_amount,by=("Diet_group")) %>% 

      gather(3:5,key = descriptive,value = value) %>% 
      
      mutate(contribution=value*Amount) %>% 
      
      ungroup() %>% 
      
      group_by(parameter,descriptive) %>% 
      
      summarise(formulation_value=sum(contribution)/sum(selected_amounts_example))
  

# plot diet formuluation mean, max, min
    
    
    # transform df in narrow form
    
    nutrient_content_formulation_wide <- 
    
    nutrient_content_formulation %>% 
      
      spread(key = descriptive,value = formulation_value)
    
    
    # calculate max value for dynamic y axis limit
    
    max= max(nutrient_content_formulation_wide$max)
    
    max <- 
    
    nutrient_content_formulation_wide %>% 
      
      ungroup() %>% 
      
      summarise(max=max(max)) %>% 
      
      pull()
    
    # make plot
    
   
    
    nutrient_content_formulation_wide %>% 
    
      ggplot(aes(parameter,mean)) +
      
      geom_point(size=5) +
      
      geom_errorbar(aes(ymin=min,ymax=max),width=.1,linetype=5) +
      
      scale_y_continuous(limits = c(0, max)) +
      
      theme_bw() +
      
      xlab("Nutrient parameter") +
      
      ylab("Nutrient content")
    
    
    

    