
# description -------------------------------------------------------------

  # Summarize biowaste composition for different BSFL substrates for different biowaste groups
  # Show as boxplots (for visualization)
  # Show datatable with descriptive statistics (downloadable, for further calculations and reports)
  # Allow access to raw data (downloadable, for references and further details)

# libraries ---------------------------------------------------------------

library(openxlsx)
library(magrittr)
library(dplyr)
library(FactoMineR)
library(factoextra)

# load data

biowaste_nutrients_narrow <- read.xlsx(xlsxFile =here::here("data/waste_sum.xlsx"),sheet = 1) %>% 
  
  gather(7:14,key = parameter,value = value) 


# save input options

levels(as.factor(biowaste_nutrients_narrow$Diet_group))

levels(as.factor(biowaste_nutrients_narrow$parameter))


# produce boxplot 

biowaste_nutrients_narrow  %>% 
  
  ggplot(aes(Diet_group,value)) +
  
  geom_boxplot() +
  
  geom_point() +
  
  labs(y = "% dm", x="", title = "Biowaste nutrients") +
  
  facet_wrap(~parameter) +
  
  coord_flip()

# produce data summary

biowaste_nutrients_narrow %>% 
  
  group_by(Diet_group,parameter) %>% 
  
  summarise(n=n(),
            mean=round(mean(value,na.rm = TRUE),1),
            sd=round(mean(value,na.rm = TRUE),1),
            median=round(median(value,na.rm = TRUE),1),
            max=round(max(value,na.rm = TRUE),1),
            min=round(min(value,na.rm = TRUE),1))
            
            
