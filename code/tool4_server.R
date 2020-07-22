
# description -------------------------------------------------------------

  # Summarize performance indicators of BSFL treatment for different biowaste groups
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

performance_narrow <- read.xlsx(xlsxFile =here::here("data/performance_sum.xlsx"),sheet = 1) %>% 

                      gather(11:16,key = parameter,value = value,na.rm = TRUE)

# save input options

levels(as.factor(performance_narrow$Diet_group))

levels(as.factor(performance_narrow$parameter))


# produce boxplot 


performance_narrow  %>% 
  
  ggplot(aes(Diet_group,value)) +
  
  geom_boxplot() +
  
  geom_point() +
  
  labs(y = "% dm", x="", title = "Biowaste nutrients") +
  
  facet_wrap(~parameter) +
  
  coord_flip()


# produce data summary


performance_narrow %>% 
  
  group_by(Diet_group,parameter) %>% 
  
  summarise(n=n(),
            mean=round(mean(value,na.rm = TRUE),1),
            sd=round(mean(value,na.rm = TRUE),1),
            median=round(median(value,na.rm = TRUE),1),
            max=round(max(value,na.rm = TRUE),1),
            min=round(min(value,na.rm = TRUE),1))
