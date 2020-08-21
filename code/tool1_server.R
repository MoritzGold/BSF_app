
# description -------------------------------------------------------------



# libraries ---------------------------------------------------------------

library(openxlsx)
library(magrittr)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggrepel)


# load data

biowaste_nutrients <- read.xlsx(xlsxFile =here::here("data/waste_sum.xlsx"),sheet = 1) 

# save input options

input_Substrate_groups <- levels(as.factor(biowaste_nutrients$Diet_group)) 
  
# select data based on input  

biowaste_nutrients_subset <-  biowaste_nutrients %>% 
  
  select(-Glucose,-Starch,-Ash_insoluable) %>% 
  
  filter(Diet_group %in% input_Substrate_groups)

# remove data description for PCA

biowaste_nutrients_clean <- 
  
  biowaste_nutrients_subset %>% select(7:11)

# run PCA

pca_biowaste_nutrients <- PCA(biowaste_nutrients_clean,scale.unit=TRUE)


# plot PCA

p <-

fviz_pca_biplot(pca_biowaste_nutrients,
                  geom.ind = "point",
                  fill.ind = biowaste_nutrients_subset$Diet_group,
                  pointshape = 21, pointsize = 4,
                  palette = "jco",
                  ggtheme = theme_minimal(),
                  arrowsize = 1,
                  labelsize = 4,
                  mean.point = FALSE,
                  legend.title = "Substrates",
                  alpha.var=0.5,
                  repel = TRUE)
# get coordindates


pca_coordinates <-

pca_biowaste_nutrients$ind$coord %>% as_tibble() %>% 
  
  select(Dim.1,Dim.2) %>% 

  bind_cols(biowaste_nutrients_subset$Diet) %>% 
  
  rename("Diet"="...3")


# add labels to the plot based on the coordinates

  p + 
    
    # geom_point(aes(x=pca_coordinates$Dim.1,y=pca_coordinates$Dim.2)) + 
    
    geom_text_repel(data=pca_coordinates,aes(x=Dim.1,y=Dim.2,label=Diet))
    

