library(ggplot2)
library(plotly)
library(dplyr)
library(caret)

## Calculated finished sq ft
plotFeatureDensSingle <- function(feature1, feature2) {
  density1 <- density(feature)
  
  plot_ly(x = ~density1$x, y = ~density1$y, 
          type = 'scatter', mode = 'lines', 
          name = 'Training Set', fill = 'tozeroy',
          fillcolor = 'rgba(168, 216, 234, 0.25)') %>%
    layout(xaxis = list(title = print(feature)),
           yaxis = list(title = 'Density'))
  
}

plotFeatureDensDouble <- function(feature1, feature2) {
  density1 <- density(feature1)
  density2 <- density(feature2)
  
  plot_ly(x = ~density1$x, y = ~density1$y, 
          type = 'scatter', mode = 'lines', 
          name = 'Training Set', fill = 'tozeroy',
          fillcolor = 'rgba(168, 216, 234, 0.25)') %>%
    add_trace(x = ~density2$x, y = ~density2$y,
              name = 'Properties File', fill = 'tozeroy',
              fillcolor = 'rgba(255, 212, 96, 0.25)') %>%
    layout(xaxis = list(title = print(feature1)),
           yaxis = list(title = 'Density'))
  
}

plotFeatureDensDouble(cleanTraining$calculatedfinishedsquarefeet, cleanProperties$calculatedfinishedsquarefeet)


cleanProperties <- cleanProperties %>%  filter(cleanProperties$calculatedfinishedsquarefeet < 10000 & 
                                                  cleanProperties$calculatedfinishedsquarefeet > 500) 

cleanTraining <- cleanTraining %>%  filter(cleanTraining$calculatedfinishedsquarefeet < 10000 & 
                                                 cleanTraining$calculatedfinishedsquarefeet > 500) 


cleanProperties <- cleanProperties %>%  filter(cleanProperties$structuretaxvaluedollarcnt < 375000)# & 
                                                 # cleanProperties$calculatedfinishedsquarefeet > 500) 

cleanTraining <- cleanTraining %>%  filter(cleanTraining$structuretaxvaluedollarcnt < 375000) #& 
                                             # cleanTraining$calculatedfinishedsquarefeet > 500) 


