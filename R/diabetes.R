install.packages("corrplot")
library(corrplot)
library(forecast)
library(tidyverse)
library(data.table)
library(prophet)
library(esquisse)
library(dplyr)
library(ggplot2)
library(lmtest)#diabets

class (diabetes)

correlacao<- cor(diabetes)
corrplot(correlacao,method = "color")

#ts.plot(diabts)

esquisser(diabetes)
library(ggplot2)

##############################################################################################################
#Relação Idade e glicose


ggplot(diabetes) +
 aes(x = Glucose, y = Age) +
 geom_point(shape = "circle", size = 1.5, colour = "#112446") +
 labs(title = "Relação Idade X Glicose") +
 theme_linedraw() +
 theme(plot.title = element_text(face = "italic", 
 hjust = 0.5))

library(ggplot2)
##############################################################################################################
#relação de dispersão entre insulna e espessura da pele

ggplot(diabetes) +
 aes(x = Insulin, y = SkinThickness) +
 geom_point(shape = "circle", size = 1.95, colour = "#D20808") +
 theme_linedraw()
#dispersão
diatestes<- diabetes[1:10,1:8]
(diatestes)
dist(diabetes)

plot(hclust(dist(diatestes)))
plot(hclust(dist (diatestes), method = 'single'))
kmeans(diabetes,4)

diabetes<- (diabetes[,-9])
hclust(dist(diabetes))
plot(hclust(dist(diabetes)))
plot(hclust(dist (diabetes), method = 'single'))

cl<- kmeans(diabetes,4)
plot(diabetes,cl$cluster)
sort(cl$cluster)
