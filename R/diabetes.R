# Execute import dataset no arquivo diabetes.csv
  library(caret)
  library(rpart)
  library(rpart.plot)
  library(cluster)
  library(caTools)
  library(MultivariateAnalysis)
  library(corrplot)
  library(forecast)
  library(randomForest)
  library(tidyverse)
  library(data.table)
  library(e1071)
  library(prophet)
  library(esquisse)
  library(dplyr)
  library(ggplot2)
  library(FuzzyR)
  library(lmtest)#diabets
plot(diabetes)
##############################################################################################################
require
#View(dbets)
#class (dbets)

correlacao<- cor(diabetes)
corrplot(correlacao,method = "color", type = ("full"))
esquisser(diabetes)

##############################################################################################################
#Histograma de dispers�o

ggplot(diabetes) +
  aes(x = BloodPressure) +
  geom_histogram(bins = 100L, fill = "#4682B4") +
  labs(
    x = "Press�o Sangu�nea",
    y = "N�mero de pessoas",
    title = "Histograma press�o Sang�inea "
  ) +
  theme_linedraw()
##############################################################################################################
#Rela��o Idade e glicose

ggplot(diabetes) +
 aes(x = Glucose, y = Age) +
 geom_point(shape = "circle", size = 1.5, colour = "#112446") +
 labs(title = "Rela��o Idade X Glicose") +
 theme_linedraw() +
 theme(plot.title = element_text(face = "italic", 
 hjust = 0.5))
##############################################################################################################
#Gr�fico de dispers�o insulina e espessura da pele 

ggplot(diabetes) +
 aes(x = Insulin, y = SkinThickness) +
 geom_point(shape = "circle", size = 1.95, colour = "#D20808") +
 theme_linedraw()

##############################################################################################################
#remo��o da classe, e constru��o da vari�vel para avalia��o de dist�ncias
diatestes<- diabetes[1:10,1:8]
(diatestes)
dist(diatestes)

##############################################################################################################
#clusteriza�ao baseado no agrupamento das vari�veis
plot(hclust(dist(diatestes)))
plot(hclust(dist (diatestes), method = "centroid"))
k_betes<-kmeans(diatestes,6)
hclust(dist(diatestes), "median")

##############################################################################################################
#plots clusteriza��o
plot(hclust(dist(diabetes)))
plot(hclust(dist (diabetes), method = 'single'))
cl<- kmeans(diabetes,9)
plot(diabetes)
sort(cl$centers)
plot(cl)

#tree decision
##############################################################################################################
diabetes$Outcome<- factor(diabetes$Outcome, levels = c(0,1))
set.seed(1)
divisao<- sample.split(diabetes$Outcome, SplitRatio = 0.75)
treino<- subset(diabetes, divisao ==T)
teste<- subset(diabetes, divisao == F)
classificador<- rpart(formula = Outcome ~., data = diabetes )
print(classificador)
rpart.plot(classificador, type = 3 )
previsao<-predict(classificador, newdata = teste[-9],type = 'class')
previsao
confusao<- table(teste[,9],previsao)
confusionMatrix(confusao)
plot(confusao)


#cmeans
##############################################################################################################
diabetes_cm<- cmeans(diabetes,2,dist = 'euclidean',method = 'cmeans')
diabetes_cm$centers
previsao<- diabetes_cm$cluster
clusplot(diabetes,previsao, color = T, lines = T,labels = 4)
diabetes_cm$membership
table(diabetes$Outcome,previsao)
confusao(diabetes$Outcome,previsao)
confusionMatrix(confusao)
#random Forest
##############################################################################################################
classificador<- randomForest(x = treino, y = treino$Outcome, ntree = 10)
previsao<-predict(classificador, newdata = teste[-9], type = 'class')
confusao<- table(teste[,9],previsao)
confusionMatrix(confusao)
plot(confusao)

class(treino$Outcome)


##############################################################################################################
#dist�ncias
Distancia(diabetes,1) #Distancia euclidiana.
Distancia(diabetes,2) #Distancia euclidiana media.
Distancia(diabetes,3) #Quadrado da distancia euclidiana media
Distancia(diabetes,4) #Distancia euclidiana padronizada
Distancia(diabetes,5) #Distancia euclidiana padronizada media
Distancia(diabetes,6) #Quadrado da distancia euclidiana padronizada media.
Distancia(diabetes,7) #Distancia de Mahalanobis
Distancia(diabetes,8) #Distancia de Cole Rodgers
Distancia(diabetes,9) #Frequencia de coincidencia
Distancia(diabetes,10)#Frequencia de discordancia
Distancia(diabetes,11)#indice Inverso de 1+coincidencia = 1/(1+c) Dados qualitativos binarios 
Distancia(diabetes,12)#Dissimilaridade de Jacard: 1-a/(a+b+c)
Distancia(diabetes,13)#Dissimilaridade de Sorensen Dice: 1-2a/(2a+b+c).
Distancia(diabetes,14)#Dissimilaridade de Sokal e Sneath: 1-2(a+d)/(2(a+d)+b+c)
Distancia(diabetes,15)#Dissimilaridade de Roger e Tanimoto: 1-(a+d)/(a+2(b+c)+d)
Distancia(diabetes,16)#Dissimilaridade de Russel e Rao: 1-a/(a+b+c+d).
Distancia(diabetes,17)#Dissimilaridade de Ochiai: 1-a/sqrt((a+b)(a+c))
Distancia(diabetes,18)#Dissimilaridade de Ochiai II: 1-ab/sqrt((a+b)(a+c)(b+d)(c+d)).
Distancia(diabetes,19)#Dissimilaridade de Haman: 1-((a+d)-(b+c))/(a+b+c+d).
Distancia(diabetes,20)#Dissimilaridade de Yule: 1-(ad-bc)/(ad+bc). Dados mistos
Distancia(diabetes,21)#Dissimilaridade de Gower
Distancia(diabetes,22)#Dissimilaridade de Gower 2

##############################################################################################################
#fuzzy

#UMBRAE = Erro Relativo Relativo Limitado M�dio N�o Escalado
#RMSE = Erro quadr�tico m�dio
#MAPE = Erro de Porcentagem Absoluta M�dia
#MAE =  m�dia aritm�tica dos erros absolutos. 
#MASE = 
#GMRAE = 
#sMAPE = erros sim�tricos na faixa de previs�o assim�trica

fuzzyr.accuracy(diabetes$Insulin,diabetes$SkinThickness)

##############################################################################################################
#gradiente

