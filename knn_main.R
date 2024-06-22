### Pacotes necessários
#install.packages("e1071")
#install.packages("caret")
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/6 - Veiculos - Dados.csv", header = T)
dados$a <- NULL

View(dados)



set.seed(42)

ind <-createDataPartition(dados$tipo, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]

## Prepara um grid com os valores de K que serao usados

tuneGrid <- expand.grid(k=c(1,3,5,7,9))


##executa o KNN com esse grid

knn <- train(tipo~.,data = treino, method = "knn", tuneGrid = tuneGrid)

knn
