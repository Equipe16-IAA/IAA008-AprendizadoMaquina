### Pacotes necessários
#install.packages("e1071")
#install.packages("caret")
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/5 - Biomassa - Dados.csv", header = T)

#View(dados)


#(Ano atual com 4 dígitos + 2 algarismos do dígito verificador do CPF de um dos integrantes)
set.seed(2034)


ind <-createDataPartition(dados$biomassa, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]

## Prepara um grid com os valores de K que serao usados

tuneGrid <- expand.grid(k=c(1,3,5,7,9, 10))


##executa o KNN com esse grid

knn <- train(biomassa~.,data = treino, method = "knn", tuneGrid=tuneGrid)

knn

##Aplica o modelo no arquivo de teste
predict.knn <- predict(knn,teste)

##mostra as métricas


rmse(teste$biomassa, predict.knn)
mae(teste$biomassa, predict.knn)
cor(teste$biomassa, predict.knn, method = "pearson")
# regressão Syx
Syx <- function(predito, observado, p) {
  return(sqrt(sum((observado - predito)^2) / (length(observado) - p)))
}
Syx(predict.knn,teste$biomassa,ncol(teste) - 1)
r2 <- function(predito, observado){ return (1 - (sum((predito-observado)^2)/sum((observado-mean(observado))^2)))}

r2(predict.knn, teste$biomassa)

