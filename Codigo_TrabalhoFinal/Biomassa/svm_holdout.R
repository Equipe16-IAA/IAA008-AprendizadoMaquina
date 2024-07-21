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


##executa o SVM com esse grid

svm <- train(biomassa~.,data = treino, method = "svmRadial")

svm

##Aplica o modelo no arquivo de teste
predict.svm <- predict(svm,teste)

##mostra as métricas

rmse(teste$biomassa, predict.svm)
mae(teste$biomassa, predict.svm)
cor(teste$biomassa, predict.svm, method = "pearson")

# regressão Syx
Syx <- function(predito, observado, p) {
  return(sqrt(sum((observado - predito)^2) / (length(observado) - p)))
}
Syx(predict.svm,teste$biomassa,ncol(teste) - 1)

r2 <- function(predito, observado){ return (1 - (sum((predito-observado)^2)/sum((observado-mean(observado))^2)))}

r2(predict.svm, teste$biomassa)


