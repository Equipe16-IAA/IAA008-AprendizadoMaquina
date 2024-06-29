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

#tuneGrid <- expand.grid(k=c(1,3,5,7,9))


##executa o KNN com esse grid

##Crosvalidation e parametrizacao da RNA

control <- trainControl(method='cv', number = 10)
#tuneGrid <- expand.grid(size = seq(from = 1, to = 3, by = 1), decay=seq(from = 0.1, to 0.7, by = 0.3))

rna <- train(tipo~., data = treino, method="nnet", trainControl = control, linout =T, MaxNWts = 10000, maxit=2000, trafe = F)
rna

##Aplica o modelo no arquivo de teste
predict.rna <- predict(rna,teste)

##mostra as métricas

confusionMatrix(predict.rna, as.factor(teste$tipo))

#rmse(teste$tipo, predict.rna)

#r2 <- function(predito, observado){ return (1 - (sum((predito-observado)^2)/sum((observado-mean(observado))^2)))}

#r2(predict.rna, teste$tipo)




