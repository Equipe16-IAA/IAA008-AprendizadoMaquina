### Pacotes necessários
#install.packages("e1071")
#install.packages("caret")
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/6 - Veiculos - Dados.csv", header = T)
dados$a <- NULL

#View(dados)


#(Ano atual com 4 dígitos + 2 algarismos do dígito verificador do CPF de um dos integrantes)
set.seed(2034)

ind <-createDataPartition(dados$tipo, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]


##executa o KNN com esse grid

rna <- train(tipo~.,data = treino, method = "nnet", linout=1, trace=FALSE)

rna

##Aplica o modelo no arquivo de teste
predict.rna <- predict(rna,teste)

##mostra as métricas

confusionMatrix(predict.rna, as.factor(teste$tipo))

