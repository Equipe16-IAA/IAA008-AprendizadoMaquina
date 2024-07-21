### Pacotes necessários
#install.packages("e1071")
#install.packages("caret")
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/10 - Diabetes - Dados.csv", header = T)
dados$num <- NULL

#View(dados)


#(Ano atual com 4 dígitos + 2 algarismos do dígito verificador do CPF de um dos integrantes)
set.seed(2034)


ind <-createDataPartition(dados$diabetes, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]

##Crosvalidation e parametrizacao da RNA

control <- trainControl(method='cv', number = 10)

rna <- train(diabetes~., data = treino, method="nnet", trainControl = control, trace = F)
rna

##Aplica o modelo no arquivo de teste
predict.rna <- predict(rna,teste)

##mostra as métricas

confusionMatrix(predict.rna, as.factor(teste$diabetes))






