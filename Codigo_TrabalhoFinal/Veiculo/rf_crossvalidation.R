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



set.seed(2034)

ind <-createDataPartition(dados$tipo, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]

##Cross-Validation
control <- trainControl(method='cv', number = 10)


##executa o RF com esse grid

rf <- train(tipo~.,data = treino, method = "rf", trControl = control)

rf

##Aplica o modelo no arquivo de teste
predict.rf <- predict(rf,teste)

##mostra as métricas

confusionMatrix(predict.rf, as.factor(teste$tipo))

