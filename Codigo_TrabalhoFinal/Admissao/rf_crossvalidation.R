### Pacotes necessários
#install.packages("e1071")
#install.packages("caret")
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/9 - Admissao - Dados.csv", header = T)
dados$num <- NULL

#View(dados)


#(Ano atual com 4 dígitos + 2 algarismos do dígito verificador do CPF de um dos integrantes)
set.seed(2034)


ind <-createDataPartition(dados$ChanceOfAdmit, p=0.80, list=FALSE)
treino <-dados[ind,]
teste <-dados[-ind,]


##Cross-Validation
control <- trainControl(method='cv', number = 10)


##executa o RF com esse grid

rf <- train(ChanceOfAdmit~.,data = treino, method = "rf", trControl = control)

rf

##Aplica o modelo no arquivo de teste
predict.rf <- predict(rf,teste)

##mostra as métricas

rmse(teste$ChanceOfAdmit, predict.rf)
mae(teste$ChanceOfAdmit, predict.rf)
cor(teste$ChanceOfAdmit, predict.rf, method = "pearson")
Syx <- function(predito, observado, p) {
  return(sqrt(sum((observado - predito)^2) / (length(observado) - p)))
}
Syx(predict.rf,teste$ChanceOfAdmit,ncol(teste) - 1)
r2 <- function(predito, observado){ return (1 - (sum((predito-observado)^2)/sum((observado-mean(observado))^2)))}

r2(predict.rf, teste$ChanceOfAdmit)



### PREDIÇÕES DE NOVOS CASOS
dados_novos_casos <- read.csv("databases/9 - Admissao - novosdados.csv", header = T)
View(dados_novos_casos)

dados_novos_casos$ChanceOfAdmit <- NULL
predict.svm <- predict(svm, dados_novos_casos)
resultado <- cbind(dados_novos_casos, predict.svm)
View(resultado)



