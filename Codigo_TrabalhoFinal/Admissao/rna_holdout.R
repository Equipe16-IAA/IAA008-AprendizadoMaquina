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


##executa o RNA com esse grid

rna <- train(ChanceOfAdmit~.,data = treino, method = "nnet",trace=FALSE, linout = T)

rna

##Aplica o modelo no arquivo de teste
predict.rna <- predict(rna,teste)

##mostra as métricas

rmse(teste$ChanceOfAdmit, predict.rna)
mae(teste$ChanceOfAdmit, predict.rna)
cor(teste$ChanceOfAdmit, predict.rna, method = "pearson")
Syx <- function(predito, observado, p) {
  return(sqrt(sum((observado - predito)^2) / (length(observado) - p)))
}
Syx(predict.rna,teste$ChanceOfAdmit,ncol(teste) - 1)
r2 <- function(predito, observado){ return (1 - (sum((predito-observado)^2)/sum((observado-mean(observado))^2)))}

r2(predict.rna, teste$ChanceOfAdmit)

