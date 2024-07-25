### Pacotes necessários
library("caret")  
library(Metrics)



##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/6 - Veiculos - Dados.csv", header = T)
dados$a <- NULL

View(dados)


#(Ano atual com 4 dígitos + 2 algarismos do dígito verificador do CPF de um dos integrantes)
set.seed(2034)

km.res = kmeans(dados[, 0:(ncol(dados) - 1)],10)
print(km.res)



table(km.res$cluster, dados$tipo)
write.csv(table(km.res$cluster, dados$tipo), "kmeans.csv")

resultado <- cbind(dados,km.res$cluster)
resultado

