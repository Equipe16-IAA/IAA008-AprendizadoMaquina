#install.packages("klaR")
library("MASS")
library("klaR")


##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv("databases/6 - Veiculos - Dados.csv", header = T)
dados$a <- NULL

#View(dados)



set.seed(42)


cluster.results <- kmodes(dados, 10, iter.max = 10, weighted = FALSE)

cluster.results