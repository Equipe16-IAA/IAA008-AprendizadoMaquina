### Pacotes necessários
#install.packages('arules', dep=T)
library(arules)
library(datasets)

##Leitura da database
setwd("D:/GIT/IAA008-AprendizadoMaquina/")

dados <- read.csv2("databases/2 - Musculacao - Dados.csv", header = F)

View(dados)
summary(dados)

set.seed(2034)

rules <- apriori(dados, parameter = list(supp = 0.001, conf = 0.7, minlen = 2))
summary(rules)


options(digits=2)

inspect(sort(rules, by="confidence"))
write.csv(inspect(sort(rules, by="confidence")),"apriori.csv")
