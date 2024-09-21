dados <- read.csv("~/ufsc/INE5405/trabalho1/operacoes.csv")


# Criar o gráfico de barras
barplot(table(dados$Sigla.Unidade.Federativa), main="Operações por estado", xlab="Estado", ylab="Operações", col="lightblue")
