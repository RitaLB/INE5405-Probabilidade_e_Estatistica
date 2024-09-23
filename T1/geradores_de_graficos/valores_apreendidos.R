dados <- read.csv("operacoes.csv")

# Transformar os valores da variável Qtd.Valores.Apreendidos para número
valores <- gsub("R\\$|\\.", "", dados$Qtd.Valores.Apreendidos) # Remove "R$" e pontos
valores <- gsub(",", ".", valores) # Substitui a vírgula por ponto
valores <- as.numeric(valores) # Converte para numérico
valores <- valores[!is.na(valores)]  # Retira todos NA

# Plotar gráfico de caixas
boxplot(valores, main = "Valores Apreendidos em cada Operação",
        ylab = "Valores Apreendidos em reais")

# Plotar gráfico de caixas com foco entre 5% e 87% dos dados para melhor vizualização
boxplot(valores, main = "Valores Apreendidos em cada Operação",
        ylab = "Valores Apreendidos em reais",
        ylim = quantile(valores, probs = c(0.05, 0.87)))
