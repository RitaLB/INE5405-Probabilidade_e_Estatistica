library(ggplot2)
dados <- read.csv("operacoes.csv")

# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")

# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")

# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))

# Considerar apenas as "durações positivas" (data do inicio antes da data da deflagração)
duracao_positiva <- dados$Duracao[dados$Duracao>0]

# Criar data frame para auxiliar na plotagem do gráfico
dados2 <- data.frame(duracao = duracao_positiva)

# Plotar gráfico de violino
ggplot(dados2, aes(x = "", y = duracao)) +
  geom_violin(fill = "blue", color = "blue") +
  labs(x = "", y = "Duração em dias", 
       title = "Duração das Operações da Polícia Federal (Dias entre Início e Deflagração)") +
  theme_minimal()
