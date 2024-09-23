library(ggplot2)
dados <- read.csv("operacoes.csv")

# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")

# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")

# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))

# Tirar espaços em branco após os tipos de crimes
dados$Area <- trimws(dados$Area)
# Substituir string grande por abreviação para caber na visualização do gráfico
dados$Area <- gsub("Crimes Ambientais e Contra o Patrimônio Cultural", "Cr. Ambientais e Patrimônio Cultural", dados$Area)

# Filtrar os dados para manter apenas durações "positivas" (data do início antes da deflagração)
dados_filtrados <- dados[dados$Duracao > 0, ]

# Plotar gráfico de violino para cada área
ggplot(dados_filtrados, aes(x = Area, y = Duracao, fill = Area)) +
  geom_violin(color = "black", width = 1) +
  labs(x = "Tipo de crime", y = "Duração em dias", 
    title = "Comparação entre Duração das Operações e Tipo de Crime") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  theme(legend.position = "none")  # Remove a legenda
