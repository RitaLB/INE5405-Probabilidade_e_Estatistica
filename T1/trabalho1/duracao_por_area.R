library(tidyr)

dados <- read.csv("operacoes.csv")

# Converter a coluna "Data da Deflagração" para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")
# Supondo que a coluna com a data de início se chame 'Data.de.Inicio'
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")

# Calcular a diferença em dias
dados$Dias_Entre <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))
dados$Dias_Entre
#dias_maior_que_zero <- dados$Dias_Entre[dados$Dias_Entre>0]
dados$Area <- trimws(dados$Area)
#dados$Area <- gsub("Crimes Ambientais e Contra o Patrimônio Cultural", "Cr. Ambientais e Patrimônio Cultural", dados$Area)

# Filtrando os dados para manter apenas Dias_Entre > 0
dados_filtrados <- dados[dados$Dias_Entre > 0, ]

############VARIAVEIS P USAR##############
dados_filtrados$Dias_Entre
dados_filtrados$Area

# Criar o gráfico de caixas com as categorias em Area e os valores em Dias_Entre
grafico <- ggplot(dados_filtrados, aes(x = factor(Area), y = Dias_Entre)) +
  geom_boxplot(aes(fill = factor(Area))) +  # Gráficos de caixa com preenchimento para cada área
  theme_minimal() +
  labs(title = "Distribuição dos Dias Entre por Área",
       x = "Área",
       y = "Dias Entre") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotacionar os labels no eixo X

# Exibir o gráfico
print(grafico)