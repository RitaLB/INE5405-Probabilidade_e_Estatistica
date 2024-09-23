dados <- read.csv("operacoes.csv")

# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")

# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")

# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))

# Filtrando os dados para manter apenas durações "positivas" (data de início antes da deflagração)
dados_filtrados <- dados[dados$Duracao > 0, ]

# Filtrando os dados para manter apenas quando há alguma prisão em flagrante
dados_filtrados <- dados_filtrados[dados_filtrados$Qtd.Prisao.em.Flagrante > 0, ]

# Plotar gráfico
plot(dados_filtrados$Duracao, dados_filtrados$Qtd.Prisao.em.Flagrante,
     main = "Comparação entre Duração das Operações em Dias e Quantidade de Prisões em Flagrante",
     xlab = "Duração em dias",
     ylab = "Quantidade de Prisões em Flagrante")
