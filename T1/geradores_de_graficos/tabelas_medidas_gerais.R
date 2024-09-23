# Carregar os pacotes necessários
library(gridExtra)
library(ggplot2)

dados <- read.csv("operacoes.csv")

# Função para calcular a moda
calcular_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#-------------- Quantitativas: -----------------------------------------

# ------Quantidade prisões em flagrantes-----

# Selecionar apenas os dados maiores que 0 da variável de quantidades de prisões em flagrante
flagrante <- dados$Qtd.Prisao.em.Flagrante
flagrante_nl <- flagrante[flagrante > 0]

# Calcular medidas com 0:
resumo = summary(flagrante) # Mínimo, 1Q, Mediana, 3Q, Média, Máx
moda = calcular_moda(flagrante) #Moda
var = var(flagrante) #Variância
dp = sd(flagrante) #Desvio padrão
amp = diff(range(flagrante))  # Amplitude (max - min)

# Criar a tabela com todas as medidas com 0's
tabela_medidas <- data.frame(
  Medida = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão", "Amplitude"),
  Valor = c(resumo["Min."], resumo["1st Qu."], resumo["Median"], resumo["Mean"], resumo["3rd Qu."], resumo["Max."], moda, var, dp, amp)
)

# Truncar os valores para 2 casas decimais com arredondamento
tabela_medidas$Valor <- round(tabela_medidas$Valor, 2)

# Gerar e formatar a tabela
tabela_medidas %>%
  kbl(col.names = c("Medida", "Valor"),  # Nomes das colunas
      caption = "Tabela de Medidas Estatísticas contendo valores nulos") %>%  # Legenda da tabela
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

# Calcular medidas SEM 0:
resumo_nl = summary(flagrante_nl) # Mínimo, 1Q, Mediana, 3Q, Média, Máx
moda_nl = calcular_moda(flagrante_nl) #Moda
var_nl = var(flagrante_nl) #Variância
dp_nl = sd(flagrante_nl) #Desvio padrão
amp_nl = diff(range(flagrante_nl))  # Amplitude (max - min)

resumo_nl

# Criar a tabela com todas as medidas SEM 0's
tabela_medidas_nl <- data.frame(
  Medida = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão", "Amplitude"),
  Valor = c(resumo_nl["Min."], resumo_nl["1st Qu."], resumo_nl["Median"], resumo_nl["Mean"], resumo_nl["3rd Qu."], resumo_nl["Max."], moda_nl, var_nl, dp_nl, amp_nl)
)

# Truncar os valores para 2 casas decimais com arredondamento
tabela_medidas_nl$Valor <- round(tabela_medidas_nl$Valor, 2)

# Gerar e formatar a tabela
tabela_medidas_nl %>%
  kbl(col.names = c("Medida", "Valor"),  # Nomes das colunas
      caption = "Tabela de Medidas Estatísticas sem valores nulos") %>%  # Legenda da tabela
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

# -----------Valores Apreendidos -----------

# Definir a exibição de números sem notação científica
options(scipen = 999)

# Transformar os valores da variável Qtd.Valores.Apreendidos para número
valores <- gsub("R\\$|\\.", "", dados$Qtd.Valores.Apreendidos) # Remove "R$" e pontos
valores <- gsub(",", ".", valores) # Substitui a vírgula por ponto
valores <- as.numeric(valores) # Converte para numérico
valores[is.na(valores)] <- 0 # Substitui NA por 0

# Calcular medidas com 0:
resumo = summary(valores) # Mínimo, 1Q, Mediana, 3Q, Média, Máx
moda = calcular_moda(valores) #Moda
var = var(valores) #Variância
dp = sd(valores) #Desvio padrão
amp = diff(range(valores))  # Amplitude (max - min)

# Criar a tabela com todas as medidas com 0's
tabela_medidas <- data.frame(
  Medida = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão", "Amplitude"),
  Valor = c(resumo["Min."], resumo["1st Qu."], resumo["Median"], resumo["Mean"], resumo["3rd Qu."], resumo["Max."], moda, var, dp, amp)
)

# Truncar os valores para 2 casas decimais com arredondamento
#tabela_medidas$Valor <- round(tabela_medidas$Valor, 2)

# Gerar e formatar a tabela
tabela_medidas %>%
  kbl(col.names = c("Medida", "Valor"),  # Nomes das colunas
      caption = "Tabela de Medidas Estatísticas contendo valores nulos") %>%  # Legenda da tabela
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

# Remover os valores NA
valores_nl <- valores[!is.na(valores) & valores != 0]  # Retira todos NA e zeros

# Definir a exibição de números sem notação científica
options(scipen = 999)

# Calcular medidas SEM 0:
resumo_nl = summary(valores_nl) # Mínimo, 1Q, Mediana, 3Q, Média, Máx
moda_nl = calcular_moda(valores_nl) #Moda
var_nl = var(valores_nl) #Variância
dp_nl = sd(valores_nl) #Desvio padrão
amp_nl = diff(range(valores_nl))  # Amplitude (max - min)

# Criar a tabela com todas as medidas SEM 0's
tabela_medidas_nl <- data.frame(
  Medida = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão", "Amplitude"),
  Valor = c(resumo_nl["Min."], resumo_nl["1st Qu."], resumo_nl["Median"], resumo_nl["Mean"], resumo_nl["3rd Qu."], resumo_nl["Max."], moda_nl, var_nl, dp_nl, amp_nl)
)

# Truncar os valores para 2 casas decimais com arredondamento
tabela_medidas_nl$Valor <- round(tabela_medidas_nl$Valor, 2)

# Gerar e formatar a tabela
tabela_medidas_nl %>%
  kbl(col.names = c("Medida", "Valor"),  # Nomes das colunas
      caption = "Tabela de Medidas Estatísticas sem valores nulos e zero") %>%  # Legenda da tabela
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


# ----------Tempo de duração da operação------
# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")
# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")
# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))
# Considerar apenas as "durações positivas" (data do inicio antes da data da deflagração)
duracao_positiva <- dados$Duracao[dados$Duracao > 0]

# Criar data frame para auxiliar na plotagem do gráfico
dados2 <- data.frame(duracao = duracao_positiva)

# Calcular medidas estatísticas
resumo <- summary(duracao_positiva)  # Mínimo, 1Q, Mediana, 3Q, Média, Máximo
moda <- calcular_moda(duracao_positiva)  # Moda
var <- var(duracao_positiva)  # Variância
dp <- sd(duracao_positiva)  # Desvio padrão
amp <- diff(range(duracao_positiva))  # Amplitude (max - min)

# Criar a tabela com todas as medidas SEM 0's
tabela_medidas <- data.frame(
  Medida = c("Mínimo", "1º Quartil", "Mediana", "Média", "3º Quartil", "Máximo", "Moda", "Variância", "Desvio Padrão", "Amplitude"),
  Valor = c(resumo["Min."], resumo["1st Qu."], resumo["Median"], resumo["Mean"], resumo["3rd Qu."], resumo["Max."], moda, var, dp, amp)
)

# Truncar os valores para 2 casas decimais com arredondamento
tabela_medidas$Valor <- round(tabela_medidas$Valor, 2)

# Gerar e formatar a tabela
tabela_medidas %>%
  kbl(col.names = c("Medida", "Valor"),  # Nomes das colunas
      caption = "Tabela de Medidas Estatísticas sem valores nulos e zero") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

