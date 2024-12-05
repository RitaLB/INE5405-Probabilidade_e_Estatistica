dados <- read.csv("operacoes.csv")
library(gridExtra)
library(ggplot2)
library(magrittr)
library(kableExtra)

# Função para calcular a moda
calcular_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

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
  kbl(col.names = c("Medida", "Valor")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

n = nrow(dados2)
print(paste("n = ", n))

media_amostral = resumo["Mean"]
z = 1.96
limite_min = media_amostral - z*(dp/sqrt(n))
limite_max = media_amostral + z*(dp/sqrt(n))

print(paste("Limite mínimo de IC(média, 95%) = ", limite_min))
print(paste("Limite máximo de IC(média, 95%) = ", limite_max))

