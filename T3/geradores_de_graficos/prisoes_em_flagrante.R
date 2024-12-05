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

# ------Quantidade prisões em flagrantes-----

# Selecionar apenas os dados maiores que 0 da variável de quantidades de prisões em flagrante
flagrante <- dados$Qtd.Prisao.em.Flagrante
flagrante_nl <- flagrante[flagrante > 0]

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

tabela_medidas_nl %>%
  kbl(col.names = c("Medida", "Valor")) %>%  # Legenda da tabela
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


nl = length(flagrante_nl)
print(paste("n = ", nl))

media_amostral = resumo_nl["Mean"]
z = 1.96
limite_minl = media_amostral - z*(dp_nl/sqrt(nl))
limite_maxl = media_amostral + z*(dp_nl/sqrt(nl))

print(paste("Limite mínimo de IC(média, 95%) = ", limite_minl))
print(paste("Limite máximo de IC(média, 95%) = ", limite_maxl))


n = length(flagrante)
p = nl/n
limite_min = p - z*sqrt((p*(1-p))/n)
limite_max = p + z*sqrt((p*(1-p))/n)

print(paste("Proporção = ", p))
print(paste("Limite mínimo de IC(proporção, 95%) = ", limite_min))
print(paste("Limite máximo de IC(proporção, 95%) = ", limite_max))

