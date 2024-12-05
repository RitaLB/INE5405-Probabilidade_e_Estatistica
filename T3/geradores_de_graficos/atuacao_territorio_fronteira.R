dados <- read.csv("operacoes.csv")

# Cria tabela que contém "sim" para onde tem atuação em território de fronteira e "não" para onde não tem
atuacao_fronteira <-table(ifelse(trimws(dados$Atuacao.em.Territorio.de.Fronteira) == "", "não", "sim"))

qtd_sim <- atuacao_fronteira["sim"]
qtd_total <- sum(atuacao_fronteira)

# Proporção de operações atuando em território de fronteira
p = qtd_sim/qtd_total

z = 1.96
limite_min = p - z*sqrt((p*(1-p))/qtd_total)
limite_max = p + z*sqrt((p*(1-p))/qtd_total)

print(paste("Proporção = ", p))
print(paste("Limite mínimo de IC(proporção, 95%) = ", limite_min))
print(paste("Limite máximo de IC(proporção, 95%) = ", limite_max))

