dados <- read.csv("~/ufsc/INE5405/trabalho1/operacoes.csv")
names(dados)
hist(dados$Qtd.Mandado.de.Busca.e.Apreesao)
hist(dados$Qtd.Prisao.em.Flagrante)
hist(dados$Qtd.Prisao.Preventiva)
hist(dados$Qtd.Prisao.Temporaria)

flagrante <- dados$Qtd.Prisao.em.Flagrante
flagrante <- flagrante[flagrante>0]
dados$Qtd.Prisao.em.Flagrante <- as.numeric(dados$Qtd.Prisao.em.Flagrante)
dados$Qtd.Prisao.em.Flagrante
boxplot(dados$Qtd.Prisao.em.Flagrante~dados$Area)

library(ggplot2)
ggplot(dados, aes(x = trimws(Area), y = Qtd.Prisao.em.Flagrante + Qtd.Prisao.Preventiva + Qtd.Prisao.Temporaria)) +
  geom_boxplot() +
  labs(title = "Distribuição de Prisões em Flagrante por Área", x = "Área", y = "Quantidade de Prisões") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dados, aes(x = trimws(Area), y = Qtd.Mandado.de.Busca.e.Apreesao)) +
  geom_boxplot() +
  labs(title = "Distribuição de Prisões em Flagrante por Área", x = "Área", y = "Quantidade de Prisões") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
