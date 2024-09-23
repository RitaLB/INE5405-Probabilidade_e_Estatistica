dados <- read.csv("operacoes.csv")

# Seleciona apenas casos onde há atuação em território de fronteira
fronteira <- trimws(dados$Atuacao.em.Territorio.de.Fronteira)
fronteira <- fronteira[fronteira != ""]

# Criar tabela com contagem para países da fronteira
tabela <- table(fronteira)

# Ajustar limite superior do gráfico para aparecer textos
ylim_max <- max(tabela) * 1.1 

bp <- barplot(tabela, col = "blue", main = "Atuação em Território de Fronteira", 
              xlab = "Países de Fronteira", ylab = "Frequência das Operações", ylim = c(0, ylim_max))

# Adicionar os valores das frequências em cima de cada barra
text(bp, tabela + 9, labels = tabela, cex = 0.8, col="red") 
