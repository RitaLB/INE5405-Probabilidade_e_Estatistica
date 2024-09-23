# Ler os dados
dados <- read.csv("operacoes.csv")

# Selecionar apenas os dados maiores que 0 da variável de quantidades de prisões em flagrante
flagrante <- dados$Qtd.Prisao.em.Flagrante
flagrante <- flagrante[flagrante > 0]

# Contar a frequência de cada valor
frequencia <- table(flagrante)

# Criar um data frame a partir da tabela
dados <- data.frame(Valores = as.numeric(names(frequencia)), Frequencia = as.vector(frequencia))

# Criar um data frame com todos os valores de 1 a 24
todos_valores <- data.frame(Valores = 1:24)

# Incluir valores ausentes com frequência 0
dados_completos <- merge(todos_valores, dados, by = "Valores", all.x = TRUE)
# Substituir NA por 0
dados_completos[is.na(dados_completos$Frequencia), "Frequencia"] <- 0 

# Ajuste do limite superior do eixo y para aparecer textos
ylim_max <- max(dados_completos$Frequencia) * 1.1

# Plotar o gráfico de linhas
plot(dados_completos$Valores, dados_completos$Frequencia, type = "o",
     xlab = "Quantidade de Prisões em Flagrante",
     ylab = "Frequência",
     main = "Frequência da Quantidade de Prisões em Flagrante em cada Operação",
     xaxt = "n",
     ylim = c(0, ylim_max))
grid()

# Adicionar valores do eixo x
axis(1, at = seq(1, 24, by = 1))

# Adicionar rótulos com as frequências
text(dados_completos$Valores, dados_completos$Frequencia, labels = dados_completos$Frequencia, pos = 3, cex = 0.8, col = "red")
