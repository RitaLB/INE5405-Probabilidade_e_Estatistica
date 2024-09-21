dados <- read.csv("~/ufsc/INE5405/trabalho1/operacoes.csv")

# Transformar a coluna para número corretamente
valores <- gsub("R\\$|\\.", "", dados$Qtd.Valores.Apreendidos) # Remove "R$" e pontos
valores <- gsub(",", ".", valores) # Substitui a vírgula por ponto
valores <- as.numeric(valores) # Converte para numérico
valores[is.na(valores)] <- 0
valores
# Criar o histograma

# Definir os intervalos personalizados (breaks)
intervalos <- c(0, 1, 1000, 10000, 100000, 1000000, 10000000, 100000000, Inf)

categorias <- c("0", "1 a mil", "mil a 10 mil", "10 mil a 100 mil", 
                "100 mil a 1 milhão", "1 milhão a 10 milhões", "10 milhões a 100 milhões", "Mais de 100 milhões")

# Aplicar a função cut para classificar os números
classificacao <- cut(valores, breaks = intervalos, labels = categorias, right = FALSE)

# Contar a frequência de cada categoria
frequencias <- table(classificacao)


# Criar o histograma com base nas categorias
barplot(frequencias, main = "Histograma por Intervalos", 
        xlab = "Intervalos", ylab = "Frequência", col = "lightblue", cex.names=0.7)

# tirando o 0 para melhor vizualização dos outros
valores_sem_zero = valores[valores>0]
intervalos_sem_zero = c(1, 1000, 10000, 100000, 1000000, 10000000, 100000000, Inf)
categorias_sem_zero <- c("1 a mil", "mil a 10 mil", "10 mil a 100 mil", 
                "100 mil a 1 milhão", "1 milhão a 10 milhões", "10 milhões a 100 milhões", "Mais de 100 milhões")

# Aplicar a função cut para classificar os números
classificacao_sem_zero <- cut(valores_sem_zero, breaks = intervalos_sem_zero, labels = categorias_sem_zero, right = FALSE)

# Contar a frequência de cada categoria
frequencias_sem_zero <- table(classificacao_sem_zero)


# Criar o histograma com base nas categorias
barplot(frequencias_sem_zero, main = "Histograma por Intervalos", 
        xlab = "Intervalos", ylab = "Frequência", col = "lightblue", cex.names=0.7)


boxplot(valores_sem_zero)

