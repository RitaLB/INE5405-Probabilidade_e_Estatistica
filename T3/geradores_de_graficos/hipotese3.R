dados <- read.csv("operacoes.csv")

# ----------Tempo de duração da operação------
# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")
# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")
# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))
# Considerar apenas as "durações positivas" (data do inicio antes da data da deflagração)
duracao_positiva <- dados$Duracao[dados$Duracao > 0]

# ----------Prisões em flagrante------
flagrante <- dados$Qtd.Prisao.em.Flagrante

# ----------Teste de correlação------
# Filtrar os dados relevantes (quantidade de prisões e duração)
flagrante <- dados$Qtd.Prisao.em.Flagrante
duracao <- dados$Duracao

# Remover valores NA para garantir consistência
cor_data <- na.omit(data.frame(flagrante, duracao))

# Teste de correlação de Pearson
cor_test <- cor.test( cor_data$duracao, cor_data$flagrante, method = "pearson")

# Exibir os resultados
print(cor_test)

# Criar o gráfico de dispersão com ggplot2
ggplot(cor_data, aes(x = duracao, y = flagrante)) +
  geom_point() +  # Plota os pontos
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Linha de regressão (opcional)
  labs(title = "Correlação entre Prisões em Flagrante e Duração da Operação",
       y = "Quantidade de Prisões em Flagrante",
       x = "Duração da Operação (dias)") +
  theme_minimal()

# GRAFICO
t <- cor_test$statistic
p_value <- cor_test$p.value
# Definir os parâmetros
alpha <- 0.05           # Nível de significância
T_critico <- qnorm(1-alpha) 
T_observado <- t     # Z calculado

# Criar o intervalo de valores para plotar a curva
x <- seq(-12, 12, length.out = 1000)
y <- dnorm(x)

# Plotar a curva normal
plot(x, y, type = "l", lwd = 2, col = "blue", main = "Distribuição Normal Padrão", 
     xlab = "T", ylab = "Densidade")

# Adicionar o valor Z calculado à curva
abline(v = T_observado, col = "red", lwd = 2, lty = 2)  # Linha vertical para o Z observado
text(T_observado, 0.01, paste("To =", T_observado), pos = 2)

# Adicionar o Z crítico à curva
abline(v = T_critico, col = "green", lwd = 2, lty = 2)  # Linha vertical para o Z crítico
text(T_critico, 0.01, paste("Tc =", round(T_critico, 2)), pos = 4)

# Pintar a região crítica (para Z > Z crítico, por exemplo)
x_critico <- seq(T_critico, 4, length.out = 500)
y_critico <- dnorm(x_critico)
polygon(c(T_critico, x_critico, 4), c(0, y_critico, 0), col = rgb(1, 0, 0, 0.3), border = NA)

# Adicionar título
legend("topright", legend = c("Curva Normal", "T Observado", "T Crítico", "Região Crítica"),
       col = c("blue", "red", "green", rgb(1, 0, 0, 0.3)), lwd = 2, lty = c(1, 2, 2, 1), fill = c(NA, NA, NA, rgb(1, 0, 0, 0.3)))
