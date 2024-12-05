dados <- read.csv("operacoes.csv")

# ------ Teste de hipótese 2 -------
# H0: proporção de operações com prisão em flagrante é maior ou igual a 30% (p ≥ 0.3)
flagrante <- dados$Qtd.Prisao.em.Flagrante
flagrante_nl <- flagrante[flagrante > 0]
n = length(flagrante)
nl = length(flagrante_nl)
p = nl/n
p0 <- 0.2
z <- (p - p0)/sqrt((p0*(1-p0))/n)
print(paste("Z = ", z))
p_value <- 1 - pnorm(z)
print(paste("valor-p = ", p_value))

# GRAFICO
# Definir os parâmetros
alpha <- 0.05           # Nível de significância
Z_critico <- qnorm(1-alpha) 
Z_observado <- z      # Z calculado

# Criar o intervalo de valores para plotar a curva
x <- seq(-12, 12, length.out = 1000)
y <- dnorm(x)

# Plotar a curva normal
plot(x, y, type = "l", lwd = 2, col = "blue", main = "Distribuição Normal Padrão", 
     xlab = "Z", ylab = "Densidade")

# Adicionar o valor Z calculado à curva
abline(v = Z_observado, col = "red", lwd = 2, lty = 2)  # Linha vertical para o Z observado
text(Z_observado, 0.01, paste("Z observado =", Z_observado), pos = 4)

# Adicionar o Z crítico à curva
abline(v = Z_critico, col = "green", lwd = 2, lty = 2)  # Linha vertical para o Z crítico
text(Z_critico, 0.01, paste("Z crítico =", round(Z_critico, 2)), pos = 4)

# Pintar a região crítica (para Z > Z crítico, por exemplo)
x_critico <- seq(Z_critico, 4, length.out = 500)
y_critico <- dnorm(x_critico)
polygon(c(Z_critico, x_critico, 4), c(0, y_critico, 0), col = rgb(1, 0, 0, 0.3), border = NA)

# Adicionar título
legend("topright", legend = c("Curva Normal", "Z Observado", "Z Crítico", "Região Crítica"),
       col = c("blue", "red", "green", rgb(1, 0, 0, 0.3)), lwd = 2, lty = c(1, 2, 2, 1), fill = c(NA, NA, NA, rgb(1, 0, 0, 0.3)))

