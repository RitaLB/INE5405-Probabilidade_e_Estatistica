dados <- read.csv("operacoes.csv")

# ------ Teste de hipótese 1 -------
# Hipotese: média de valores apreendidos nos crimes fazendários é maior que a média dos valores apreendidos nos outros crimes
fazendarios <- dados$Qtd.Valores.Apreendidos[trimws(dados$Area) == "Crimes Fazendários"]
nao_fazendarios <- dados$Qtd.Valores.Apreendidos[trimws(dados$Area) != "Crimes Fazendários"]
fazendarios <- gsub("R\\$|\\.", "", fazendarios) # Remove "R$" e pontos
fazendarios <- gsub(",", ".", fazendarios) # Substitui a vírgula por ponto
fazendarios <- as.numeric(fazendarios) # Converte para numérico
fazendarios[is.na(fazendarios)] <- 0 # Substitui NA por 0

nao_fazendarios <- gsub("R\\$|\\.", "", nao_fazendarios) # Remove "R$" e pontos
nao_fazendarios <- gsub(",", ".", nao_fazendarios) # Substitui a vírgula por ponto
nao_fazendarios <- as.numeric(nao_fazendarios) # Converte para numérico
nao_fazendarios[is.na(nao_fazendarios)] <- 0 # Substitui NA por 0

# H0: média de valores apreendidos nos crimes fazendários < média dos valores apreendidos nos outros crimes
m_f <- mean(fazendarios)
m_nf <- mean(nao_fazendarios)
somatorio_f <- sum((fazendarios-m_f)**2)
somatorio_nf <- sum((nao_fazendarios-m_nf)**2)
n_f <- length(fazendarios)
n_nf <- length(nao_fazendarios)
s2 <- (somatorio_f + somatorio_nf)/(n_f+n_nf-2)
teste <- (m_f - m_nf)/sqrt(s2*(1/n_f + 1/n_nf))
print(paste("Z = ", teste))
p_value <- pnorm(teste)
print(paste("valor-p = ", p_value))
m_f
m_nf

# GRAFICO
# Definir os parâmetros
alpha <- 0.05           # Nível de significância
Z_critico <- qnorm(1-alpha) 
Z_observado <- teste      # Z calculado

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
#text(Z_critico, 0.01, paste("Z crítico =", round(Z_critico, 2)), pos = 4)

# Pintar a região crítica (para Z > Z crítico, por exemplo)
x_critico <- seq(Z_critico, 4, length.out = 500)
y_critico <- dnorm(x_critico)
polygon(c(Z_critico, x_critico, 4), c(0, y_critico, 0), col = rgb(1, 0, 0, 0.3), border = NA)

# Adicionar título
legend("topright", legend = c("Curva Normal", "Z Observado", "Z Crítico", "Região Crítica"),
       col = c("blue", "red", "green", rgb(1, 0, 0, 0.3)), lwd = 2, lty = c(1, 2, 2, 1), fill = c(NA, NA, NA, rgb(1, 0, 0, 0.3)))

