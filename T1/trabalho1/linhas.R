# Ler os dados
dados <- read.csv("~/ufsc/INE5405/trabalho1/operacoes.csv")

# Converter a coluna "Data da Deflagração" para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")
# Extrair o número do mês
dados$Mes <- format(dados$Data.da.Deflagracao, "%m")

# Definir os labels dos meses
labels_meses <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", 
                  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

# Converter a coluna Mes para um fator com os labels correspondentes
dados$Mes <- factor(dados$Mes, levels = sprintf("%02d", 1:12), labels = labels_meses)

# Contar a frequência de ocorrências por mês
frequencia_meses <- table(dados$Mes)

# Converter a tabela de frequência para um vetor para o gráfico
frequencia_vector <- as.numeric(frequencia_meses)

# Criar o gráfico de linhas
plot(frequencia_vector, type="o", main="Frequência de Operações por Mês", 
     xlab="Mês", ylab="Frequência", col="blue", xaxt="n", ylim=c(0, max(frequencia_vector) + 1))

# Adicionar os labels dos meses no eixo X
axis(1, at=1:12, labels=labels_meses)

# Adicionar a linha
lines(frequencia_vector, type="o", col="blue")

