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

# Criar o gráfico de barras
barplot(frequencia_meses, main="Frequência de Operações por Mês", xlab="Mês", ylab="Frequência", col="lightblue")


#################### TIPO DE CRIME ####################
tabela <- table(dados$Area)
# Ajustar layout para evitar sobreposição
cores <- rainbow(length(tabela))
# Ajustar layout para evitar sobreposição
#par(mar = c(5, 4, 4, 8))  # Aumenta a margem à direita

# Criar gráfico de pizza
pie(tabela, 
    col = cores, 
    main = "Distribuição por Área",  # Título do gráfico
    radius = 1,  # Ajusta o raio do gráfico de pizza
    cex = 0.7,   # Ajusta o tamanho do texto dos rótulos
    labels = paste(trimws(names(tabela)),round(100 * prop.table(tabela), 1), "%"),  # Adiciona porcentagem
    clockwise = TRUE,  # Faz a pizza girar no sentido horário
    init.angle = 90)  # Define o ângulo inicial para começar o gráfico

# Crie uma tabela com a contagem por Área
tabela <- table(dados$Area)


