dados <- read.csv("operacoes.csv")

# Criar tabela com contagem para cada tipo de crime
tabela <- table(dados$Area)

# Cores
cores <- rainbow(length(tabela))

# Criar gráfico de pizza
pie(tabela, 
    col = cores, 
    main = "Distribuição por Tipo de Crime",
    radius = 1, 
    cex = 0.7,   # Tamanho do texto dos rótulos
    labels = paste(trimws(names(tabela)),round(100 * prop.table(tabela), 1), "%"),  # Labels de cada setor
    clockwise = TRUE,  # Sentido horário
    init.angle = 90)  # Ângulo inicial


