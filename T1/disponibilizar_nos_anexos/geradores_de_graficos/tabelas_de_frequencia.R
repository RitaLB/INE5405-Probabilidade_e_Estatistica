library(kableExtra)
dados <- read.csv("operacoes.csv")

#### TABELA DE FREQUÊNCIA DA QUANTIDADE DE PRISÕES EM FLAGRANTE ####
# Calcular a frequência absoluta
frequencia_abs <- table(dados$Qtd.Prisao.em.Flagrante)

# Calcular a frequência relativa
frequencia_rel <- prop.table(frequencia_abs)

# Criar um data frame com a tabela de frequência
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),                          
  Frequencia_Absoluta = as.vector(frequencia_abs),        
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2))

# Gerar tabela
tabela_frequencia %>%
  kbl(col.names = c("Quantidade de Prisões em Flagrante", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência da Quantidade de Prisões em Flagrante") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")

#### TABELA DE FREQUÊNCIA DOS VALORES APREENDIDOS ####
# Transformar a coluna de valores apreendidos para números
valores <- gsub("R\\$|\\.", "", dados$Qtd.Valores.Apreendidos) # Remove "R$" e pontos
valores <- gsub(",", ".", valores) # Substitui a vírgula por ponto
valores <- as.numeric(valores) # Converte para numérico
valores[is.na(valores)] <- 0 # Substitui NA por 0

# Definir os intervalos
intervalos <- c(0, 1, 1000, 10000, 100000, 1000000, 10000000, 100000000, Inf)

# Categorias dos intervalos
categorias <- c("0", "1 a mil", "mil a 10 mil", "10 mil a 100 mil", 
                "100 mil a 1 milhão", "1 milhão a 10 milhões", "10 milhões a 100 milhões", "Mais de 100 milhões")

# Aplicar a função cut para classificar os números
classificacao <- cut(valores, breaks = intervalos, labels = categorias, right = FALSE)

frequencia_abs <- table(classificacao)
frequencia_rel <- prop.table(frequencia_abs)
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),                      
  Frequencia_Absoluta = as.vector(frequencia_abs),           
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2))

tabela_frequencia %>%
  kbl(col.names = c("Classes de Valores Apreendidos em R$", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência dos Valores Apreendidos") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


#### TABELA DE FREQUÊNCIA DA DURAÇÃO ####
# Converter a coluna Data.da.Deflagração para o formato de data
dados$Data.da.Deflagracao <- as.Date(dados$Data.da.Deflagracao, format="%d/%m/%Y")

# Converter a coluna Data.do.Inicio para o formato de data
dados$Data.do.Inicio <- as.Date(dados$Data.do.Inicio, format="%d/%m/%Y")

# Calcular a diferença em dias
dados$Duracao <- as.numeric(difftime(dados$Data.da.Deflagracao, dados$Data.do.Inicio, units = "days"))

# Filtrar apenas durações "positivas" (data do inicio antes da data da deflagração)
duracao <- dados$Duracao[dados$Duracao>0]

# Definir os intervalos
intervalos <- c(1, 30, 90, 365, 730, 1095, 1460, 1825, Inf)

# Categorias dos intervalos
categorias <- c("Até 1 mês", "Até 1 trimestre", "Até 1 ano", "Até 2 anos", 
                "Até 3 anos", "Até 4 anos", "Até 5 anos", "Mais de 5 anos")

# Aplicar a função cut para classificar os números
classificacao <- cut(duracao, breaks = intervalos, labels = categorias, right = FALSE)

frequencia_abs <- table(classificacao)
frequencia_rel <- prop.table(frequencia_abs)
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),                                # Agora são strings
  Frequencia_Absoluta = as.vector(frequencia_abs),              # Frequência absoluta
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2)  # Frequência relativa em porcentagem
)

tabela_frequencia %>%
  kbl(col.names = c("Tempo de duração", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência da Duração da Operação") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


#### TABELA DE FREQUÊNCIA DO TIPO DE CRIME ####
frequencia_abs <- table(trimws(dados$Area))
frequencia_rel <- prop.table(frequencia_abs)
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),              
  Frequencia_Absoluta = as.vector(frequencia_abs),          
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2))

tabela_frequencia %>%
  kbl(col.names = c("Tipo de Crime", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência do Tipo de Crime") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


#### TABELA DE FREQUÊNCIA DO LOCAL (ESTADO) DA OPERAÇÃO ####
frequencia_abs <- table(dados$Sigla.Unidade.Federativa)
frequencia_rel <- prop.table(frequencia_abs)
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),                         
  Frequencia_Absoluta = as.vector(frequencia_abs),          
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2))

tabela_frequencia %>%
  kbl(col.names = c("Estados", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência de Operações por Estado") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")


#### TABELA DE FREQUÊNCIA DA ATUAÇÃO EM TERRITÓRIO DE FRONTEIRA ####
# Calcular a frequência absoluta
frequencia_abs <- table(dados$Atuacao.em.Territorio.de.Fronteira[trimws(dados$Atuacao.em.Territorio.de.Fronteira) != ""])
frequencia_rel <- prop.table(frequencia_abs)
tabela_frequencia <- data.frame(
  Valor = names(frequencia_abs),                            
  Frequencia_Absoluta = as.vector(frequencia_abs),           
  Frequencia_Relativa = round(100 * as.vector(frequencia_rel), 2))

tabela_frequencia %>%
  kbl(col.names = c("Países", "Frequência Absoluta", "Frequência Relativa (%)"),
      caption = "Tabela de Frequência de Operações por Área de Fronteira") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center")
