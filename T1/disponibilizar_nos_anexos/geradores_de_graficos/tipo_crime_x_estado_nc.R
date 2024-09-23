library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)

dados <- read.csv("operacoes.csv")
# Calcule a frequência de crimes
freq_crimes <- dados %>%
  count(Sigla.Unidade.Federativa, Area) %>%
  rename(frequencia = n)

# Definindo uma paleta personalizada com mais cores
custom_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#FF7F00", "#FFFF33", 
                   "#A65628", "#984EA3", "#999999", "#D9D9D9", "#FFBB78",
                   "#F46D43", "#D73027", "#313695")  # Novas cores adicionadas

grafico <- ggplot(freq_crimes, aes(x = Sigla.Unidade.Federativa, y = frequencia, fill = Area)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  labs(title = "Frequência de Crimes por Estado e Tipo",
       x = "Estado",
       y = "Frequência",
       fill = "Tipo de Crime") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exibir o gráfico
print(grafico)

# Salve o gráfico
ggsave("grafico_frequencia_crimes.png", plot = grafico, width = 12, height = 6)
