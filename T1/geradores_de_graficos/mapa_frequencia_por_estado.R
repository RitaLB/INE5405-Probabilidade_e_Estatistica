library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(geobr)

dados <- read.csv("operacoes.csv")
freq_estados <- dados %>%
  count(Sigla.Unidade.Federativa) %>%
  rename(frequencia = n)

# Obtenha os dados dos estados brasileiros
brasil <- geobr::read_state()
siglas_estados <- data.frame(
  Sigla.Unidade.Federativa = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
                               "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", 
                               "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", 
                               "SE", "SP", "TO"),
  ID = c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", 
         "GO", "MA", "MG", "MS", "MT", "PA", "PB", "PE", 
         "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC", 
         "SE", "SP", "TO")
)
names(brasil)
mapa_frequencia <- brasil %>%
  left_join(freq_estados, by = c("abbrev_state" = "Sigla.Unidade.Federativa"))

ggplot(data = mapa_frequencia) +
  geom_sf(aes(fill = frequencia), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
  theme_minimal() +
  labs(title = "Frequência de Operações por Estado",
       fill = "Frequência") + 
  geom_sf_label(aes(label = frequencia), size = 2.5, color = "black", 
                fill = "white", label.padding = unit(0.2, "lines"),
                nudge_x = 0.1, nudge_y = 0.1)  # Ajuste conforme necessário
