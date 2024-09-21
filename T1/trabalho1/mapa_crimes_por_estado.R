library(ggplot2)
library(dplyr)
library(sf)
library(geobr)

# Obtenha os dados dos estados brasileiros
brasil <- geobr::read_state()

# Calcule a frequência de crimes
freq_crimes <- dados %>%
  count(Sigla.Unidade.Federativa, Area) %>%
  rename(frequencia = n)

# Calcule os centros dos estados
centros_estados <- brasil %>%
  st_centroid() %>%
  st_as_sf() %>%
  mutate(ID = abbrev_state)  # Adicione uma coluna para junção

# Combine os centros com a frequência dos crimes
centros_crimes <- centros_estados %>%
  left_join(freq_crimes, by = c("ID" = "Sigla.Unidade.Federativa"))

# Crie o gráfico com pontos coloridos
ggplot() +
  geom_sf(data = brasil, fill = "white", color = "black") +  # Desenhe os estados
  geom_sf(data = centros_crimes, aes(color = Area, size = frequencia), 
          alpha = 0.7) +
  scale_color_viridis_d(na.value = "grey50") +
  theme_minimal() +
  labs(title = "Dispersão dos Tipos de Crime por Estado",
       color = "Tipo de Crime", size = "Frequência")
