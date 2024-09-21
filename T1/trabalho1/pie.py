import matplotlib.pyplot as plt
import pandas as pd

# Carregar os dados de um arquivo CSV
# Substitua o caminho do arquivo com o seu arquivo CSV
dados = pd.read_csv('operacoes.csv')

# Contar a frequência de cada valor na coluna 'Area'
tabela = dados['Area'].value_counts()

# Criar o gráfico de pizza
fig, ax = plt.subplots()

# Definir uma paleta de cores
cores = plt.cm.Paired(range(len(tabela)))

# Criação do gráfico de pizza
wedges, texts = ax.pie(tabela, 
                       colors=cores, 
                       startangle=90, 
                       wedgeprops=dict(width=0.3),  # Fatias mais finas
                       counterclock=False)

# Adicionar legendas ao lado das fatias
ax.legend(wedges, 
          tabela.index,
          title="Áreas",
          loc="center left",
          bbox_to_anchor=(1, 0, 0.5, 1))

# Adicionar título ao gráfico
ax.set_title("Distribuição por Área")

# Mostrar o gráfico
plt.show()
