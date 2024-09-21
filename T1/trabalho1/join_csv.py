import pandas as pd
import os

# Definir o diretório onde estão os arquivos CSV
caminho_diretorio = 'arquivos'

# Obter a lista de todos os arquivos CSV no diretório
arquivos_csv = [f for f in os.listdir(caminho_diretorio) if f.endswith('.csv')]

# Inicializar uma lista para armazenar os dataframes
dataframes = []

# Ler e adicionar cada arquivo CSV à lista de dataframes
for arquivo in arquivos_csv:
    caminho_completo = os.path.join(caminho_diretorio, arquivo)
    # Ler o arquivo com a codificação e delimitador corretos
    df = pd.read_csv(caminho_completo, encoding='ISO-8859-1', delimiter=';')
    dataframes.append(df)

# Concatenar todos os dataframes em um só
df_final = pd.concat(dataframes)

# Salvar o dataframe final em um novo arquivo CSV
df_final.to_csv('operacoes.csv', index=False, sep=',')
