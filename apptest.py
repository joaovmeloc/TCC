import streamlit as st
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.neighbors import NearestNeighbors
import pickle 


st.set_page_config(page_title="Playlists Spotify")

with st.container():

    st.title(":green[Lista de músicas - Spotify] :headphones:")
    st.subheader("Spotipy + Streamlit")
    st.write("Informações e características das músicas mais ouvidas do Spotify entre os anos de 2014 e 2022")
    
with st.container():

    st.write("---")
    dados = pd.read_csv("https://raw.githubusercontent.com/joaovmeloc/TCC/main/topmusicas.csv", sep = ";")

    options = st.multiselect(
    'Selecione um gênero musical',
    list(dados['genero_principal'].unique()),
    ['Rock'])

    st.write('You selected:', options)

    dados2 = dados[dados['genero_principal'] == options[0]]


    st.dataframe(dados2)
    st.write("Na tabela acima contém as informações de todas as 8165 músicas presentes no banco de dados. Está ordenada em ordem alfabética pelo nome das músicas.")

    # colocar um botão para filtrar as musicas por genero 

with st.container():
    
    st.write("---")
    st.write("Para a realização da montagem das playlists, é necessário que o usário escolha uma música da tabela acima.")

    # Colocar um botão para o usuário inserir o nome da música.

    title = st.text_input('Selecione e insira uma faixa', 'ASTROTHUNDER')
    st.write('A faixa selecionada foi:', title)


with st.container():

    st.write("---")
    st.subheader("PLAYLIST")
    st.write("A playlist abaixo contém as 30 músicas mais semelhantes do banco de dados, de acordo com a música que foi inserida no campo acima. A junção foi realizada utilizando a técnica estatística K-nearest Neighbors")

    # Variáveis Dummies
    dados['time_signature'] = dados['time_signature'].astype(str)
    ddummies = pd.get_dummies(dados[['genero_principal','key', 'popularidade', 'acustico', 'discurso',
                     'vivacidade', 'instrumentalidade', 'energia', 'tempo', 'time_signature', 'sonoridade', 'dancabilidade', 'valencia']])


    # padronização
    scaler = StandardScaler()
    dadospadronizados = pd.DataFrame(scaler.fit_transform(ddummies), columns=ddummies.columns)

    # juntando as variaveis dummies com os nomes das musicas e dos artistas
    dados2 = dados['musica']
    musicas = pd.concat([dados2, dadospadronizados], axis=1)
    musicas.set_index(dados.musica,inplace=True)
    musicas.drop(columns='musica',inplace=True)
    

    # modelo
    modelo = NearestNeighbors(n_neighbors=30).fit(musicas)
    semelhantes = modelo.kneighbors(np.array(musicas.loc["ASTROTHUNDER"]).reshape(1, -1),return_distance=False)[0]

    #playlist
    playlist = dados.iloc[semelhantes]
    st.dataframe(playlist)