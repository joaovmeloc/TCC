
import pandas as pd
import numpy as np
import random
import scipy
import seaborn as sns
import streamlit as st
import pandas as pd
import spotipy
import spotipy.util as util
import pandas as pd
import openpyxl
import matplotlib.pyplot as plt
import itertools
import ast

st.set_page_config(page_title="App Spotify")


with st.container():
    st.title(":green[Your Spotify Liked Songs] :headphones:")
    st.subheader("Spotipy + Streamlit")
    

@st.cache_data
def carregar_dados(un,ci,cs):
    token = util.prompt_for_user_token(un,
                                    "user-library-read",
                                    client_id=ci,#'d74e71368e0347638883fcd5dc70d6ff',
                                    client_secret=cs,#'12d9fccf2f11414fa9c4d5c188873453',
                                    redirect_uri='http://localhost:8080/callback')

    spotipy_obj = spotipy.Spotify(auth=token)
    saved_tracks_resp = spotipy_obj.current_user_saved_tracks(limit=50, offset=0)
    number_of_tracks = saved_tracks_resp['total']
    #print('%d tracks' % number_of_tracks)

    def save_only_some_fields(track_response):
        
        album_uri =  track_response['track']['album']['uri']
        main_artist_uri = track_response["track"]["artists"][0]["uri"]

        return {
            'ID': str(track_response['track']['id']),
            'Track': str(track_response['track']['name']),
            'Album': str(track_response['track']['album']['name']),
            'Main Artist': track_response["track"]["artists"][0]["name"],
            'Genres': spotipy_obj.artist(main_artist_uri)['genres'],
            'Release Date': track_response['track']['album']['release_date'],
            'Duration (ms)': track_response['track']['duration_ms'],
            'Popularity': track_response['track']['popularity'],
            'Added at': track_response['added_at']
        }
        
    data_dict = [save_only_some_fields(track) for track in saved_tracks_resp['items']]

    while saved_tracks_resp['next']:
            saved_tracks_resp = spotipy_obj.next(saved_tracks_resp)
            data_dict.extend([save_only_some_fields(track) for track in saved_tracks_resp['items']])

    data = pd.DataFrame(data_dict)

    audio_features = {}

    for idd in data['ID'].tolist():
        audio_features[idd] = spotipy_obj.audio_features(idd)[0]

    data['Acousticness'] = data['ID'].apply(lambda idd: audio_features[idd]['acousticness'])
    data['Speechiness'] = data['ID'].apply(lambda idd: audio_features[idd]['speechiness'])
    data['Key'] = data['ID'].apply(lambda idd: str(audio_features[idd]['key']))
    data['Liveness'] = data['ID'].apply(lambda idd: audio_features[idd]['liveness'])
    data['Instrumentalness'] = data['ID'].apply(lambda idd: audio_features[idd]['instrumentalness'])
    data['Energy'] = data['ID'].apply(lambda idd: audio_features[idd]['energy'])
    data['Time'] = data['ID'].apply(lambda idd: audio_features[idd]['tempo'])
    data['Time Signature'] = data['ID'].apply(lambda idd: audio_features[idd]['time_signature'])
    data['Loudness'] = data['ID'].apply(lambda idd: audio_features[idd]['loudness'])
    data['Danceability'] = data['ID'].apply(lambda idd: audio_features[idd]['danceability'])
    data['Valence'] = data['ID'].apply(lambda idd: audio_features[idd]['valence'])
    data['Duration (ms)'] = data['ID'].apply(lambda idd: audio_features[idd]['duration_ms'])

    

    ## pivot wider nos generos
    genero = data.explode("Genres")  #retirando os generos de dentro da lista
    crosstab = pd.crosstab(genero["Track"], genero["Genres"]) # fazendo os generos como dummies

    ## puxando a tabela pivotada pra geral
    crosstab['Track']=crosstab.index
    crosstab.index = range(0,len(crosstab['Track']))
    data_full = data.merge(crosstab,how="left",on="Track")

    ## criando a variável dos que nao tem gênero
    data_full['No Genre']= 0
    for i in range(0,len(data_full)):
        if data_full['Genres'][i] == []:
            data_full['No Genre'][i]= 1

    ## substituindo os NaN por 0
    data = data_full.fillna(0)
    data = data.dropna()
    data["Added at"] = data["Added at"].str.split("T", expand=True)[0]
    return data

with st.container():

    st.write("---")
    user = st.text_input('Username', 'moreirapupe')
    c_id = st.text_input('Client ID', 'd74e71368e0347638883fcd5dc70d6ff')
    c_secret = st.text_input('Key Secret','12d9fccf2f11414fa9c4d5c188873453')

    st.subheader("Liked Songs List!")

    try:
        dados = carregar_dados(un=user,ci=c_id,cs=c_secret)
    except:
        dados = pd.DataFrame({"Error":["Ocorreu algum erro na consulta"]})
    st.dataframe(dados.iloc[:,0:20],
                     column_config = {
                         "Track": st.column_config.Column(
                             "Track", help = "Faixa Musical"
                         ),
                         "Album": st.column_config.Column(
                             "Album", help = "Álbum"
                         ),
                         "Main Artist": st.column_config.Column(
                             "Main Artist", help = "Artista Principal"
                         ),
                         "Genres": st.column_config.Column(
                             "Genres", help = "Gêneros"
                         ),
                         "Release Date": st.column_config.Column(
                             "Release Date", help = "Data de Lançamento"
                         ),
                         "Duration (ms)": st.column_config.Column(
                             "Duration (ms)", help = "Duração (ms)"
                         ),
                         "Popularity": st.column_config.Column(
                             "Popularity", help = "Nível de Popularidade"
                         ),
                         "Added at": st.column_config.Column(
                             "Added at", help = "Adicionado em"
                         ),
                         "Acousticness": st.column_config.Column(
                             "Acousticness", help = "Nível de Acústico"
                         ),
                         "Speechiness": st.column_config.Column(
                             "Speechiness", help = "Nível de Fala"
                         ),
                         "Key": st.column_config.Column(
                             "Key", help = "Chave"
                         ),
                         "Liveness": st.column_config.Column(
                             "Liveness", help = "Nível de Vivacidade"
                         ),
                         "Instrumentalness": st.column_config.Column(
                             "Instrumentalness", help = "Nível de Instrumentalidade"
                         ),
                         "Energy": st.column_config.Column(
                             "Energy", help = "Nível de Energia"
                         ),
                         "Time": st.column_config.Column(
                             "Time", help = "Tempo"
                         ),
                         "Time Signature": st.column_config.Column(
                             "Time Signature", help = "Tempo de Assinatura"
                         ),
                         "Loudness": st.column_config.Column(
                             "Loudness", help = "Nível de Sonoridade"
                         ),
                         "Danceability": st.column_config.Column(
                             "Danceability", help = "Nível de Dançabilidade"
                         ),
                         "Valence": st.column_config.Column(
                             "Valence", help = "Valência"
                         )

                     },
                     hide_index=True)

with st.container():

    st.write("---") 

    st.title("Statistical Analyzes :bar_chart:")

    dados_teste = dados.iloc[:,0:20]

    # Inserindo gráfico
    if st.button("Metrics Table"):

        st.dataframe(dados_teste.describe())


with st.container():

    st.write("---") 

    st.title("Songs Added")

    tab1, tab2 = st.tabs(["By Year", "By Month"])

    with tab1:

        # > Transformando colunas
        dados_teste2 = dados_teste
        dados_teste2["Added at"] = pd.to_datetime(dados_teste2["Added at"])


        # 2) Analises por tempo

        # Por ano:

        # > Criando a coluna ano
        dados_teste2["Year add"] = dados_teste2["Added at"].dt.year
        dados_teste2["Year add"] = dados_teste2["Year add"].astype(dtype = "object")
        dados_teste2['ID'] = dados_teste2["ID"].astype(dtype = 'object')

        h = pd.DataFrame(dados_teste2.groupby("Year add")['ID'].count())
        h['Year add'] = h.index
        h.columns = ["Frequency", "Year"]

        st.bar_chart(h, x = 'Year', y = 'Frequency')
        st.line_chart(h, x = 'Year', y = 'Frequency')

    with tab2:

        # Por mês:
        dados_teste2["Month add"] = dados_teste2["Added at"].dt.month_name()
        dados_teste2["Month num"] = dados_teste2["Added at"].dt.month

        ano_select = st.selectbox("Selecione o Ano:", list(dados_teste2['Year add'].unique()))
        ano = int(ano_select)

        j = dados_teste2[dados_teste2['Year add'] == ano]
        j = j.iloc[:,[0,21,22]]
        j = j.sort_values(by = "Month num")
        j.columns = ["Frequency", "Month", "Num"]
        h = j.groupby(["Num"])['Frequency'].count()

        k = j.groupby(["Num", "Month"])['Frequency'].count()
        k
        
        st.bar_chart(h)
        st.line_chart(h)

with st.container():

    st.write("---") 

    st.title("Analysis of Musical Genres")

    banco_completo = dados

    st.subheader("In General and Time")

    banco_completo["Added at"] = pd.to_datetime(banco_completo["Added at"])
    banco_completo["Year add"] = banco_completo["Added at"].dt.year
    banco_completo["Year add"] = banco_completo["Year add"].astype(dtype = "object")

    lista_ano = list(banco_completo['Year add'].unique())
    lista_ano.append("Todos")

    selec_ano = st.selectbox("Escolha o Ano:", lista_ano)
    ano_selecionado = selec_ano

    if ano_selecionado == "Todos":
        banco_completo = banco_completo
    if ano_selecionado != "Todos":
        banco_completo = banco_completo[banco_completo['Year add'] == int(ano_selecionado)]
    
    lista = list(banco_completo['Genres'])

    
    lista_resultante = [elemento for sublista in lista for elemento in sublista]

    b = pd.DataFrame(lista_resultante)
    b.columns = ["Gêneros"]
    m = pd.DataFrame(b.value_counts())
    m.columns = ["Frequency"]
    m['Gêneros'] = m.index
    m = m.iloc[[0,1,2,3,4], :]
    m

    st.bar_chart(m, x = 'Gêneros', y = 'Frequency')


    banco_generos = pd.DataFrame(lista_resultante)
    banco_generos.columns = ["Gêneros"]

    lista_genero = list(banco_generos['Gêneros'].unique())
    lista_genero.append("Todos")

    genre_select = st.selectbox("Selecione o Gênero:", lista_genero)
    genre = str(genre_select)

    if genre == "Todos" and ano_selecionado == "Todos":
        texto1 = f"O número de músicas curtidas no geral é: {str(len(banco_completo))}"
    if genre == "Todos" and ano_selecionado != "Todos":
        texto1 = f"O número de músicas curtidas no ano {ano_selecionado} é: {str(len(banco_completo))}"
    if genre != "Todos" and ano_selecionado == "Todos":
        texto1 = f"O número de músicas curtidas do gênero {genre} em todos os anos é: {str(lista_resultante.count(genre))}" 
    if genre != "Todos" and ano_selecionado != "Todos":
        texto1 = f"O número de músicas curtidas do gênero {genre} no ano {ano_selecionado} é: {str(lista_resultante.count(genre))}"       
        
    st.subheader(texto1) 

    banco_nogenre = banco_completo[banco_completo['No Genre'] == 1]
    texto2 = f"O número de músicas curtidas sem gênero é: {str(len(banco_nogenre['No Genre']))}"
    st.subheader(texto2)
