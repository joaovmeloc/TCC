####### TCC 2 - SPOTIFY ########

## CLUSTERIZAÇÃO 

library(spotifyr)
library(tidyverse)
library(ggridges)
library(httpuv)
library(dplyr)
library(purrr)
library(knitr)
library(lubridate)
library(kableExtra)
library(hms)
library(factoextra)
library(GGally)
library(readxl)
library(ggplot2)
library(wordcloud2)
library(gghighlight)
library(patchwork)

Sys.setenv(SPOTIFY_CLIENT_ID = '3fb839ce36d2433482b7ac7ababfc3d3')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f808d449ad0a450a865063fea40b70d1')

access_token <- get_spotify_access_token()



# importando o banco de dados que foi gerado no Python

dados1 <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/dados_topmusicas.xlsx")


view(dados1)


# modificando a variável "key" (notas) e modificando a variável "duration" para minutos

dadosmusicas <- dados1 %>% 
  as.data.frame() %>%
  mutate(key = recode(as.character(key),
                      "0" = "C",
                      "1" = "C#",
                      "2" = "D",
                      "3" = "D#",
                      "4" = "E",
                      "5" = "F",
                      "6" = "F#",
                      "7" = "G",
                      "8" = "G#",
                      "9" = "A",
                      "10" = "A#",
                      "11" = "B")) %>% 
  mutate(duracao = as_hms(round(duracao / 1000)))



view(dadosmusicas)










## PLAYLISTS

set.seed(2901)
# clusterização kmeans

variaveis <- dadosmusicas %>%
  select(-c(1:6),-key, -duracao, -time_signature) #seleciona variaveis para cluster

dados_kmean <- scale(variaveis) 
dados_kmean

# método de elbow (cotovelo) para definir o número de clusters





library(cluster)

# Calcular a soma dos quadrados internos para diferentes números de clusters
set.seed(3108)
wcss <- vector()
for (i in 1:15) {
  kmeans_model <- kmeans(dados_kmean, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Plotar o gráfico da soma dos quadrados internos em relação ao número de clusters
wss1 <- plot(1:15, wcss, type = "b", pch = 19, frame = FALSE, xlab = "Número de clusters", ylab = "WCSS")

# Determinar o número de clusters com base no método Elbow
diff_wcss <- c(0, diff(wcss))
elbow <- which(diff_wcss < mean(diff_wcss))
elbow
# Adicionar uma linha vertical indicando o número de clusters escolhido pelo método Elbow
wss2 <- abline(v = 5, lty = 5)
# Imprimir o número de clusters escolhido
cat("Número de clusters escolhido pelo método Elbow:", elbow, "\n")  #2


# outro teste

wss <- sapply(1:10,function(k){kmeans(dados_kmean, k, nstart=25,
                                  iter.max = 10)$tot.withinss})


plot(wss)

fviz_nbclust(dados_kmean, kmeans, method = "wss")+
  geom_vline(xintercept = 5, linetype = 2)#Visualiza quantidade de cluster







library(ggplot2)
library(gridExtra)

# Crie uma lista com os dados para cada boxplot
boxplot_data <- list(wss1, wss2)

# Crie uma lista vazia para armazenar os gráficos
plots <- list()


# Divida a lista de gráficos em duas sublistas
plots1 <- boxplot_data[1:2]
plots2 <- boxplot_data[4:6]
plots3 <- boxplot_data[7:10]

# Agrupe os boxplots em duas imagens separadas usando grid.arrange
grid.arrange(grobs = plots1, ncol = 2)
grid.arrange(grobs = plots2, ncol = 2)
grid.arrange(grobs = plots3, ncol = 2)


# testes 

teste_cluster <- kmeans(variaveis, centers = 5) # CLusterizacao
  
fviz_cluster(teste_cluster, variaveis, ellipse.type = "t") +
  theme_bw()#visualizar cluster

cluster <- teste_cluster$cluster

tabela_cluster <- dadosmusicas %>%
  cbind(cluster)
view(tabela_cluster)


playlist1 <- tabela_cluster %>%
  filter(cluster == 1)

playlist2 <- tabela_cluster %>%
  filter(cluster == 2)

playlist3 <- tabela_cluster %>%
  filter(cluster == 3)

playlist4 <- tabela_cluster %>%
  filter(cluster == 4)

playlist5 <- tabela_cluster %>%
  filter(cluster == 5) 

view(playlist1)


## criando playlists

playlist1 <- playlist1 %>%
  select(musica, artista) %>%
  head(10)

playlist2 <- playlist2 %>%
  select(musica, artista) %>%
  head(10)

playlist3 <- playlist3 %>%
  select(musica, artista) %>%
  head(10)

playlist4 <- playlist4 %>%
  select(musica, artista) %>%
  head(10)

playlist5 <- playlist5 %>%
  select(musica, artista) %>%
  head(10)


xtable(playlist5)





# nao ta funcionando

create_playlist("joaovmelo","playlist1") # criando playlists ("usuario", "nome da playlist")


add_tracks_to_playlist("0rQY7V2aXZpZ4A4gNvXkyE", uris = playlist1$id[1:5])

add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[51:100])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[101:150])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[151:200])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[201:250])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[251:300])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[301:351])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[351:400])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[451:500])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[501:550])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[551:600])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[601:650])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[651:660])
























### NUVENS DE PALAVRAS COM OS GENEROS

# playlist1

view(playlist1)

set.seed(2901)


genero1 <- playlist1 %>%
  separate_rows(generos, sep = ",") %>% 
  group_by(generos) %>% count() %>%
  wordcloud2(minSize = 15)

genero1

genero2 <- playlist2 %>%
  separate_rows(generos, sep = ",") %>% 
  group_by(generos) %>% count() %>%
  wordcloud2(minSize = 15)

genero2


genero3 <- playlist3 %>%
  separate_rows(generos, sep = ",") %>% 
  group_by(generos) %>% count() %>%
  wordcloud2(minSize = 15)

genero3

genero4 <- playlist4 %>%
  separate_rows(generos, sep = ",") %>% 
  group_by(generos) %>% count() %>%
  wordcloud2(minSize = 15)

genero4

genero5 <- playlist5 %>%
  separate_rows(generos, sep = ",") %>% 
  group_by(generos) %>% count() %>%
  wordcloud2(minSize = 15)

genero5





















# banco "festa"


festinha <- as.data.frame(playlist_dancabilidade1 %>% inner_join(playlist_energia1))
festinhatop <- as.data.frame(festinha %>% inner_join(playlist_valencia3))


ggplot(festinhatop) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(festinhatop$genero_principal)


create_playlist("joaovmelo","balada") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[1:50])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[51:100])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[101:150])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[151:200])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[201:250])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[251:300])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[301:351])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[351:400])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[401:450])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[451:500])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[501:550])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[551:600])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[601:650])
add_tracks_to_playlist("7dAUFVhcrLCuIaLoZfShSQ", uris = festinhatop$id[651:660])


# testes sonoridade

teste_sonoridade <- kmeans(variaveis$sonoridade, centers = 3) # CLusterizacao

fviz_cluster(teste_sonoridade, variaveis[-c(1:6)], ellipse.type = "t") #visualizar cluster

cluster_sonoridade <- teste_sonoridade$cluster

tabela_sonoridade <- variaveis %>%
  cbind(cluster_sonoridade)


playlist_sonoridade1 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 1) %>%
  arrange(desc(sonoridade))

playlist_sonoridade2 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 2) %>%
  arrange(desc(sonoridade))

playlist_sonoridade3 <- tabela_sonoridade %>%
  select(id,artista, musica, genero_principal, sonoridade, cluster_sonoridade) %>%
  filter(cluster_sonoridade == 3) %>%
  arrange(desc(sonoridade))


# banco "academia" 

academia <- as.data.frame(playlist_sonoridade3 %>% inner_join(playlist_energia1))
academia2 <- as.data.frame(academia %>% inner_join(playlist_dancabilidade1))
academiatop <- as.data.frame(academia2 %>% inner_join(playlist_valencia3))
count(academiatop)

create_playlist("joaovmelo","academia") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[1:50])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[51:100])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[101:150])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[151:200])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[201:250])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[251:300])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[301:351])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[351:400])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[401:450])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[451:500])
add_tracks_to_playlist("2lzvSkzjRLCDUeEjAJpqMO", uris = academiatop$id[501:550])

ggplot(academiatop) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(academiatop$genero_principal)



# banco "triste" 

triste <- as.data.frame(playlist_energia3 %>% inner_join(playlist_dancabilidade3))

triste2 <- as.data.frame(triste %>% inner_join(playlist_valencia1))

tristes <- as.data.frame(triste2 %>% inner_join(playlist_sonoridade2))



ggplot(tristes) +
  geom_bar(aes(x = genero_principal), fill = "tomato", color = "black")

table(tristes$genero_principal)



create_playlist("joaovmelo","sad") # criando playlists ("usuario", "nome da playlist")
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[1:50])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[51:100])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[101:150])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[151:200])
add_tracks_to_playlist("57XVm7Bva3e0iKbkAGjuof", uris = tristes$id[201:226])



# banco dormir/relaxar




















# banco estudos








### NUVENS DE PALAVRAS COM OS GENEROS

# genero festa

gen_festa <- get_playlist_audio_features("balada", "7dAUFVhcrLCuIaLoZfShSQ") %>% 
  unnest(track.artists) %>% # Variavel a principio como lista; 
  distinct(track.id, .keep_all = TRUE) %>% 
  select(added_at, track.id, track.name, artist.id = id, artist.name = name, 
         track.album.name, track.album.release_date, track.album.release_date_precision, 
         track.popularity, danceability, energy, loudness, speechiness, acousticness, 
         instrumentalness, liveness, valence, tempo) %>% 
  rowwise() %>%
  mutate(genres = str_flatten(get_artist(artist.id)$genres, collapse = ","), 
         artist.popularity = get_artist(artist.id)$popularity)

gen_festa %>% separate_rows(genres, sep = ",") %>% 
  group_by(genres) %>% count() %>%
  wordcloud2(minSize = 10)





# genero triste

gen_triste <- get_playlist_audio_features("sad", "57XVm7Bva3e0iKbkAGjuof") %>% 
  unnest(track.artists) %>% # Variavel a principio como lista; 
  distinct(track.id, .keep_all = TRUE) %>% 
  select(added_at, track.id, track.name, artist.id = id, artist.name = name, 
         track.album.name, track.album.release_date, track.album.release_date_precision, 
         track.popularity, danceability, energy, loudness, speechiness, acousticness, 
         instrumentalness, liveness, valence, tempo) %>% 
  rowwise() %>%
  mutate(genres = str_flatten(get_artist(artist.id)$genres, collapse = ","), 
         artist.popularity = get_artist(artist.id)$popularity)

gen_triste %>% separate_rows(genres, sep = ",") %>% 
  group_by(genres) %>% count() %>%
  wordcloud2(minSize = 10)





# genero academia

gen_acad <- get_playlist_audio_features("academia", "2lzvSkzjRLCDUeEjAJpqMO") %>% 
  unnest(track.artists) %>% # Variavel a principio como lista; 
  distinct(track.id, .keep_all = TRUE) %>% 
  select(added_at, track.id, track.name, artist.id = id, artist.name = name, 
         track.album.name, track.album.release_date, track.album.release_date_precision, 
         track.popularity, danceability, energy, loudness, speechiness, acousticness, 
         instrumentalness, liveness, valence, tempo) %>% 
  rowwise() %>%
  mutate(genres = str_flatten(get_artist(artist.id)$genres, collapse = ","), 
         artist.popularity = get_artist(artist.id)$popularity)

gen_acad %>% separate_rows(genres, sep = ",") %>% 
  group_by(genres) %>% count() %>%
  wordcloud2(minSize = 10)