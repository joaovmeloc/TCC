###### TCC 2 #######

# pacotes utilizados

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

Sys.setenv(SPOTIFY_CLIENT_ID = 'f223560d206544d28c09bae26f0c1517')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '806a29f4658b43cf8ac07296ddcbb9cf')

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


ggplot(dadosmusicas) +
  geom_bar(aes(x = key), fill = "tomato", color = "black") +
  theme_bw()

table(tracks$key)



# medidas resumo


summary(dadosmusicas)

sd(dadosmusicas$dancabilidade)
sd(dadosmusicas$energia)
sd(dadosmusicas$discurso)
sd(dadosmusicas$acustico)
sd(dadosmusicas$vivacidade)
sd(dadosmusicas$valencia)
sd(dadosmusicas$popularidade)
sd(dadosmusicas$sonoridade)
sd(dadosmusicas$tempo)



# ANÁLISE EXPLORATÓRIA 


# BOXPLOTS


ggplot(dadosmusicas, aes(x = factor(''), y = dancabilidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Dançabilidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = energia)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Energia") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = discurso)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Discurso") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = acustico)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Acústico") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = vivacidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Vivacidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = valencia)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Valência") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = popularidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Popularidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = sonoridade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Sonoridade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

ggplot(dadosmusicas, aes(x = factor(''), y = tempo)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Tempo") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())




## MATRIZ DE CORRELAÇÃO

library(corrplot)


dados_cor <- dadosmusicas %>% 
  select(popularidade, acustico, discurso, vivacidade, instrumentalidade,
         energia, tempo, sonoridade, dancabilidade, valencia)



corrplot(cor(dados_cor, method = "pearson"))

matriz_corr <- cor(dados_cor)

corrplot(matriz_corr,method="square",tl.srt=45, 
         title = "Nível de correlação entre as variáveis", 
         mar = c(2,2,2,2), tl.col = "red",diag = F)

ggcorr(dados_cor, label=T)






# correlações

cor(dadosmusicas$energia, dadosmusicas$valencia)
cor(dadosmusicas$energia, dadosmusicas$sonoridade)
cor(dadosmusicas$energia, dadosmusicas$acustico)
cor(dadosmusicas$acustico, dadosmusicas$sonoridade)

# Analisando Popularidade 

pop <- dadosmusicas %>%
  select(musica, artista, popularidade, generos, genero_principal) %>%
  arrange(desc(popularidade)) %>%
  head(30)


pop2 <- dadosmusicas %>%
  select(musica, artista, popularidade) %>%
  arrange(popularidade) %>%
  head(30)


## gráfico top10


musicasmaispopulares <- pop %>%
  select(musica, artista, popularidade,generos) %>%
  head(20)


musicasmaispopulares %>%
  ggplot(aes(reorder(musica, popularidade),popularidade))+ 
  geom_col(color="black", fill= "cyan")+ 
  coord_flip()+ xlab("Nome da música")+ 
  ylab("Popularidade")+ ggtitle("20 Músicas mais famosas ") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())



musicasmenosfamosas <- pop2 %>%
  arrange(popularidade) %>%
  head(20)


musicasmenosfamosas %>%
  ggplot(aes(reorder(musica, popularidade),popularidade))+ 
  geom_col(color="black", fill= "cyan")+ 
  coord_flip()+ xlab("Nome da música")+ 
  ylab("Popularidade")+ ggtitle("20 Músicas menos famosas ")






## PLAYLISTS

set.seed(123)
# clusterização kmeans

variaveis <- dadosmusicas %>%
  select(-key, -duracao, -time_signature) #seleciona variaveis para cluster

dados_kmean <- scale(variaveis[-c(1:6)]) 


fviz_nbclust(dados_kmean, kmeans, method = "gap_stat")+
  geom_vline(xintercept = 3, linetype = 2)#Visualiza quantidade de cluster


# testes energia

teste_energia <- kmeans(variaveis$energia, centers = 3) # CLusterizacao

fviz_cluster(teste_energia, variaveis[-c(1:6)], ellipse.type = "t") +
  theme_bw()#visualizar cluster

cluster_energia <- teste_energia$cluster

tabela_energia <- variaveis %>%
  cbind(cluster_energia)


playlist_energia1 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 1) %>%
  arrange(desc(energia))

playlist_energia2 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 2) %>%
  arrange(desc(energia))

playlist_energia3 <- tabela_energia %>%
  select(id,artista, musica, genero_principal, energia, cluster_energia) %>%
  filter(cluster_energia == 3) %>%
  arrange(desc(energia))



# testes dancabilidade

teste_dancabilidade <- kmeans(variaveis$dancabilidade, centers = 3) # CLusterizacao

fviz_cluster(teste_dancabilidade, variaveis[-c(1:6)], ellipse.type = "t") +
  theme_bw()#visualizar cluster

cluster_dancabilidade <- teste_dancabilidade$cluster

tabela_dancabilidade <- variaveis %>%
  cbind(cluster_dancabilidade)


playlist_dancabilidade1 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 1) %>%
  arrange(desc(dancabilidade))

playlist_dancabilidade2 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 2) %>%
  arrange(desc(dancabilidade))

playlist_dancabilidade3 <- tabela_dancabilidade %>%
  select(id,artista, musica, genero_principal, dancabilidade, cluster_dancabilidade) %>%
  filter(cluster_dancabilidade == 3) %>%
  arrange(desc(dancabilidade))




# testes valencia

teste_valencia <- kmeans(variaveis$valencia, centers = 3) # CLusterizacao

fviz_cluster(teste_valencia, variaveis[-c(1:6)], ellipse.type = "t") +
  theme_bw()#visualizar cluster

cluster_valencia <- teste_valencia$cluster

tabela_valencia <- variaveis %>%
  cbind(cluster_valencia)


playlist_valencia1 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 1) %>%
  arrange(desc(valencia))

playlist_valencia2 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 2) %>%
  arrange(desc(valencia))

playlist_valencia3 <- tabela_valencia %>%
  select(id,artista, musica, genero_principal, valencia, cluster_valencia) %>%
  filter(cluster_valencia == 3) %>%
  arrange(desc(valencia))


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





#### ADIVINHAR POPULARIDADE (CATEGORIAS) QUE AS MUSICAS ESTAO INSERIDAS A PARTIR DAS OUTRAS VARIAVEIS.

# categorias

dadosmusicas$popularidade

niveis <- seq(-0.1, 100, length.out = 11)

categorias <- cut(dadosmusicas$popularidade, breaks = niveis, labels = FALSE)


dadosmusicas <- cbind(dadosmusicas, Categorias = categorias)

view(dadosmusicas)



## previsao

# modelo Random Forest


library(randomForest)

# selecionando apenas as variaveis de caracterizam as musicas
dadosmusicas2 <- dadosmusicas %>%
  select(-c(1:8))


view(dadosmusicas2)

# Dividir o banco de dados em conjunto de treinamento e conjunto de teste
set.seed(1234)  # Definir a semente para reprodutibilidade
indices_treinamento <- sample(1:nrow(dadosmusicas2), 0.7 * nrow(dadosmusicas2))  # 70% dos dados para treinamento
conjunto_treinamento <- dadosmusicas2[indices_treinamento, ]
conjunto_teste <- dadosmusicas2[-indices_treinamento, ]


# Criar o modelo de Random Forest
modelo_rf <- randomForest(popularidade ~ ., data = conjunto_treinamento)

# Fazer previsões para o conjunto de teste
previsoes_rf <- predict(modelo_rf, newdata = conjunto_teste)

# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_rf)

# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse <- mean(squared_diff)

# Exibição do resultado
print(mse)



view(conjunto_treinamento)
view(conjunto_teste)





# Verificar se a variável possui NAs

has_na <- any(is.na(conjunto_treinamento$popularidade))


if (has_na) {
  print("A variável possui NAs.")
} else {
  print("A variável não possui NAs.")
}





### modelo SVM (Suport Vector Machine)

library(e1071)

modelo_svm <- svm(popularidade ~ ., data = conjunto_treinamento, kernel = "radial")

# previsoes
previsoes_svm <- predict(modelo_svm, newdata = conjunto_teste)

acuracia_svm <- sum(previsoes == conjunto_teste$popularidade) / nrow(conjunto_teste)

view(conjunto_teste)
view(conjunto_treinamento)





# comparando previsoes com as categorias


previsao_rf <- as.data.frame(previsoes_rf)
previsao_svm <- as.data.frame(previsoes_svm)

# random forest
dadosmusicas3 <- cbind(conjunto_teste, previsao_rf = previsao_rf)
view(dadosmusicas3)
view(conjunto_teste)


niveis <- seq(-0.1, 100, length.out = 11)

categorias_rf <- cut(dadosmusicas3$previsoes_rf, breaks = niveis, labels = FALSE)

dadosmusicas4 <- cbind(dadosmusicas3, Categorias_rf = categorias_rf)
view(dadosmusicas4)



# svm 
dadosmusicas5 <- cbind(dadosmusicas4, previsao_svm = previsao_svm)
view(dadosmusicas5)

categorias_svm <- cut(dadosmusicas5$previsoes_svm, breaks = niveis, labels = FALSE)

dadosmusicas6 <- cbind(dadosmusicas5, Categorias_svm = categorias_svm)
view(dadosmusicas6)








##### realizando um novo teste com uma variável categórica. 

dadosmusicas2


# Definir os intervalos e os rótulos das categorias
intervalos <- c(0, 20, 40, 60, 80, 100)
categorias <- c("Desconhecida", "Baixa", "Média", "Alta", "Muito Alta")

# Aplicar a transformação
categorias_popularidade <- cut(dadosmusicas2$popularidade, breaks = intervalos, labels = categorias, include.lowest = TRUE)

# Exibir os resultados
print(categorias_popularidade)



dadosmusicas2 <- cbind(dadosmusicas2, Categorias = categorias_popularidade)
view(dadosmusicas2)

## aplicando random forest

# Dividir os dados em conjunto de treinamento e teste
set.seed(423)  # Define a semente aleatória para reprodução dos resultados
train_indices <- sample(1:nrow(dadosmusicas2), 0.8 * nrow(dadosmusicas2))  # 80% dos dados para treinamento
train_data <- dadosmusicas2[train_indices, ]
test_data <- dadosmusicas2[-train_indices, ]

# Criar o modelo de Random Forest
rf_model <- randomForest(Categorias ~ ., data = train_data, ntree = 100)

# Fazer previsões no conjunto de teste
predictions <- predict(rf_model, test_data)

# Calcular a acurácia das previsões
accuracy <- sum(predictions == test_data$Species) / nrow(test_data)
print(paste("Acurácia do modelo:", accuracy))


predictions_rf <- as.data.frame(predictions)

test_data <- cbind(test_data, previsoes_rf = predictions_rf)
view(test_data)

