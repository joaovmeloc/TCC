###### TCC 2 - SPOTIFY ########

### ANALISE DESCRITIVA 


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

# notas
ggplot(dadosmusicas) +
  geom_bar(aes(x = key), fill = "lightgreen", color = "black") +
  theme_bw()

tabela_notas <- table(dadosmusicas$key)
tabela_notas
# A = LÁ, B = SI, C = DÓ, D = RÉ, E = MI, F = FÁ, G = SOL

# time_signature
ggplot(dadosmusicas) +
  geom_bar(aes(x = time_signature), fill = "lightgreen", color = "black") +
  theme_bw()

tabela_timesig <- table(dadosmusicas$time_signature)
tabela_timesig

# tabela latex
require("xtable")
xtable(tabela_notas)
xtable(tabela_timesig)

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
sd(dadosmusicas$instrumentalidade)


# ANÁLISE EXPLORATÓRIA 


# BOXPLOTS


box_dance <- ggplot(dadosmusicas, aes(x = factor(''), y = dancabilidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Dançabilidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_ener <- ggplot(dadosmusicas, aes(x = factor(''), y = energia)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Energia") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_discurso <- ggplot(dadosmusicas, aes(x = factor(''), y = discurso)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Discurso") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_acustico <- ggplot(dadosmusicas, aes(x = factor(''), y = acustico)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Acústico") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_viva <- ggplot(dadosmusicas, aes(x = factor(''), y = vivacidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Vivacidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_vale<- ggplot(dadosmusicas, aes(x = factor(''), y = valencia)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Valência") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_pop <- ggplot(dadosmusicas, aes(x = factor(''), y = popularidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Popularidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_sono <- ggplot(dadosmusicas, aes(x = factor(''), y = sonoridade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Sonoridade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

box_tempo <- ggplot(dadosmusicas, aes(x = factor(''), y = tempo)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Tempo") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())


box_instru <- ggplot(dadosmusicas, aes(x = factor(''), y = instrumentalidade)) +
  geom_boxplot(fill = c("tomato"), width = 0.5, col= "black", outlier.color = "red") +
  labs(x = "", y = "Instrumentalidade") +
  stat_summary(fun = "mean", geom = "point", shape = 22, size = 3, fill = "white") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())




# agrupando os boxplots


# Carregue as bibliotecas
library(ggplot2)
library(gridExtra)

# Crie uma lista com os dados para cada boxplot
boxplot_data <- list(box_dance, box_ener, box_discurso, 
                     box_acustico, box_viva, box_vale, 
                     box_pop, box_sono, box_tempo, box_instru)

# Crie uma lista vazia para armazenar os gráficos
plots <- list()


# Divida a lista de gráficos em duas sublistas
plots1 <- boxplot_data[1:3]
plots2 <- boxplot_data[4:6]
plots3 <- boxplot_data[7:10]

# Agrupe os boxplots em duas imagens separadas usando grid.arrange
grid.arrange(grobs = plots1, ncol = 2)
grid.arrange(grobs = plots2, ncol = 2)
grid.arrange(grobs = plots3, ncol = 2)






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
  head(10)




musicasmaispopulares %>%
  ggplot(aes(reorder(musica, popularidade),popularidade))+ 
  geom_col(color="black", fill= "lightgreen")+ 
  coord_flip()+ xlab("Nome da música")+ 
  ylab("Popularidade")+ ggtitle("10 Músicas mais populares") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank())

tabela_pop <- pop %>%
  select(musica, artista, popularidade) %>%
  head(20)
tabela_pop


# tabela latex
xtable(tabela_pop)

# correlações

round(cor(dadosmusicas$energia, dadosmusicas$valencia),2)
round(cor(dadosmusicas$energia, dadosmusicas$sonoridade),2)
round(cor(dadosmusicas$energia, dadosmusicas$acustico),2)
round(cor(dadosmusicas$acustico, dadosmusicas$sonoridade),2)

