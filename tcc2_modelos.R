###### TCC 2 - SPOTIFY ########

## MODELOS 

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

# banco de dados apenas com variáveis numéricas

dados_mod <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/dados_topmusicas.xlsx")

dados_mod <- dados_mod %>%
  select(-c(1:8))

dados_mod <- as.data.frame(dados_mod)
view(dados_mod)

# analise de componentes principais 

## matriz de correlação

library(corrplot)

matriz_corr <- cor(dados_mod)
view(matriz_corr)


corrplot(matriz_corr,method="square",tl.srt=45, 
         title = "Nível de correlação entre as variáveis", 
         mar = c(2,2,2,2), tl.col = "red",diag = F)

#### estudar melhor depois
# pca 

pca_mod <- prcomp(matriz_corr)
summary(pca_mod)


# Obtendo as cargas dos componentes principais
loadings <- pca_mod$rotation

# Extraindo as variáveis importantes
important_vars_cor <- which(abs(loadings[,1]) > 0.5275
                            | abs(loadings[,2]) > 0.5275
                            | abs(loadings[,3]) > 0.5275
                            | abs(loadings[,4]) > 0.5275
                            | abs(loadings[,5]) > 0.5275)

important_vars_cor





## previsao

# modelo Random Forest

dados_imporvar <- dados_mod %>%
  select("popularidade", "vivacidade", "dancabilidade", "instrumentalidade", "tempo")
view(dados_imporvar)

library(randomForest)


# Dividir o banco de dados em conjunto de treinamento e conjunto de teste
set.seed(068)  # Definir a semente para reprodutibilidade
indices_treinamento <- sample(1:nrow(dados_imporvar), 0.7 * nrow(dados_imporvar))  # 70% dos dados para treinamento
conjunto_treinamento <- dados_imporvar[indices_treinamento, ]
conjunto_teste <- dados_imporvar[-indices_treinamento, ]


# Criar o modelo de Random Forest
modelo_rf_imporvar <- randomForest(popularidade ~ ., data = conjunto_treinamento)

# Fazer previsões para o conjunto de teste
previsoes_rf_imporvar <- predict(modelo_rf_imporvar, newdata = conjunto_teste)


view(conjunto_teste)

# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_rf_imporvar)

# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse_rf_imporvar_ <- mean(squared_diff)

# Exibição do resultado
print(mse_rf_imporvar)




## utilizando todas as variaveis 


## previsao

# modelo Random Forest

library(randomForest)


# Dividir o banco de dados em conjunto de treinamento e conjunto de teste
set.seed(0687)  # Definir a semente para reprodutibilidade
indices_treinamento <- sample(1:nrow(dados_mod), 0.7 * nrow(dados_mod))  # 70% dos dados para treinamento
conjunto_treinamento <- dados_mod[indices_treinamento, ]
conjunto_teste <- dados_mod[-indices_treinamento, ]

view(conjunto_treinamento)


# Criar o modelo de Random Forest
modelo_rf <- randomForest(popularidade ~ ., data = conjunto_treinamento)

conjunto_teste <- conjunto_teste %>%
  select(-1)
# Fazer previsões para o conjunto de teste
previsoes_rf <- predict(modelo_rf, newdata = conjunto_teste)


view(conjunto_teste)

accuracy <- sum(predictions == test_data$Categorias) / nrow(test_data)

# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_rf)
class(dados_mod$popularidade)
# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse_rf <- mean(squared_diff)

# Exibição do resultado
print(mse_rf)


### modelo SVM (Suport Vector Machine)

library(e1071)

modelo_svm <- svm(popularidade ~ ., data = conjunto_treinamento, kernel = "radial")

# previsoes
previsoes_svm <- predict(modelo_svm, newdata = conjunto_teste)


# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_svm)

# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse_svm <- mean(squared_diff)

# Exibição do resultado
print(mse_svm)







###### TESTE mais adequado 
### utilizar categorias nominais ou utilizar categorias de 0 a 10?
##### realizando um novo teste com uma variável categórica. 




# Definir os intervalos e os rótulos das categorias
intervalos <- c(0, 20, 40, 60, 80, 100)
categorias <- c("Desconhecida", "Baixa", "Média", "Alta", "Muito Alta")

# Aplicar a transformação
categorias_popularidade <- cut(dados_mod$popularidade, breaks = intervalos, labels = categorias, include.lowest = TRUE)

# Exibir os resultados
print(categorias_popularidade)



dados_mod2 <- cbind(dados_mod, Categorias = categorias_popularidade)
view(dados_mod2)

## aplicando random forest

# Dividir os dados em conjunto de treinamento e teste
set.seed(423)  # Define a semente aleatória para reprodução dos resultados
train_indices <- sample(1:nrow(dados_mod2), 0.8 * nrow(v))  # 80% dos dados para treinamento
train_data <- dados_mod2[train_indices, ]
test_data <- dados_mod2[-train_indices, ]

# Criar o modelo de Random Forest
rf_model <- randomForest(Categorias ~ ., data = train_data, ntree = 100)

# Fazer previsões no conjunto de teste
predictions <- predict(rf_model, test_data)

view(test_data)

# Calcular a acurácia das previsões
accuracy <- sum(predictions == test_data$Categorias) / nrow(test_data)
print(paste("Acurácia do modelo:", accuracy))


predictions_rf <- as.data.frame(predictions)

test_data <- cbind(test_data, previsoes_rf = predictions_rf)
view(test_data)




### CATEGORIAS DE 1 A 10 


dados_mod$popularidade

niveis <- seq(-0.1, 100, length.out = 11)

categorias <- cut(dadosmusicas$popularidade, breaks = niveis, labels = FALSE)


dados_mod3 <- cbind(dados_mod, Categorias = categorias)

view(dados_mod3)



#Dividir o banco de dados em conjunto de treinamento e conjunto de teste
set.seed(4321)  # Definir a semente para reprodutibilidade
indices_treinamento <- sample(1:nrow(dados_mod3), 0.7 * nrow(dados_mod3))  # 70% dos dados para treinamento
conjunto_treinamento <- dados_mod3[indices_treinamento, ]
conjunto_teste <- dados_mod3[-indices_treinamento, ]


# Criar o modelo de Random Forest
modelo_rf <- randomForest(popularidade ~ ., data = conjunto_treinamento)

# Fazer previsões para o conjunto de teste
previsoes_rf <- round(predict(modelo_rf, newdata = conjunto_teste))

view(conjunto_teste)

# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_rf)

# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse_rf <- mean(squared_diff)

# Exibição do resultado
print(mse_rf)  #10.15699 (variável resposta "popularidade")




### modelo SVM (Suport Vector Machine)

library(e1071)

modelo_svm <- svm(popularidade ~ ., data = conjunto_treinamento, kernel = "radial")

# previsoes
previsoes_svm <- round(predict(modelo_svm, newdata = conjunto_teste))


# Dados de exemplo
df <- data.frame(y = conjunto_teste$popularidade,
                 y_hat = previsoes_svm)

# Cálculo do erro quadrático médio
diff <- df$y - df$y_hat
squared_diff <- diff^2
mse_svm <- mean(squared_diff)

# Exibição do resultado
print(mse_svm)  #12.63369 (variável resposta "popularidade")






# comparando previsoes com as categorias


previsao_rf <- as.data.frame(previsoes_rf)
previsao_svm <- as.data.frame(previsoes_svm)

# random forest
dados_mod_rf <- cbind(conjunto_teste, previsao_rf = previsao_rf)
view(dados_mod_rf)
view(conjunto_teste)


niveis <- seq(-0.1, 100, length.out = 11)

categorias_rf <- cut(dados_mod_rf$previsoes_rf, breaks = niveis, labels = FALSE)

dados_mod_rf <- cbind(dados_mod_rf, Categorias_rf = categorias_rf)
view(dados_mod_rf)



# svm 
dados_mod_svm <- cbind(conjunto_teste, previsao_svm = previsao_svm)
view(dados_mod_svm)

categorias_svm <- cut(dados_mod_svm$previsoes_svm, breaks = niveis, labels = FALSE)

dados_mod_svm <- cbind(dados_mod_svm, Categorias_svm = categorias_svm)
view(dados_mod_svm)



