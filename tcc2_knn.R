#### TCC - KNN ####

library(readxl)
library(tidyverse)
library(dplyr)
library(hms)


dados <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/dados_topmusicas.xlsx")

dados <- dados %>% 
  as.data.frame() %>%
  mutate(key = recode(as.character(key),
                      "0" = "C",
                      "1" = "Cs",
                      "2" = "D",
                      "3" = "Ds",
                      "4" = "E",
                      "5" = "F",
                      "6" = "Fs",
                      "7" = "G",
                      "8" = "Gs",
                      "9" = "A",
                      "10" = "As",
                      "11" = "B")) 



dados <- as.data.frame(dados)
head(dados, 10)

view(dados)

#exportando csv

write.csv2(dados, file = "C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/topmusicas.csv", row.names = FALSE)



## importando csv das playlists

playlist1 <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/playlist1.xlsx")
xtable::xtable(playlist1)

playlist2 <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/playlist2.xlsx")
xtable::xtable(playlist2)

playlist3 <- read_xlsx("C:/Users/Joao victor melo/OneDrive/Área de Trabalho/UNB/UnB_2023.1/TCC2/playlist3.xlsx")
xtable::xtable(playlist3)
