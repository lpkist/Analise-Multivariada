# Caso seja necessário instalar os pacotes, utilizar
# install.packages("tidyverse")
# install.packages("aplpack")
# install.packages("cluster")

# Se der erro de enconding, basta abrir utilizando encoding UTF-8

# Importando os pacotes
library(tidyverse)
library(aplpack)
library(cluster)

# Lendo os dados
dados <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ-8Bn5E6kzkIii4cAsuWQYsqLuDk1WGMwg0yk65aPbIHSjY4q7ivPTqG7qwR5ydWgsrgLWD93W17Xw/pub?output=csv")
dados <- dados %>% select(-c(Ash, Ash_Alcanity))

# Tipos das colunas
apply(dados, 2, class) # todos numéricos
summary(dados)

# Determinação do número k de agrupamentos
pams <- list()
for(i in 2:(nrow(dados)-1)){
  pam <- pam(dados, i)
  pams[[i-1]] <- c(i, pam$silinfo$avg.width) # silhueta do PAM com k = i
}
pams_df <- do.call(rbind, pams)
colnames(pams_df) <- c("k", "s_barra_k")
pams_df %>% data.frame() %>% 
  arrange(-s_barra_k) %>% 
  # Valores de k ordernados por s_barra_k
  head() %>% mutate(s_barra_k = round(s_barra_k,4))

# Aplicação do PAM com k=2
melhor_pam <- pam(dados, 2)
melhor_pam$medoids # atributos dos medoides
melhor_pam$id.med # índices dos medoides

# Informações de cada grupo
melhor_pam$clusinfo
melhor_pam$silinfo

# faces de Chernoff dos índices
faces(dados[melhor_pam$id.med,], labels = melhor_pam$id.med) 

# faces de Chernoff do primeiro grupo
faces(dados[melhor_pam$clustering==1,], labels = "", face.type = 1)

# faces de Chernoff do segundo grupo
faces(dados[melhor_pam$clustering==2,][1:(sum(melhor_pam$clustering==2)/2),], labels = "", face.type = 1)
faces(dados[melhor_pam$clustering==2,][(sum(melhor_pam$clustering==2)/2+1):(sum(melhor_pam$clustering==2)),], labels = "", face.type = 1)



