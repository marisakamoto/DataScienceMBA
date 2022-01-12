
# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("rgdal","raster","tmap","maptools","sf","rgeos","sp","adehabitatHR",
             "tidyverse","broom","rayshader","knitr","kableExtra","RColorBrewer",
             "profvis")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

