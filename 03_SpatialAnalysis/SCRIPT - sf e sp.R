# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("tidyverse","sf","tmap","rgdal","rgeos","adehabitatHR","knitr",
             "kableExtra")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# 1. CRIANDO UM OBJETO SF A PARTIR DE UMA BASE DE DADOS

# Carregando a base de dados
load("shoppings.RData")

# Observando a classe do objeto shoppings:
class(shoppings)

# Observando as variáveis da base de dados shoppings:
shoppings %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Criando um objeto do tipo sf a partir de um data frame:
sf_shoppings <- st_as_sf(x = shoppings, 
                         coords = c("longitude", "latitude"), 
                         crs = 4326)

# Observando a classe do objeto sf_shoppings:
class(sf_shoppings)

# Um componente interessante do objeto sf_shoppings é chamado geometry:
sf_shoppings$geometry

# Note que um objeto sf é, de fato, um data frame georreferenciado. Não há
# polígonos atrelados a ele, nem a necessidade de se utilizar o operador @!

# Plotando o objeto sf_shoppings de forma espacial:
tm_shape(shp = sf_shoppings) + 
  tm_dots(size = 1)

# Adicionando uma camada de um mapa do Leafleet que considere a bounding box do 
# objeto sf_shoppings:
tmap_mode("view")

tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "deepskyblue4", 
          border.col = "black", 
          size = 0.2, 
          alpha = 0.8)

#tmap_mode("plot") #Para desativar as camadas de mapas on-line


# 2. COMBINANDO UM OBJETO SIMPLE FEATURE COM UM SHAPEFILE

# Carregando um shapefile do município de São Paulo
shp_saopaulo <- readOGR("shapefile_municipio", "municipio_sp")

# Visualização gráfica do objeto shp_saopaulo:
tm_shape(shp = shp_saopaulo) + 
  tm_borders()

# Combinando o objeto shp_saopaulo com o objeto sf_shoppings:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "regiao", 
          size = 0.02)


# 3. BUFFER ANALYSIS

# O buffering é uma técnica para se medir distâncias para fora de um dado ponto
# geográfico.

# A aplicação da técnica de buffering pode ser feita com o uso da função 
# gBuffer(), do pacote rgeos:
buffer_shoppings <- gBuffer(spgeom = sf_shoppings,
                            width = 1500,
                            byid = TRUE)

# A função gBuffer() não funciona com objetos do tipo sf. Para utilizá-la,
# precisaremos converter o objeto sf_shoppings para o tipo spatial points (sp).

# Para tanto, primeiramente, precisaremos isolar as coordenadas de longitude e 
# de latitude do data frame original shoppings:
coordenadas_shoppings <- cbind(shoppings$longitude,
                               shoppings$latitude)

coordenadas_shoppings

# Depois, utilizaremos a função SpatialPoints() para criar um objeto do tipo sp:
sp_shoppings <- SpatialPoints(coords = coordenadas_shoppings,
                              proj4string = CRS("+proj=longlat"))

# Criamos nosso primeiro objeto de classe sp! Vamos explorá-lo:
sp_shoppings@coords
sp_shoppings@bbox
sp_shoppings@proj4string@projargs

# Uma plotagem básica:
plot(sp_shoppings)

# A função SpatialPoints() não permite a existência de um data frame em um
# objeto sp. Mais a frente, estudaremos a função SpatialPointsDataFrame() que
# quebra essa lógica e permite a existência de uma base de dados atrelada a
# um objeto de classe sp.

# Visualizando o resultado:
tmap_mode("plot")

tm_shape(shp = sp_shoppings) + 
  tm_dots(size = 1)

# Vamos tentar aplicar a função gBuffer() mais uma vez:
buffer_shoppings <- gBuffer(spgeom = sp_shoppings,
                            width = 1500,
                            byid = TRUE)

# Dessa vez, o erro foi diferente. Além de exigir um objeto de classe sp,
# a função gBuffer() exige que o objeto se oriente com distâncias euclidianas.
# Nosso atual objeto se orienta de forma geodésica.
shoppings_UTM <- spTransform(x = sp_shoppings,
                             CRSobj = CRS("+init=epsg:22523"))

# Visualizando o resultado:
tm_shape(shp = shoppings_UTM) + 
  tm_dots(size = 1)

# Agora sim, poderemos aplicar a função gBuffer():
buffer_shoppings <- gBuffer(spgeom = shoppings_UTM, 
                            width = 1500, 
                            byid = TRUE)

# Plotagem do objeto buffer_shoppings:
tm_shape(shp = buffer_shoppings) + 
  tm_borders()

tmap_mode("view")

tm_shape(shp = buffer_shoppings) + 
  tm_borders()

# Combinando os objetos shp_saopaulo, sf_shoppings e buffer_shoppings:
tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "regiao", 
          size = 0.02) +
  tm_shape(buffer_shoppings) + 
  tm_borders(col = "black") 


# 4. BUFFER UNION

# A técnica de buffer union combina aqueles outputs da técnica de buffering que,
# por ventura, se encontrem.
buffer_union <- gUnaryUnion(spgeom = buffer_shoppings)


tm_shape(shp = shp_saopaulo) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sf_shoppings) + 
  tm_dots(col = "regiao", 
          size = 0.02) +
  tm_shape(shp = buffer_union) + 
  tm_borders(col = "black") + 
  tm_fill(col = "gray",
          alpha = 0.5) 


# 5. KERNEL DENSITIES

# A técnica de kernel densities calcula a densidade da presença de pontos de
# interesse em determinada área geográfica.

# O primeiro passo será criar um objeto sp com a base de dados atrelada a ele:
shoppings_sp_df <- SpatialPointsDataFrame(data = shoppings,
                                          coords = coordenadas_shoppings,
                                          proj4string = CRS("+proj=longlat"))


# Note como a função SpatialPointsDataFrame() permite a existência de um data
# frame junto a nosso objeto de classe sp:
shoppings_sp_df@data

# Para o cálculo das kernel densities, podemos utilizar a função kernelUD():
shoppings_dens <- kernelUD(xy = shoppings_sp_df,
                           h = "href",
                           grid = 1000,
                           boundary = NULL)

plot(shoppings_dens)

# Para estabelecer as zonas com maior densidade, propomos o seguinte:
zona1 <- getverticeshr(x = shoppings_dens, percent = 20) 
zona2 <- getverticeshr(x = shoppings_dens, percent = 40) 
zona3 <- getverticeshr(x = shoppings_dens, percent = 60) 
zona4 <- getverticeshr(x = shoppings_dens, percent = 80)

tmap_options(check.and.fix = TRUE) 

tm_shape(shp = shp_saopaulo) + 
  tm_fill(col = "gray90") + 
  tm_borders(col = "white", alpha = 0.5) + 
  tm_shape(shp = shoppings_sp_df) + 
  tm_dots(col = "regiao", size = 0.25) + 
  tm_shape(zona1) + 
  tm_borders(col = "firebrick4", lwd = 2.5) +
  tm_fill(alpha = 0.4, col = "firebrick4") + 
  tm_shape(zona2) + 
  tm_borders(col = "firebrick3", lwd = 2.5) + 
  tm_fill(alpha = 0.3, col = "firebrick3") + 
  tm_shape(zona3) + 
  tm_borders(col = "firebrick2", lwd = 2.5) + 
  tm_fill(alpha = 0.2, col = "firebrick2") +
  tm_shape(zona4) + 
  tm_borders(col = "firebrick1", lwd = 2.5) + 
  tm_fill(alpha = 0.1, col = "firebrick1")


# FIM ---------------------------------------------------------------------
