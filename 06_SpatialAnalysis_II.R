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
          size = 0.05)

#show all attributes
attributes(sf_shoppings$geometry)

attr(sf_shoppings$geometry, which = "crs")


attributes(shp_saopaulo@data)

spTransform(shp_saopaulo,
            CRSobj = "+proj=utm +zone=18 +north +unit=km +datum=WGS84 +init=epsg:4326 ")-> shp_distorted
plot(shp_distorted)

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
tmap_mode("view")

tm_shape(shp = sp_shoppings) + 
  tm_dots(size = 0.1)

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
  tm_dots(size = .1)

# Agora sim, poderemos aplicar a função gBuffer():
buffer_shoppings <- gBuffer(spgeom = shoppings_UTM, 
                            width = 1500, 
                            byid = TRUE)


# Plotagem do objeto buffer_shoppings:
tm_shape(shp = buffer_shoppings) + 
  tm_borders()

shp_saopaulo <- readOGR("shapefile_municipio", "municipio_sp")

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
  tm_borders(col = "skyblue4", lwd = 2.5) +
  tm_fill(alpha = 0.4, col = "skyblue4") + 
  tm_shape(zona2) + 
  tm_borders(col = "skyblue3", lwd = 2.5) + 
  tm_fill(alpha = 0.3, col = "skyblue3") + 
  tm_shape(zona3) + 
  tm_borders(col = "skyblue2", lwd = 2.5) + 
  tm_fill(alpha = 0.2, col = "skyblue2") +
  tm_shape(zona4) + 
  tm_borders(col = "skyblue1", lwd = 2.5) + 
  tm_fill(alpha = 0.1, col = "skyblue1")


tmaptools::palette_explorer()

# FIM -----------#c----------------------------------------------------------
#Clean RAM space
gc()

library(raster)
library(rayshader)
# Carregando nosso objeto raster:
relevo_sp <- raster("raster_sp/relevo_sp.tif")


# Observando a classe do objeto relevo_sp:
class(relevo_sp)

# Explorando o objeto relevo_sp:
relevo_sp

# Plotando o objeto relevo_sp de forma espacial:
plot.new() #Limpando a aba Plots

plot(relevo_sp)

# Podemos estabelecer um histograma a respeito das altitudes do objeto relevo_sp
hist(relevo_sp, 
     main = "", 
     xlab = "Elevation",
     ylab = "Frequency",
     col = "deepskyblue4",
     maxpixels = 2160000)

# Ok, mas onde estão os dados de altitude no objeto relevo_sp, e como poderei
# plotá-los utilizando o ggplot2, por exemplo?

# A base de dados do objeto relevo_sp pode ser extraída com a função 
# as.data.frame():
relevo_sp_df <- as.data.frame(relevo_sp, xy = TRUE)

# Observando a base de dados relevo_sp_df:
head(relevo_sp_df)
min(relevo_sp_df$relevo_sp)
max(relevo_sp_df$relevo_sp)

# Gerando o histograma anterior com o ggplot2:
relevo_sp_df %>% 
  ggplot() +
  geom_histogram(aes(x = relevo_sp),
                 fill = "deepskyblue4",
                 color = "white") +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Altitudes das Formações Geográficas",
       y = "Frequência") +
  theme_bw()

# Apesar da função plot() ter cumprido o que esperávamos, a função image() 
# possui maior capacidade visual para trabalharmos com imagens:
image(relevo_sp)

# Podemos deixar a visualização mais elegante com o uso dos atributos xlab, 
# ylab e main, conforme o código a seguir:
image(relevo_sp, 
      xlab = "Longitude", 
      ylab = "Latitude", 
      legend.only= T,
      main = "São Paulo Shore Elevation ")

# A função image() também permite a especificação de outras cores em seus
# resultados quando combinada com a função terrain.colors(). Dessa forma, 
# podemos comandar o seguinte:
image(relevo_sp, 
      xlab = "Longitude", 
      ylab = "Latitude", 
      main = "Relevo de Parte do Litoral Paulista",
      col = terrain.colors(10))

# Podemos, ainda, delimitar, para o caso, as altitudes mínimas e máximas a 
# serem plotadas com o uso do argumento zlim. Suponhamos que apenas nos 
# interessem as áreas com altitude máxima de 800m:
image(relevo_sp, 
      xlab = "Longitude", 
      ylab = "Latitude", 
      main = "Relevo de Parte do Litoral Paulista",
      col = terrain.colors(10),
      zlim = c(0,800))

# E se quisermos uma plotagem 3D?

# Podemos fazer isso com o auxílio do pacote rayshader

# O primeiro passo é transformar nosso objeto raster em uma matriz:
library(raycaster)
relevo_matriz <- raster_to_matrix(raster = relevo_sp, verbose = interactive())

# Depois:
relevo_matriz %>%
  sphere_shade(texture = "imhof1") %>%
  plot_3d(relevo_matriz, zscale = 50, theta = -45, phi = 45, water = TRUE,
          windowsize = c(1000,800), zoom = 0.75, waterlinealpha = 0.3,
          wateralpha = 0.5, watercolor = "lightblue", waterlinecolor = "white",
          waterdepth = 100)

# Para capturar um snapshot:
render_snapshot()

# A plotagem espacial dos objetos de classe raster comporta a utilização do
# pacote tmap:
tm_shape(shp = relevo_sp) + 
  tm_raster(style = "quantile", 
            n = 5) +
  tm_layout(legend.position = c("left", "bottom"), 
            legend.outside = TRUE)


# 2. COMBINANDO OBJETOS RASTER COM OBJETOS SHAPEFILE

# Carregando nosso shapefile do Estado de São Paulo:
shp_sp <- readOGR("shapefile_sp", "estado_sp")

# Plotando o shapefile shp_sp:
tm_shape(shp = shp_sp) + 
  tm_borders()

# Plotando a combinação do objeto raster com o objeto shapefile:
tm_shape(shp = relevo_sp) + 
  tm_raster(style = "quantile", n = 5) +
  tm_shape(shp = shp_sp) + 
  tm_borders() + 
  tm_layout(legend.position = c("left", "bottom"), 
            legend.outside = TRUE)

# 3. CARREGANDO OBJETOS RASTER POR INTEIRO NA RAM DO COMPUTADOR

# Para verificarmos se o objeto relevo_sp está aberto na RAM da máquina, 
# utilizamos a função inMemory(), comandando da seguinte maneira:
inMemory(relevo_sp)

mem_relevo_sp <- readAll(relevo_sp)

# A seguir, tornamos a utilizar a função inMemory(), porém aplicada ao novo 
# objeto criado mem_relevo_sp:
inMemory(mem_relevo_sp)
gc()
# Demonstrando o ganho computacional de ter o objeto raster completamente 
# carregado pela máquina:
info_relevo_sp <- profvis({for(i in 1:10){
  summary(relevo_sp[])
  print(i)}})

info_mem_relevo_sp <- profvis({for(i in 1:10){
  summary(mem_relevo_sp[])
  print(i)}})

# Verificando a perfomance dos dois cálculos:
print(info_relevo_sp)
print(info_mem_relevo_sp)

# 3. RECORTANDO OBJETOS RASTER

# Recortando objetos raster com o mouse -----------------------------------
plot(mem_relevo_sp)


# Recortes retangulares com o mouse:

# Quando o comando abaixo for realizado, o sinal de 'stop' no Console do R
# ficará ativo. Nesse momento, o aluno deve utilizar seu mouse no mapa
# apresentado na aba Plots.

# Passo 1: clicar onde se deseja iniciar o recorte;
# Passo 2: clicar onde se deseja finalizar o recorte.
recorte_mouse_1 <- drawExtent()

# Para salvar a tarefa feita, podemos comandar:
relevo_recortado_1 <- crop(x = mem_relevo_sp, 
                           y = recorte_mouse_1)

# Para visualizar o que foi feito:
plot(relevo_recortado_1)

# Outros recortes com o mouse:
plot(mem_relevo_sp)

# Quando o comando abaixo for realizado, o sinal de 'stop' no Console do R
# ficará ativo. Nesse momento, o aluno deve utilizar seu mouse no mapa
# apresentado na aba Plots.

# O leitor deve clicar nos vértices dos recortes desejados e, quando estiver
# satisfeito com o recorte, deve apertar a tecla Esc de seu teclado.
recorte_mouse_2 <- raster::select(mem_relevo_sp, use = "pol")

# Para observar o que foi feito:
plot(recorte_mouse_2)


# Recortando objetos raster com a determinação de vetores -----------------

# O primeiro passo será salvar as coordenadas limítrofes de seu recorte.
recorte_coordenadas <- c(-45.51042, -45.18708, -23.95708, -23.74125)

# Em seguida, devemos utilizar a função crop():
relevo_recortado_3 <- crop(mem_relevo_sp, recorte_coordenadas)

# Para visualizar o que foi feito:
plot(relevo_recortado_3)

gc()

# FIM ---------------------------------------------------------------------
