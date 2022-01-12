# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("tidyverse","raster","tmap","rgdal","rayshader")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# 1. INTRODUÇÃO AOS OBJETOS RASTER

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
     xlab = "Altitudes das Formações Geográficas",
     ylab = "Frequência",
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
      main = "Relevo de Parte do Litoral Paulista")

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


# FIM ---------------------------------------------------------------------