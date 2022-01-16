library(measurements)
conv_unit(x = -34.234213445, from = "dec_deg", to = "deg_min_sec")
# To create robots to read chrome coordinates in maps
library(RSelenium)

# 1. INTRODUCTORY PART

# Loading shapefile -------------------------------------------------
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

summary(shp_sp)

# Object class and type of
class(shp_sp)
typeof(shp_sp)

# Acessando a base de dados e outros componentes do objeto shp_sp ---------

# To access the data, we use the @. ALWAYS USE @ to access data and not mistakenly change the database:
shp_sp@data
shp_sp@data$NM_MUNICIP #city name
shp_sp@data$CD_GEOCMU #city codes

shp_sp@polygons #it plots all coordinates from all polygons
shp_sp@plotOrder #the orders of the polygon will be plotted

#the last attribute to be plotted is number 33
shp_sp@data[33,]

shp_sp@bbox #boundary box, the limits

#shows the database
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Para acessar os outros componentes do shapefile, utilizaremos o operador @:
shp_sp@polygons #Posições geográficas dos polígonos
shp_sp@plotOrder #Ordem de plotagem dos polígonos
shp_sp@bbox #Eixo X (Longitude Oeste e Leste; Latitude Norte e Sul)
shp_sp@proj4string@projargs #Sistema de projeção geográfica do shapefile


# Plotagem básica de um shapefile -----------------------------------------
plot(shp_sp)


# Introdução à manipulação de dados em shapefiles -------------------------

# Vamos supor que haja a necessidade do cálculo das áreas dos municípios
# paulistas. Podemos fazer isso com a função area():

shp_sp@data["area_aproximada"] <- raster::area(shp_sp) / 1000000

shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


#  E caso haja a necessidade da inserção de dados externos? ---------------

# Carregando uma base de dados real a respeito dos municípios de SP:
load("dados_sp.RData")

# Observando a base de dados carregada
dados_sp %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Para combinar os dados do objeto dados_sp com a base de dados de nosso 
# shapefile, podemos utilizar a função merge():
shp_dados_sp <- merge(x = shp_sp,
                      y = dados_sp,
                      by.x = "CD_GEOCMU",
                      by.y = "codigo")
class(shp_dados_sp)
shp_dados_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Salvando nosso shapefile:
writeOGR(obj = shp_dados_sp, 
         layer = "nosso_novo_shapefile", 
         driver = "ESRI Shapefile", 
         dsn = "mbadsa")

# Caso a intenção fosse a plotagem dos dados do dataset presente no objeto 
# shp_dados_sp, a lógica seria a mesma já aprendida no curso:
shp_dados_sp@data %>% 
  ggplot() +
  geom_histogram(aes(x = idh),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "IDH",
       y = "Frequência") +
  theme_bw()
#But we wont use ggplot for spatial analysis, because it will get increasingly more complex thememap
# Porém, como deveria ser feita a plotagem espacial dos dados do dataset do 
# objeto shp_dados_sp?

# A seguir, a solução via ggplot2; depois, a solução via pacote tmap, que será
# adotado pelo curso


# 2. Visualizing spatial data 


# Utilizando o ggplot2: ---------------------------------------------------

# Passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame.

summary(shp_dados_sp)
shp_dados_df <- tidy(shp_dados_sp, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id) %>% 
  left_join(shp_dados_sp@data,
            by = "CD_GEOCMU")

#Passo 2: A plotagem.
shp_dados_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = idh),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "IDH") +
  scale_fill_viridis_c() +
  theme_bw()

# you can animate the map, by when hovering the mouse it gives you some information on the area
plotly::ggplotly(
  shp_dados_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, y = lat, group = group, fill = idh, label= NM_MUNICIP),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "IDH") +
    scale_fill_viridis_c() +
    theme_bw()
)

help(package = "tm_shape")
# Utilizando a tmap: ------------------------------------------------------
library(tmap)


tm_shape(shp = shp_dados_sp) +
  tm_fill(col = "idh", palette = "Blues")



# Como saber quais paletas de cores podem ser utilizadas? -----------------
display.brewer.all()

# Vamos reconstruir o último mapa, utilizando uma nova paleta de cor e propondo
# 4 variações de cores:
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "Greens")

# Adicionando um histograma ao mapa anterior
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "Greens", 
          legend.hist = TRUE)

# Reposicionando o histograma
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "Greens", 
          legend.hist = TRUE) +
  tm_layout(legend.outside = TRUE)

# Posicionando manualmente o histograma, e adicionando um título
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "RdPu", 
          legend.hist = TRUE) +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = FALSE,
            main.title = "HDI Distribution by City in São Paulo State")

# Adicionando uma bússola e bordas aos polígonos
tm_shape(shp = shp_dados_sp) + 
  tm_fill(col = "idh", 
          style = "quantile", 
          n = 4, 
          palette = "Reds", 
          legend.hist = TRUE) +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = F,
            main.title = "A Distribuição do IDH nos Municípios de SP") +
  tm_borders(alpha = 0.8) +
  tm_compass(type = "8star", 
             show.labels = 3)

# 3. DESMEMBRANDO SHAPEFILES

# Carregando um novo shapefile:
shp_mundo <- readOGR(dsn = "shapefile_mundo", layer = "mundo")

# Visualizando o shapefile shp_mundo:
tmap::tm_shape(shp = shp_mundo) + 
  tm_borders()

# Observando as variáveis da base de dados do objeto shp_mundo:
shp_mundo@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Suponha que a intenção seja a de criar um shapefile da América do Sul. Assim,
# note que a variável contnnt consegue estratificar os países desejados.

# Dessa forma:
shp_amsul <- shp_mundo[shp_mundo@data$contnnt == "South America", ]

# Plotando o  objeto shp_amsul:
tm_shape(shp = shp_amsul) + 
  tm_borders()


# 4. COMBINANDO SHAPEFILES

# Vamos supor que a intenção seja a de construir um shapefile do Mercosul.

# Carregando shapefiles a serem combinados
shp_argentina <- readOGR(dsn = "shapefile_mercosul", layer = "argentina_shapefile")
shp_brasil <- readOGR(dsn = "shapefile_mercosul", layer = "brasil_shapefile")
shp_paraguai <- readOGR(dsn = "shapefile_mercosul", layer = "paraguai_shapefile")
shp_venezuela <- readOGR(dsn = "shapefile_mercosul", layer = "venezuela_shapefile")

# A combinação pode ser feita com a função bind(), do pacote raster
shp_mercosul <- bind(shp_argentina, 
                     shp_brasil, 
                     shp_paraguai, 
                     shp_venezuela)

# Observando a base de dados do objeto shp_mercosul:
shp_mercosul@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualizando o shapefile criado shp_mercosul:
tm_shape(shp = shp_mercosul) + 
  tm_borders(lwd = 1) +
  tm_fill(col = "mercosul") +
  tm_layout(legend.width = 0.8)


# FIM ---------------------------------------------------------------------