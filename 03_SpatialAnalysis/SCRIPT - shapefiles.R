# Instalação e Carregamento dos Pacotes Necessários para a Aula -----------

pacotes <- c("rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


################################## SHAPEFILES ##################################

# 1. PARTE INTRODUTÓRIA

# Carregando um shapefile -------------------------------------------------
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

# Características básicas do objeto shp_sp
summary(shp_sp)

# Classe e tipo do objeto carregado
class(shp_sp)
typeof(shp_sp)


# Acessando a base de dados e outros componentes do objeto shp_sp ---------


# Para acessar a base de dados de um shapefile, devemos utilizar o operador @:
shp_sp@data

shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Para acessar as variáveis da base de dados atrelada ao shapefile, utilizaremos
# o operador $:
shp_sp$NM_MUNICIP
shp_sp$CD_GEOCMU

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
area(shp_sp)

area(shp_sp) / 1000000

shp_sp@data["area_aproximada"] <- area(shp_sp) / 1000000

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

# Porém, como deveria ser feita a plotagem espacial dos dados do dataset do 
# objeto shp_dados_sp?

# A seguir, a solução via ggplot2; depois, a solução via pacote tmap, que será
# adotado pelo curso


# 2. VISUALIZAÇÃO DE DADOS ESPACIAIS


# Utilizando o ggplot2: ---------------------------------------------------

# Passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame.
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
       color = "IDH") +
  scale_fill_viridis_c() +
  theme_bw()


# Utilizando a tmap: ------------------------------------------------------
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
          palette = "BuPu", 
          legend.hist = TRUE) +
  tm_layout(legend.text.size = 0.7,
            legend.title.size = 0.9,
            legend.hist.size = 0.5,
            legend.hist.height = 0.2,
            legend.hist.width = 0.3,
            frame = FALSE,
            main.title = "A Distribuição do IDH nos Municípios de SP")

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
tm_shape(shp = shp_mundo) + 
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
