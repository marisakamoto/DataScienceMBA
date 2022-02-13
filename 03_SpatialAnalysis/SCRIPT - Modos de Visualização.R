pacotes <- c("rgdal","plotly","tidyverse","knitr","kableExtra","gridExtra",
             "png","grid","magick","rgl","devtools","GISTools","rayshader",
             "tmap","broom")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Outras Opções e Dicas de Visualização de Objetos Espaciais ------------------


# Para começar a explorar algumas opções de visualização de mapas, vamos começar
# com um shapefile da Região Centro-Oeste do Brasil.

# Carregando um shapefile da Região Centro-Oeste brasileira
shp_centro_oeste <- readOGR(dsn = "shapefile_centrooeste", 
                            layer = "centrooeste_shapefile")

# Explorando a base de dados do shapefile shp_centro_oeste:
shp_centro_oeste@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Notaram os erros de enconding? Para corrigi-los, uma opção pode ser recarregar
# o shapefile com um tipo de encoding que suporte os caracteres utilizados:
shp_centro_oeste <- readOGR(dsn = "shapefile_centrooeste", 
                            layer = "centrooeste_shapefile",
                            encoding = "UTF8")

# Explorando a base de dados do shapefile shp_centro_oeste recarregado:
shp_centro_oeste@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualizando o shapefile de forma espacial:
tm_shape(shp = shp_centro_oeste) + 
  tm_borders()

# Para quem não sabe onde fica a Região Centro-Oeste brasileira:
tmap_mode("view")

tm_shape(shp = shp_centro_oeste) + 
  tm_borders()

tmap_mode("plot")

# Caso a intenção seja a de identificar cada município envolvido na plotagem, a
# função tm_text() pode ser uma solução, caso não haja muitos polígonos 
# envolvidos
tm_shape(shp = shp_centro_oeste) + 
  tm_borders() +
  tm_text(text = "NM_MUNICIP", size = 0.4)

# Quando há uma grande quantidade de polígonos, sugiro a utilização conjunta das
# funcionalidades do pacote plotly e ggplot2:

# Passo 1: Transformando nosso shapefile num data frame:
shp_centro_oeste_df <- tidy(shp_centro_oeste, region = "CD_GEOCODM") %>% 
  rename(CD_GEOCODM = id) %>% 
  mutate(state = substr(x = group, start = 1, stop = 2),
         state = factor(state,
                        levels = c("50", "51", "52", "53"),
                        labels = c("MS", "MT", "GO", "DF")),
         city_cod = substr(x = group, start = 1, stop = 7)) %>% 
  left_join(shp_centro_oeste@data,
           by = "CD_GEOCODM") %>% 
  rename(city = NM_MUNICIP)

# Passo 2: Fazendo a plotagem de forma espacial e interativa:
ggplotly(
  shp_centro_oeste_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = state, 
                     label = city),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "State") +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
)

# Destacando as capitais dos Estados:

# Passo 1: Criando uma variável que identifique as capitais no objeto 
# shp_centro_oeste_df:
shp_centro_oeste_df %>% 
  mutate(capital = ifelse(city %in% c("CAMPO GRANDE", "CUIABÁ", 
                                      "GOIÂNIA", "BRASÍLIA"),
                          yes = TRUE,
                          no = FALSE)) -> shp_centro_oeste_df

# Passo 2: Fazendo a plotagem de forma espacial e interativa:
ggplotly(
  shp_centro_oeste_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = capital, 
                     label = city),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Capital?") +
    scale_fill_viridis_d(option = "plasma") +
    facet_wrap(~state) +
    theme_bw()
)

# Adicionando informações sobre a pobreza da Região Centro-Oeste:
load("dados_centro_oeste.RData")

shp_centro_oeste_dados <- merge(x = shp_centro_oeste,
                                y = dados_centro_oeste,
                                by.x = "CD_GEOCODM",
                                by.y = "CD_GEOCODM")

# Plotando espacialmente a variável de interesse:
tm_shape(shp = shp_centro_oeste_dados) + 
  tm_fill(col = "poverty", 
          style = "quantile", 
          n = 4, 
          palette = "viridis", 
          legend.hist = TRUE) +
  tm_borders(alpha = 0.8) +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Para uma aproximação da plotagem feita anteriormente com o pacote ggplot2:
shp_centro_oeste_df <- tidy(shp_centro_oeste_dados, region = "CD_GEOCODM") %>% 
  rename(CD_GEOCODM = id) %>% 
  left_join(shp_centro_oeste_dados@data, by = "CD_GEOCODM")

# Vamos reconstruir o último mapa, explorando algumas palestas de cores do
# pacote viridis:

# a) Esquema de cores padrão, viridis:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_c(option = "viridis") +
  theme_bw()

# Se a intenção é a cópia perfeita do mapa da linha 133, mas com a biblioteca
# ggplot2, então temos que replicar as faixas estabelecidas pelo tmap:
shp_centro_oeste_df <- shp_centro_oeste_df %>% 
  mutate(poverty_bands = ifelse(test = poverty <= 25.40, 
                                yes = "7.84 to 25.40",
                                no = ifelse(test = poverty > 25.40 & poverty <= 31.80,
                                            yes = "25.41 to 31.80",
                                            no = ifelse(test = poverty > 31.80 & poverty <= 39.88,
                                                        yes = "31.81 to 39.88",
                                                        no = "39.89 to 72.04"))),
         poverty_bands = factor(poverty_bands,
                                levels = c("7.84 to 25.40",
                                           "25.41 to 31.80",
                                           "31.81 to 39.88",
                                           "39.89 to 72.04"))) 


# Então:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "viridis") +
  theme_bw()

# b) Esquema de cores cividis:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "cividis") +
  theme_bw()

# c) Esquema de cores inferno:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "inferno") +
  theme_bw()

# d) Esquema de cores magma:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "magma") +
  theme_bw()

# e) Esquema de cores turbo:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "turbo") +
  theme_bw()

# f) Esquema de cores plasma:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "plasma") +
  theme_bw()

# g) Esquema de cores rocket:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "rocket") +
  theme_bw()

# h) Esquema de cores mako:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "mako") +
  theme_bw()

# Plotando o histograma da variável poverty:
shp_centro_oeste_df %>% 
  ggplot() +
  geom_histogram(aes(x = poverty, fill = ..count..), color = "white") +
  scale_fill_gradient("Poverty", low = "#440154FF", high = "#FDE725FF") +
  labs(x = "Poverty",
       y = "Frequência") +
  theme_bw()

# Ok, mas como podemos fazer para combinar o mapa ao histograma?

# Passo 1: salvar o mapa e o histograma em objetos distintos:
mapa <- shp_centro_oeste_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = poverty_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Poverty") +
  scale_fill_viridis_d(option = "viridis") +
  theme_bw()

histograma <- shp_centro_oeste_df %>% 
  ggplot() +
  geom_histogram(aes(x = poverty, fill = ..count..), color = "white") +
  scale_fill_gradient("Poverty", low = "#440154FF", high = "#FDE725FF") +
  labs(x = "Poverty",
       y = "Frequência") +
  theme_bw()

# Passo 2: Utilizar a função grid.arrange(), do pacote gridExtra, para combinar
# os objetos quem contêm os gráficos de interesse:
# Versão em linha:
grid.arrange(mapa, histograma, nrow = 1)

# Versão em coluna:
grid.arrange(mapa, histograma, ncol = 1)

# Plotagens da variável de interesse de forma espacial e 3D -------------

# Carregando um shapefile do Estado de Santa Catarina (SC):
shp_sc <- readOGR(dsn = "shapefile_sc",
                  layer = "sc_state")

# Visualizando o objeto shp_sc:
tm_shape(shp = shp_sc) + 
  tm_borders()

# Para quem não sabe onde fica o Estado de SC:
tmap_mode("view")

tm_shape(shp = shp_sc) + 
  tm_borders()

tmap_mode("plot")

# Carregando dados sobre a pobreza em SC:
load("dados_sc.RData")

# Observando a base de dados dados_sc:
dados_sc %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

summary(dados_sc) # Note que há missing values

# Passo 1: Transformando o objeto shp_sc num data frame:
shp_sc_df <- tidy(shp_sc, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id)

# Passo 2: Juntando as informações da base de dados dados_sc ao objeto 
# shp_sc_df:
shp_sc_df <- shp_sc_df %>% 
  left_join(dados_sc, by = "CD_GEOCMU")

# Passo 3: Gerando um mapa no ggplot2
shp_sc_df %>%
  ggplot(aes(x = long,
           y = lat, 
           group = group, 
           fill = poverty)) +
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sc_df$poverty),
                      low = "#FFF3B0", 
                      high="#E09F3E") +
  layer(geom = "path", 
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 4: Salvando o mapa gerado no Passo 3 num objeto:
mapa_sc <- shp_sc_df %>%
  ggplot(aes(x = long,
             y = lat, 
             group = group, 
             fill = poverty)) +
  geom_polygon() +
  scale_fill_gradient(limits = range(shp_sc_df$poverty),
                      low = "#FFF3B0", 
                      high="#E09F3E") +
  layer(geom = "path", 
        stat = "identity", 
        position = "identity",
        mapping = aes(x = long, 
                      y = lat, 
                      group = group, 
                      color = I('#FFFFFF'))) +
  theme(legend.position = "none", 
        axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 5: Salvando o objeto mapa_sc como um arquivo de extensão *.png, com uma
# boa resolução:
xlim <- ggplot_build(mapa_sc)$layout$panel_scales_x[[1]]$range$range
ylim <- ggplot_build(mapa_sc)$layout$panel_scales_y[[1]]$range$range

ggsave(filename = "mapa_co_dsa.png",
       width = diff(xlim) * 4, 
       height = diff(ylim) * 4, 
       units = "cm")

# Passo 6: Carregando o arquivo mapa_co_dsa.png:
background_mapa <- readPNG("mapa_co_dsa.png")

# Passo 7: Capturando as coordenadas dos centroides de cada município de SC num 
# data frame:
coordinates(shp_sc) %>% 
  data.frame() %>% 
  rename(longitude = 1,
         latitude = 2) %>% 
  mutate(CD_GEOCMU = shp_sc@data$CD_GEOCMU) %>% 
  select(latitude, everything()) -> coords_sc

# Passo 8: Adicionando as coordenadas dos municípios do Centro-Oeste no objeto
# map_data_centro_oeste
shp_sc_df <- shp_sc_df %>% 
  left_join(coords_sc, by = "CD_GEOCMU")

# Passo 9: Georreferenciando a imagem PNG e plotando marcações sobre a pobreza 
# em SC nos centroides de cada polígono
shp_sc_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) + 
  scale_colour_gradient(name = "Poverty", 
                        limits = range(shp_sc_df$poverty), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 10: Salvando o resultado do Passo 9 num objeto
mapa_pobreza <- shp_sc_df %>%
  ggplot() + 
  annotation_custom(
    rasterGrob(background_mapa, 
               width=unit(1,"npc"),
               height=unit(1,"npc")),-Inf, Inf, -Inf, Inf) + 
  xlim(xlim[1],xlim[2]) + # x-axis Mapping
  ylim(ylim[1],ylim[2]) + # y-axis Mapping
  geom_point(aes(x = longitude, y = latitude, color = poverty), size = 1.5) + 
  scale_colour_gradient(name = "Poverty", 
                        limits = range(shp_sc_df$poverty), 
                        low = "#FCB9B2", 
                        high = "#B23A48") + 
  theme(axis.line = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.background = element_blank())

# Passo 11: Gerando o mapa 3D da pobreza em SC (não feche a janela pop-up que
# foi aberta até completar esse script até o final)
plot_gg(ggobj = mapa_pobreza, 
        width = 11, 
        height = 6, 
        scale = 300, 
        multicore = TRUE, 
        windowsize = c(1000, 800))

# Passo 12: Melhorando o resultado do Passo 11:
render_camera(fov = 70, 
              zoom = 0.5, 
              theta = 130, 
              phi = 35)

# Opções de salvamento em vídeo do mapa gerado nos Passos 11 e 12:
azimute_metade <- 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
azimute_completo <- c(azimute_metade, rev(azimute_metade))

rotacao <- 0 + 45 * sin(seq(0, 359, length.out = 360) * pi/180)

zoom_metade <- 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoom_completo <- c(zoom_metade, rev(zoom_metade))

render_movie(filename = "resutado1_sc", 
             type = "custom", 
             frames = 360, 
             phi = azimute_completo, 
             zoom = zoom_completo, 
             theta = rotacao)

# Carregando os dados de alguns terremotos na Oceania:
data(quakes)

# Observando os dados do objeto quakes:
quakes %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Aplicando a função class():
class(quakes)

# Transformando o data frame quakes num objeto sf:
sf_terremotos <- st_as_sf(x = quakes, 
                          coords = c("long", "lat"), 
                          crs = 4326)

# Observando os dados do objeto sf_terremotos:
sf_terremotos %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualizando o objeto sf_terremotos de forma espacial:
tm_shape(sf_terremotos) +
  tm_dots(size = 0.5, alpha = 0.3)

# Gerando gráficos estratificados pela profundidade do terremoto - 
# estratificação por tamanho:
tm_shape(shp = sf_terremotos) +
  tm_bubbles(size = "depth", 
             scale = 1, 
             shape = 19, 
             alpha = 0.3,
             title.size = "Profundidade dos Terremotos") +
  tm_compass() +
  tm_layout(legend.outside = TRUE,
            legend.text.size = 3)

# Gerando gráficos estratificados pela profundidade do terremoto - 
# estratificação por cor:
tm_shape(shp = sf_terremotos) +
  tm_dots(col = "depth", 
          shape = 19, 
          alpha = 0.5, 
          size = 0.6, 
          palette = "viridis", 
          title = "Profundidade dos Terremotos") +
  tm_compass() +
  tm_layout(legend.outside = TRUE,
            legend.text.size = 3)

# Como gerar plotar dois gráficos na aba Plots, simultaneamente, com o tmap:
grid.newpage()

# Passo 1: Salvar em um objeto o primeiro gráfico de interesse:
plot_01 <- tm_shape(shp = sf_terremotos) +
  tm_bubbles(size = "depth", 
             scale = 1, 
             shape = 19, 
             alpha = 0.3,
             title.size = "Profundidade dos Terremotos") +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Passo 2: Salvar em um objeto o segundo gráfico de interesse:
plot_02 <- tm_shape(shp = sf_terremotos) +
  tm_dots(col = "depth", 
          shape = 19, 
          alpha = 0.5, 
          size = 0.6, 
          palette = "viridis", 
          title = "Profundidade dos Terremotos") +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Passo 3: Preparar o ambiente gráfico do R para receber gráficos de forma
# simultânea:
pushViewport(
  viewport(
    layout = grid.layout(1,2)
  )
)

# Passo 4: Executar as plotagens
print(plot_01, vp = viewport(layout.pos.col = 1, height = 5))
print(plot_02, vp = viewport(layout.pos.col = 2, height = 5))

# Estabelecendo cutoffs
tmap_mode("view")

sf_terremotos %>% 
  filter(mag >= 6) %>% 
  tm_shape() +
  tm_dots(col = "depth", 
          shape = 19, 
          alpha = 0.5, 
          size = 0.2, 
          #palette = "-plasma", 
          title = "Profundidade dos Terremotos")

# Fim ---------------------------------------------------------------------
