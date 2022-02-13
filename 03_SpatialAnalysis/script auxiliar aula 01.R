# Pessoal, tudo bem? Adicionei comentários ao script auxiliar da aula. Abraços! 

# Pacote e sua respectiva utilização para conversão de unidades de medidas, 
# incluindo o caso das coordenadas geográficas:
library(measurements)
help(package = "measurements")

conv_unit(x = -23.5928180, from = "dec_deg", to = "deg_min_sec")
conv_unit(x = "-23 35 34.1448000000091", from = "deg_min_sec", to = "dec_deg")


# Pacote para criação de robôs em R:
library(RSelenium)


# Aqui, durante a aula, nós criamos um data frame fictício para demonstrar que
# objetos que habitam o ambiente S3 do R, via de regra, utilizam somente o 
# operador $ para o ato de explorar seus componentes:
x <- rnorm(100)
y <- rnorm(100)

df <- data.frame(x, y)

df$x
df$y

# Notem que para objetos do ambiente S4, pode ser que haja a necessidade do uso
# do operador @:
shp_sp@data$NM_MUNICIP
shp_sp@data$CD_GEOCMU

shp_sp@polygons
shp_sp@plotOrder

shp_sp@data$NM_MUNICIP[33]
shp_sp@data[33, ]


shp_sp@bbox
shp_sp@proj4string


# Demonstrando diferentes métodos para a criação de uma variável no interior
# da base de dados de um shapefile. Caso queiram, a função mutate(), do pacote
# dplyr, também pode ser utilizada.
shp_sp@data["area_aproximada"] <- raster::area(shp_sp) / 1000000

shp_sp@data$area2 <- raster::area(shp_sp) / 1000000

# Aqui nós quebramos as rotinas das linha 133 a 136, do script de shapefiles:
shp_dados_df <- tidy(shp_dados_sp, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id) %>% 
  left_join(shp_dados_sp@data,
            by = "CD_GEOCMU")

passo1 <- tidy(shp_dados_sp, region = "CD_GEOCMU")
passo2 <-  rename(passo1, CD_GEOCMU = id) 
passo3 <- left_join(passo2, shp_dados_sp@data, by = "CD_GEOCMU")


# Aqui, nós utilizamos o pacote plotly, função ggplotly(), para deixar 
# interativo o mapa gerado na linha 139 (script de shapefiles) 
plotly::ggplotly(
  shp_dados_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = idh, 
                     label = NM_MUNICIP),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "IDH") +
    scale_fill_viridis_c() +
    theme_bw()
)

# Fim ---------------------------------------------------------------------