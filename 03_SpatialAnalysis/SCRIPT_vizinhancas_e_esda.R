pacotes <- c("tidyverse","rgdal","spdep","knitr","kableExtra","tmap","gtools")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

plot_gg(ggobj = mapa_pobreza, 
        width = 11, 
        height = 6, 
        scale = 300, 
        multicore = TRUE, 
        windowsize = c(1000, 800))

# Estabelecendo Vizinhanças -----------------------------------------------


# 01) Vizinhanças por Contiguidade ----------------------------------------

# Carregando um shapefile:
shp_sp <- readOGR(dsn = "shapefile_sp", layer = "estado_sp")

# Visualizando o shapefile
plot(shp_sp)


# Estabelecendo vizinhanças por contiguidade, critério queen:
vizinhos_queen <- poly2nb(pl = shp_sp,
                          queen = TRUE,
                          row.names = shp_sp@data$NM_MUNICIP)

# Visualizando a vizinhança estabelecida:
plot(shp_sp, border = "lightgray")
plot(vizinhos_queen, 
     coordinates(shp_sp), 
     add = TRUE, 
     col = "#33638DFF")

# Informações relevantes sobre a vizinhança queen estabelecida:
summary(vizinhos_queen)

# Ok! Cadê a matriz W?
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B")

# Notaram o erro reportado pelo R? O erro diz, explicitamente, que há 
# observações sem uma vizinhança definida. Noutras palavras, há a ocorrência de 
# ilhas:
plot(shp_sp, border = "lightgray")
plot(vizinhos_queen, 
     coordinates(shp_sp), 
     add = TRUE, 
     col = "#33638DFF")

# No caso, a ilha diz respeito ao município de Ilhabela. Para contornar a
# situação, podemos utilizar o argumento zero.policy = TRUE.
matrizW_queen <- nb2mat(neighbours = vizinhos_queen,
                        style = "B",
                        zero.policy = TRUE)

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_queen) <- shp_sp@data$NM_MUNICIP

View(matrizW_queen)


# E se a ideia fosse o estabelecimento de contiguidades de outras ordens?
vizinhos_queen_ordens <- nblag(neighbours = vizinhos_queen, maxlag = 5)


vizinhos_queen_ordens[[1]] #Contiguidades de ordem 1
vizinhos_queen_ordens[[2]] #Contiguidades de ordem 2
vizinhos_queen_ordens[[3]] #Contiguidades de ordem 3
vizinhos_queen_ordens[[4]] #Contiguidades de ordem 4
vizinhos_queen_ordens[[5]] #Contiguidades de ordem 5


# Exemplo de plotagem com ordens superiores de contiguidades:
plot(shp_sp, border = "lightgray")
plot(vizinhos_queen_ordens[[2]], 
     coordinates(shp_sp), 
     add = TRUE, 
     col = "#33638DFF")

# Estabelecendo vizinhanças por contiguidade, critério rook:
vizinhos_rook <- poly2nb(pl = shp_sp,
                         queen = FALSE,
                         row.names = shp_sp@data$NM_MUNICIP)

# Visualizando a vizinhança estabelecida:
plot.new()
plot(shp_sp, border = "lightgray")
plot(vizinhos_rook, 
     coordinates(shp_sp), 
     add = TRUE, 
     col = "#95D840FF")

# Informações relevantes sobre a vizinhança rook estabelecida:
summary(vizinhos_rook)

# A matriz W pode ser extraída da seguinte forma:
matrizW_rook <- nb2mat(neighbours = vizinhos_rook,
                       style = "B",
                       zero.policy = TRUE)

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_rook) <- shp_sp@data$NM_MUNICIP

View(matrizW_rook)


# 02) Vizinhanças por Distância Geográfica ---------------------------------

# Carregando um shapefile:
shp_ba <- readOGR(dsn = "shapefile_ba", 
                  layer = "ba_state",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

# Estabelecendo vizinhanças por distâncias geográficas:
vizinhos_distancias <- dnearneigh(coordinates(shp_ba), 
                                  d1 = 0, 
                                  d2 = 90, 
                                  longlat = TRUE)

# Verificar com ?dnearneigh que por padrão, ao argumentar longlat = TRUE,
# as distâncias serão calculadas em km. Caso o objeto seja da classe sp, as
# unidades de medida consideradas serão aquelas presentes no CRS do objeto.

# Informações relevantes sobre a vizinhança estabelecida:
summary(vizinhos_distancias)

# Visualizando a vizinhança estabelecida:
plot.new()
plot(shp_ba, border = "lightgray")
plot(vizinhos_distancias, 
       coordinates(shp_ba), 
       add = TRUE, 
       col = "#CC6A70FF")

# A matriz W pode ser extraída da seguinte forma:
matrizW_distancias <- nb2mat(neighbours = vizinhos_distancias,
                             style = "B")

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_distancias) <- shp_ba@data$MUNICIPIO
rownames(matrizW_distancias) <- shp_ba@data$MUNICIPIO

View(matrizW_distancias)


# 03) Vizinhanças Ponderadas por k-Nearest Neighbors -----------------------

# Carregando um shapefile:
shp_sc <- readOGR(dsn = "shapefile_sc", 
                  layer = "sc_state",
                  encoding = "UTF-8", 
                  use_iconv = TRUE)

# Para a abordagem, o primeiro passo é a criação de um objeto do tipo lista
# que contenha os outputs da função knearneigh.
lista_knear <- knearneigh(coordinates(shp_sc), 
                          longlat = TRUE, 
                          k = 3)

# Depois, esse objeto do tipo lista pode ser transformado em um objeto que
# contenha as vizinhanças calculas:
vizinhos_knear <- knn2nb(knn = lista_knear)

# Informações relevantes sobre a vizinhança estabelecida:
vizinhos_knear

# Visualizando a vizinhança estabelecida:
plot.new()
plot(shp_sc, border = "lightgray")
plot(vizinhos_knear, 
     coordinates(shp_sc), 
     add = TRUE, 
     col = "#13306DFF")

# A matriz W pode ser extraída da seguinte forma:
matrizW_knear  <- nb2mat(neighbours = vizinhos_knear,
                         style = "B")

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_knear) <- shp_sc@data$NM_MUNICIP
rownames(matrizW_knear) <- shp_sc@data$NM_MUNICIP

View(matrizW_knear)


# 04) Vizinhanças Ponderadas por Distâncias Sociais -----------------------

# Carregando uma base de dados:
load("dados_sp.RData")

# Juntando a base de dados ao shapefile do Estado de SP:
shp_sp@data %>% 
  rename(codigo = 2) %>% 
  mutate(codigo = as.numeric(codigo)) %>% 
  left_join(dados_sp, by = "codigo") -> shp_sp@data

# Verificando o join:
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Para que possamos traçar as distâncias sociais, assumindo como métrica de 
# distância social o PIB per capita das cidades de SP, devemos estipular uma
# distância de corte di(k). Para o caso, propomos como cut-off a variação de 
# 0.01 desvio-padrão. 

# Desta forma, apenas para facilitar o cálculo, vamos padronizar a variável pib 
# pelo procedimento zscores:
shp_sp@data["Zpib"] <- scale(shp_sp@data$pib)

summary(shp_sp@data)
mean(shp_sp@data$Zpib)

# A matriz de distâncias sociais pode ser obtida da seguinte maneira:
matrizW_distsocial <- distancia_social(x = shp_sp@data$Zpib, 
                                       dmin = 0, 
                                       dmax = 0.01, 
                                       style = "B")

# Para facilitar o estudo da nossa matriz W, podemos comandar:
colnames(matrizW_distsocial) <- shp_sp@data$NM_MUNICIP
rownames(matrizW_distsocial) <- shp_sp@data$NM_MUNICIP

View(matrizW_distsocial)

# A vizinhança pode ser calculada da seguinte forma:
vizinhos_distsocial <- mat2listw(x = matrizW_distsocial)


# Visualizando a vizinhança estabelecida:
plot.new()
plot(shp_sp, border = "lightgray")
plot(vizinhos.distsocial, 
     coordinates(shp_sp), 
     add = TRUE, 
     col = "#FDE725FF")


# Padronização de Matrizes Espaciais --------------------------------------

# 01) Padronização em Linha da Matriz W -----------------------------------

# Utilizaremos o objeto vizinhos_queen:
vizinhos_queen

# A matriz binária W de pesos espaciais do objeto vizinhos_queen está no objeto 
# matrizW_queen.

# Para a definição da matriz W padronizada em linha:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)

# Para uma melhor visualização do novo objeto matrizW_queen_linha:
colnames(matrizW_queen_linha) <- rownames(matrizW_queen_linha)

View(matrizW_queen_linha)


# 02) Dupla Padronização da Matriz W --------------------------------------

# Para a definição da matriz W duplamente padronizada:
matrizW_queen_dupla_padr <- nb2mat(vizinhos_queen,
                                   style = "U",
                                   zero.policy = TRUE)

# Para uma melhor visualização do novo objeto matrizW_queen_dupla_padr:
colnames(matrizW_queen_dupla_padr) <- rownames(matrizW_queen_dupla_padr)

View(matrizW_queen_dupla_padr)


# 03) Padronização da Matriz W pela Estabilização da Variância ------------

# Para a definição da matriz W duplamente padronizada:
matrizW_queen_est_var <- nb2mat(vizinhos_queen,
                                style = "S",
                                zero.policy = TRUE)

# Para uma melhor visualização do novo objeto matrizW_queen_est_var:
colnames(matrizW_queen_est_var) <- rownames(matrizW_queen_est_var)

View(matrizW_queen_est_var)


# Técnicas para Verificação da Autocorrelação Espacial --------------------

# Antes de dar prosseguimento aos estudos das autocorrelações espaciais sobre
# a variável IDH, vamos observar alguns comportamentos:
tm_shape(shp = shp_sp) +
  tm_fill(col = "idh", style = "quantile", n = 5, palette = "-magma") +
  tm_borders()


# 01) Autocorrelação Global – a Estatística I de Moran --------------------

# Para o cálculo da Estatística I de Moran, nosso algoritmo esperará como
# declaração um objeto de classe listw. Como exemplificação, voltaremos a 
# utilizar o objeto matrizW_queen:
listw_queen <- mat2listw(matrizW_queen)
class(listw_queen)

# Após isso, poderemos utilizar a função
moran.test(x = shp_sp@data$pib, 
           listw = listw_queen, 
           zero.policy = TRUE)


# 02) O Diagrama da Estatística I de Moran --------------------------------
moran.plot(x = shp_sp@data$idh, 
           listw = listw_queen, 
           zero.policy = TRUE,
           xlab = "IDH", 
           ylab = "IDH Espacialmente Defasado",
           pch = 19)


# 03) Autocorrelação Local – a Estatística Moran Local --------------------

# Seguindo o proposto por Anselin (1995), devemos padronizar em linha nossa 
# matriz de pesos espaciais W:
matrizW_queen_linha <- nb2mat(vizinhos_queen,
                              style = "W",
                              zero.policy = TRUE)

# Considerando a variável idh do objeto SP.dados, podemos aferir sua Estatística 
# Moran Local, com o uso da função localmoran(), como se segue:
moran_local <- localmoran(x = shp_sp@data$idh, 
                          listw = listw_queen, 
                          zero.policy = TRUE)

# Juntando os resultados da Estatística Moran Local no dataset do objeto shp_sp:
moran_local_mapa <- cbind(shp_sp, moran_local)

# Plotando a Estatística Moran Local de forma espacial:
tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "Ii", style = "quantile", n = 5, palette = "-magma") +
  tm_borders()

# Notaram o erro na escala de cores?

# Corrigindo:
quantile(moran_local_mapa@data$Ii, type = 5, probs = c(0,0.2,0.4,0.6,0.8))

moran_local_mapa@data <- moran_local_mapa@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = Ii, q = 5))) 

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "faixa_quantis", palette = "-magma") +
  tm_borders()

# Outra coisa interessante sobre o objeto moran_local
attr(x = moran_local, which = "quadr")

moran_local_mapa <- cbind(moran_local_mapa, 
                          attr(x = moran_local, which = "quadr")[1])

tm_shape(shp = moran_local_mapa) +
  tm_fill(col = "mean", palette = "-viridis") +
  tm_borders(col = "gray")


# 04) Clusterização LISA --------------------------------------------------

# O primeiro passo é o estabelecimento de um objeto que reservará espaços para 
# conter, no futuro, os quadrantes AA, AB, BA e BB:
quadrantes <- vector(mode = "numeric", length = nrow(moran_local))

quadrantes

# Criando um vetor que contenha o centro das observações da variável idh ao 
# redor de sua média:
centro_idh <- shp_sp@data$idh - mean(shp_sp@data$idh)

centro_idh

# Criando um vetor que contenha o centro dos valores da Estatística Moran Local 
# em torno de sua média:
centro_moran_local <- moran_local[,1] - mean(moran_local[,1])

centro_moran_local

# Criando um objeto que guarde a significância a ser adotada:
sig <- 0.05

# Enquadrando nossas observações em seus respectivos quadrantes:
quadrantes[centro_idh > 0 & centro_moran_local > 0] <- "HH"
quadrantes[centro_idh > 0 & centro_moran_local < 0] <- "HL"
quadrantes[centro_idh < 0 & centro_moran_local > 0] <- "LH"
quadrantes[centro_idh < 0 & centro_moran_local < 0] <- "LL"

quadrantes

# Ajustando a presença da observação em razão de sua significância estatística:
quadrantes[moran_local[,5] > sig] <- "Estatisticamente_não_significante"

quadrantes

# Juntando o objeto quadrantes ao objeto moran_local_mapa
moran_local_mapa@data["quadrantes"] <- factor(quadrantes)

# Plotando os quadrantes de forma espacial (versão 'default'):
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "darkred",
                      HL = "red", 
                      LH = "lightblue", 
                      LL = "darkblue",
                      Estatisticamente_não_significante = "white")) +
  tm_borders()

# Versão do gráfico anterior para daltônicos:
tm_shape(shp = moran_local_mapa) +
  tm_polygons(col = "quadrantes", 
              pal = c(HH = "#FDE725FF",
                      HL = "#7AD151FF", 
                      LH = "#2A788EFF", 
                      LL = "#440154FF",
                      Estatisticamente_não_significante = "white")) +
  tm_borders()

# 05) Autocorrelação Local -  A Estatística G de Getis e Ord --------------

# A Estatística G de Getis e Ord, por definição, espera uma matriz espacial
# W de distâncias geográficas. Utilizaremos o objeto matrizW_distancias.

# Lembre-se de que o objeto matrizW_distancias foi calculado em razão de um
# shapefile do Estado da Bahia!

# Vamos importar os dados de IDH dos municípios da Bahia:
load("dados_ba.RData")

# Vamos adicionar os dados carregados ao nosso shapefile:
shp_ba@data %>% 
  left_join(dados_ba, by = "Codigo") ->shp_ba@data

# Um último ajuste é a transformação do objeto matrizW_distancias em um
# objeto de classe listw:
listw_dist <- mat2listw(x = matrizW_distancias)

# Calculando a Estatística G de Getis e Ord:
g_local <- localG(x = shp_ba@data$idh,
                  listw = listw_dist)

# Juntando as informações do objeto g_local ao nosso shapefile:
mapa_G <- cbind(shp_ba, as.matrix(g_local)) 

head(mapa_G@data)

# Renomeando a nova variável para facilitar:
mapa_G@data %>% 
  rename(estistica_g = 4) -> mapa_G@data

# Plotando a Estatística G de forma espacial:
tm_shape(mapa_G) + 
  tm_fill("estistica_g", 
          palette = "-viridis") + 
  tm_borders()

# Mais uma vez, o erro a respeito da quantidade de cores. Vamos ajustar:
mapa_G@data <- mapa_G@data %>% 
  mutate(faixa_quantis = factor(quantcut(x = estistica_g, q = 8))) 

# Plotando a Estatística G de forma espacial (versão 'default'):
tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "-RdBu") + 
  tm_borders()

# Versão do gráfico anterior para daltônicos>
tm_shape(mapa_G) + 
  tm_fill("faixa_quantis", 
          palette = "plasma") + 
  tm_borders()

# FIM ---------------------------------------------------------------------