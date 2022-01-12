# Data Wrangling no R

# Atividade de Análise nº 6 - Datasets de filmes e séries
# Os datasets contêm informações de filmes e séries disponíveis em streamings
# Fonte: https://www.kaggle.com/datasets, enviados por Ruchi Bhatia

filmes <- read.csv("(6.2) Filmes Streaming.csv")
series <- read.csv("(6.3) Séries Streaming.csv")

library(tidyverse)

glimpse(filmes)
glimpse(series)

# Os dois datasets têm estruturas semelhantes quanto às variáveis
# Porém, o dataset sobre flimes tem colunas a mais
# Vamos fazer uma rápida organização dos datasets e juntá-los

completo <- filmes %>% select(everything(), -(Directors:Runtime)) %>% 
                       bind_rows(series) %>% 
                       select(!X)

# O banco de dados contém avaliações das agências Rotten Tomatoes e IMDb
# Vamos procurar os melhores classificados em ambas as agências

# O primeiro passo é ajustar as duas variáveis com notas, pois são texto

ajuste_imdb <- as.numeric(str_sub(completo$IMDb, 1, 3))
ajuste_rotten <- as.numeric(str_sub(completo$Rotten.Tomatoes, 1, 2))

completo <- completo %>%  mutate(IMDB = ajuste_imdb,
                                 ROTTEN = ajuste_rotten)

# Em seguida, vamos verificar as médias e as médias por tipo (filme ou série)
# E vamos procurar um ponto de corte mais restritivo, o percentil 95

completo %>% mutate(Type = replace(Type, Type==1, "serie"), 
                    Type = replace(Type, Type==0, "filme"))  %>%
  group_by(Type) %>%
  summarise(mediaIMDB=mean(IMDB, na.rm=T),
            mediaROTTEN=mean(ROTTEN, na.rm=T),
            p95IMDB=quantile(IMDB, probs=0.95, type=5, na.rm=T),
            p95ROTTEN=quantile(ROTTEN, probs=0.95, type=5, na.rm=T)) %>%
  ungroup() %>% droplevels(.)


# Vamos gerar os datasets com os "melhores" filmes e séries

melhores_series <- completo %>% filter(Type==1) %>% 
  mutate(melhoresIMDb = cut(IMDB, 
                                 c(-Inf,
                                 quantile(IMDB, probs=0.95, type=5, na.rm=T),
                                 Inf),
                                 c(0,1)),
         melhoresTOTTEN = cut(ROTTEN,
                                 c(-Inf,
                                 quantile(ROTTEN, probs=0.95, type=5, na.rm=T),
                                 Inf),
                                 c(0,1))) %>% 
  filter(melhoresIMDb==1&melhoresTOTTEN==1)

melhores_filmes <- completo %>% filter(Type==0) %>% 
  mutate(melhoresIMDb = cut(IMDB, 
                            c(-Inf,
                              quantile(IMDB, probs=0.95, type=5, na.rm=T),
                              Inf),
                            c(0,1)),
         melhoresTOTTEN = cut(ROTTEN,
                              c(-Inf,
                                quantile(ROTTEN, probs=0.95, type=5, na.rm=T),
                                Inf),
                              c(0,1))) %>% 
  filter(melhoresIMDb==1&melhoresTOTTEN==1)
