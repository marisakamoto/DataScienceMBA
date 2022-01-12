# Data Wrangling no R

# Atividade de Análise nº 3 - Dataset WDI World Bank
# O dataset contém muitos indicadores sobre o desenvolvimento dos países
# https://databank.worldbank.org/source/world-development-indicators

library(readxl)

dataset_wdi <- read_excel("(3.2) WDI World Bank Wrangling.xlsx")

dim(dataset_wdi)

# Existem 6 colunas e 383.838 linhas

View(dataset_wdi)

# O dataset está estruturado de modo que, para cada país, existem várias linhas
# Cada linha representa um tipo de indicador existente na fonte consultada
# Da forma como está, é difícil realizar análises com tais indicadores

# Vamos analisar um pouco melhor o conteúdo do dataset:

unique(dataset_wdi$`Country Name`)
unique(dataset_wdi$`Series Name`)
options(max.print = 3000)
unique(dataset_wdi$`Series Name`) # após ajuste, aparecem todos os indicadores
unique(dataset_wdi$`Topic`)

# Temos informações para o ano de 2019 em muitos indicadores de vários tópicos
# Também podemos notar que existem missing values marcados como ".."

# Para facilitar, vamos iniciar simplificando os nomes das colunas

dataset_wdi <- dataset_wdi %>% rename(pais=1,
                                      cod_pais=2,
                                      serie=3,
                                      cod_serie=4,
                                      ano_2019=5,
                                      topico=6)

glimpse(dataset_wdi)

# Temos um problema com a variável que contém as informações de 2019
# Ela está no formato de caracteres, provavelmente, devido aos missings ".."

# Vamos utilizar o "mutate" em conjunto com a função "na_if"
# "na_if" substitui determinada informação por NAs

dataset_wdi <- dataset_wdi %>% mutate(ano_2019 = na_if(ano_2019, ".."))

ano_2019_aj <- as.numeric(dataset_wdi$ano_2019)

dataset_wdi <- dataset_wdi %>% mutate(ano_2019_aj) %>% 
                               select(everything(),-ano_2019)

glimpse(dataset_wdi)

# Na prática, a primeira linha não seria necessária neste caso
# O própio as.numeric já converteria, por coerção, os ".." em NAs

# Vamos continuar organizando o dataset, colocando em uma estrutura mais prática

unique(dataset_wdi$topico)

# Vamos supor que o objetivo seja analisar informações do tópico "saúde"
# Existem vários tópicos sobre saúde, mas com diversos subtópicos
# Vamos filtrar todos os tópicos sobre saúde utilizando "str_detect"
# A seguir, vamos pedir todos os tópicos que começam com "saúde"

dataset_wdi_saude <- dataset_wdi %>% filter(str_detect(topico, "^Health"))

unique(dataset_wdi_saude$topico)

# Neste momento, já temos um dataset mais ajustado

# Porém, uma estrutura mais interessante seria colocar as séries nas colunas
# Assim, elas se tornariam variáveis e teríamos uma linha para cada país

# Uma função que pode ser utilizada, parte do tidyverse (tidyr), é "pivot_wider"

dataset_wdi_saude_wide <- pivot_wider(dataset_wdi_saude,
                                       id_cols = c("pais", "cod_pais"),
                                       names_from = "serie",
                                       values_from = "ano_2019_aj")

# Pode ser útil manter os tópicos de cada variável, por exemplo:

dataset_wdi_saude_final <- pivot_wider(dataset_wdi_saude,
                                      id_cols = c("pais", "cod_pais"),
                                      names_from = c("topico", "serie"),
                                      values_from = "ano_2019_aj")

# Por fim, vamos adicionar a categoria "income group" ao dataset

income <- read_excel("(2.3) WBD PIB per Capita.xls")

dataset_wdi_saude_final <- dataset_wdi_saude_final %>% 
                            rename("Country Code" = cod_pais) %>%
                            left_join(income, 
                                      by = "Country Code") %>% 
                            select(everything(),-c("Country Name","2019"))
