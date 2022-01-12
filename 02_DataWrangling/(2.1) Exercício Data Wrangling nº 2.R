# Data Wrangling no R

# Atividade de análise nº 2 - Dataset (A1) WHO COVID-19 Global Table
# O dataset contém informações sobre a pandemia do COVID-19 em diversos países
# Atualmente, as informações estão desatualizadas (mas o foco é data wrangling)
# O dataset é um arquivo CSV, com informações separadas por vírgulas

base_covid <- read.csv("(2.2) WHO COVID-19 Global Table.csv", 
                       header = TRUE, 
                       sep = ",",
                       dec = ".")

# O primeiro argumento indica o nome do dataset
# O argumento "header" indica que a primeia linha contém os nomes das variáveis
# O argumento "sep" indica que a separação das colunas é feita por vírgulas
# O argumento "dec" indica que a separação dos decimais ocorre por pontos

dim(base_covid)
names(base_covid)

# Os nomes estão ruins, vamos alterar e simplificar os nomes das variáveis:

base_covid <- base_covid %>% rename(nome = 1,
                                    regiao = 2,
                                    casos_total = 3,
                                    casos_relativo = 4,
                                    casos_semana = 5,
                                    casos_semana_relativo = 6,
                                    casos_dia = 7,
                                    mortes_total = 8,
                                    mortes_relativo = 9,
                                    mortes_semana = 10,
                                    mortes_semana_relativo = 11,
                                    mortes_dia = 12,
                                    tipo_transmissao = 13)

names(base_covid)

# A seguir, vamos alterar as categorias da variável "tipo_transimssao"
# Podemos utilizar a função mutate e, por exemplo, traduzir para português
# Também podemos criar uma categoria para a variável "casos_relativo"

# Primeiramente: identificar as categorias da variável "tipo_transimssao"

table(base_covid$tipo_transmissao)

# ou também poderia ser feito:

unique(base_covid$tipo_transmissao)

# Em uma rápida análise dos países, vemos que há "Global" e "Other"
# Antes de criar categorias para "casos_relativo", vamos excluí-los do dataset

base_covid <- base_covid[-c(1),] # excluída pelo número de sua linha
base_covid <- base_covid[!(base_covid$nome=="Other"),] # excluída por seu nome

# Podemos trocar os nomes de "tipo_transimssao" com o "mutate" e "recode"
# Podemos criar a nova categoria para "casos_relativo" com "mutate" e "cut"

base_covid <- base_covid %>% mutate(tipo_transmissao = recode(tipo_transmissao,
                                                              "Clusters of cases" = "Casos Concentrados",
                                                              "Community transmission" = "Transmissão Comunitária",
                                                              "No cases" = "Sem Casos",
                                                              "Not applicable" = "Não Aplicável",
                                                              "Pending" = "Pendente",
                                                              "Sporadic cases" = "Casos Esporádicos")) %>% 
                            mutate(grupos = cut(casos_relativo,
                                                 c(-Inf, quantile(base_covid$casos_relativo,
                                                                  type = 5,
                                                                  probs = c(0.25, 0.50, 0.75),
                                                                  TRUE),Inf),
                                                 c("primeiro quartil",
                                                   "segundo quartil",
                                                   "terceiro quartil",
                                                   "quarto quartil")))

# Neste caso, a função cut foi aprimorada em relação ao exemplo da aula
# Aqui, geramos as categorias com base nos quartis da variável original

table(base_covid$grupos)

# Vamos excluir a variável "mortes_dia", pois não vamos utilizar
# Ao mesmo tempo, vamos trazer a variável "grupos" para o começo do dataset

base_covid <- base_covid %>% select(nome, 
                                    regiao,
                                    grupos,
                                    everything(),
                                    -mortes_dia)

# Em seguida, vamos agrupar o dataset com base na variável "grupos"
# Vamos criar um dataset com informações de resumo (média, desvio padrão, ...)
# No final, realizar o ungroup para manter o dataset na estrutura original

base_quartis <- base_covid %>% group_by(grupos) %>% 
  summarise(média=mean(casos_relativo, na.rm = T),
            desvio_padrão=sd(casos_relativo, na.rm = T),
            obs.=n()) %>%
  ungroup () %>% droplevels(.)

# Como já fizemos o ungroup acima, poderíamos realizar uma análise diferente:

base_regiao <- base_covid %>% group_by(regiao) %>% 
  summarise(média=mean(casos_relativo, na.rm = T),
            desvio_padrão=sd(casos_relativo, na.rm = T),
            obs.=n()) %>%
  ungroup () %>% droplevels(.)

# Vamos adicionar duas novas variáveis ao dataset utilizando a função "join"
# As variáveis estão na planilha em Excel WBD Pib per Capita
# Vamos trazer "income group" e "PIB em 2019" utilizando "right join"

library(readxl)

PIB2019 <- read_excel("(2.3) WBD PIB per Capita.xls")

# A chave para o merge é o nome do país, mas é necessária a alteração no nome
# Vamos aproveitar o mesmo código e fazer algumas alterações adicionais

base_covid_2 <- PIB2019 %>% rename(nome="Country Name") %>%
  right_join(base_covid, by = "nome") %>% 
  select(everything(), -`Country Code`) %>% 
  rename(grupo_renda="Income group") %>% 
  mutate(grupo_renda = recode(grupo_renda,
                              "High income" = "PIB Muito Elevado",
                              "Upper middle income" = "PIB Elevado",
                              "Lower middle income" = "PIB Baixo",
                              "Low income" = "PIB Muito Baixo"))

# Como é um "right join", levamos as variáveis do PIB2019 para a base_covid
# Portanto, a base_covid_2 contém as mesmas observações da base_covid
# Note que surgem os NAs nos casos não identificados

# Vamos analisar com base na nova variável adicionada pelo merge

base_renda <- base_covid_2 %>% group_by(grupo_renda) %>% 
  summarise(média=mean(casos_relativo, na.rm = T),
            desvio_padrão=sd(casos_relativo, na.rm = T),
            obs.=n()) %>%
  ungroup () %>% droplevels(.)

# Ou mesmo um gráfico para ilustrar por imagem:

base_covid_2 %>% group_by(grupo_renda) %>% 
  summarise(média=mean(casos_relativo, na.rm = T),
            desvio_padrão=sd(casos_relativo, na.rm = T),
            obs.=n()) %>% ungroup () %>% droplevels(.) %>% 
  ggplot() + 
  geom_col(aes(x=grupo_renda, y=média), fill="orange") + 
  labs(x = "Grupo de Renda",
       y = "Média de Casos Relativos",
       title = "Análise Casos Covid")
