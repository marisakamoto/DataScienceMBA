#Data Wrangling II - 16/nov/21
#==================Exercise 1.1. =========================#
library("tidyverse")
library("dplyr")
library("readxl")

ds_start <- read_excel("(1.2) Dataset Aula Data Wrangling.xls")

new_names <- c("Obs",
                 "Time",
                 "Distance",
                 "Traffic_Lights",
                 "Period",
                 "Profile")

print(new_names)

# 2º: Em seguida, atribuimos o vetor com nomes ao dataset

names(ds_start) <- new_names

head(ds_start)

#Summarise
descript_time <- summarise(ds_start, 
                           Obs = n(),
                           Average = mean(Time),
                           Median = median(Time),
                           desv_pad = sd(Time),
                           Minimun = min(Time),
                           Max = max(Time),
                           quartile_3rd = quantile(Time, type = 5, 0.75),
                           quartile_2nd = quantile(Time, type = 5, 0.25))
print(descript_time)
#group_by

group_ds <- group_by(ds_start, Period) #R remembers this choice in the object

group_time <- group_ds %>% summarise(ave = mean(Time), sd = sd(Time), obs = n())
View(group_time)

#to ungroup
ungroup_ds <- group_ds %>% ungroup() %>% droplevels(.)
summarise(ungroup_ds, mean(Time)) #it shows the mean in the whole dataset

# To group with more than one criteria:

new_group_ds <- group_ds %>% group_by(Period, Profile) %>% 
  summarise(mean = mean(Time), 
            min = min(Time), 
            max = max(Time), 
            count = n()) %>% 
  arrange(desc(max))

View(new_group_ds)

#=====================Exercise 2.1. ====================#


covid_raw <- read.csv("(2.2) WHO COVID-19 Global Table.csv", 
                       header = TRUE, 
                       sep = ",",
                       dec = ".")

names(covid_raw)

# Os nomes estão ruins, vamos alterar e simplificar os nomes das variáveis:

covid_ds <- covid_raw %>% rename(country = 1,
                                    region = 2,
                                    cases_total = 3,
                                    cases_relative = 4,
                                    cases_week = 5,
                                    cases_week_relative = 6,
                                    cases_day = 7,
                                    deaths_total = 8,
                                    deaths_relative = 9,
                                    deaths_week = 10,
                                    deaths_week_relative = 11,
                                    deaths_day = 12,
                                    transmission = 13)

names(covid_ds)
# A seguir, vamos alterar as categorias da variável "tipo_transimssao"
# Podemos utilizar a função mutate e, por exemplo, traduzir para português
# Também podemos criar uma categoria para a variável "casos_relativo"

# Primeiramente: identificar as categorias da variável "tipo_transimssao"

table(covid_ds$transmission)
# drop Global and Other Coutries
covid_ds <- covid_ds[-c(1),] # excluída pelo número de sua linha
covid_ds <- covid_ds[!(covid_ds$country=="Other"),] # excluída por seu nome

# Podemos trocar os nomes de "tipo_transimssao" com o "mutate" e "recode"
# Podemos criar a nova categoria para "casos_relativo" com "mutate" e "cut"

covid_ds <- covid_ds %>% mutate(transmission = recode(transmission,
                                                              "Clusters of cases" = "Casos Concentrados",
                                                              "Community transmission" = "Transmissão Comunitária",
                                                              "No cases" = "Sem Casos",
                                                              "Not applicable" = "Não Aplicável",
                                                              "Pending" = "Pendente",
                                                              "Sporadic cases" = "Casos Esporádicos")) %>% 
  mutate(groups = cut(cases_relative,
                      c(-Inf, quantile(covid_ds$cases_relative,
                                       type = 5,
                                       probs = c(0.25, 0.50, 0.75),
                                       TRUE),Inf),
                      c("1st quartile",
                        "2nd quartile",
                        "3rd quartile",
                        "4th quartile")))

# Neste caso, a função cut foi aprimorada em relação ao exemplo da aula
# Aqui, geramos as categorias com base nos quartis da variável original

table(covid_ds$groups)
View(covid_ds)
# Vamos excluir a variável "mortes_dia", pois não vamos utilizar
# Ao mesmo tempo, vamos trazer a variável "grupos" para o começo do dataset

covid_ds <- covid_ds %>% select(country, 
                                    region,
                                    groups,
                                    everything(),
                                    -deaths_day)

#lets group them by quartile

quartile_covid <- covid_ds %>%  group_by(groups) %>% 
  summarise(mean = mean(cases_relative, na.rm = T), #na.rm means to remove nulls from summary
            sd = sd(cases_relative, na.rm = T),
            obs = n()) %>% 
  ungroup() %>% droplevels(.)
View(quartile_covid)


#lets group them by region

quartile_covid <- covid_ds %>%  group_by(region) %>% 
  summarise(mean = mean(cases_relative, na.rm = T), #na.rm means to remove nulls from summary
            sd = sd(cases_relative, na.rm = T),
            obs = n()) %>% 
  arrange(desc(mean)) %>% 
  ungroup() %>% droplevels(.)

View(quartile_covid)

#=====================Exercise 6.1 ====================#

movies <- read.csv("(6.2) Filmes Streaming.csv")
series <- read.csv("(6.3) Séries Streaming.csv")

glimpse(movies)
glimpse(series)

# select all variables that are common to both of them
cinema <- movies %>% select(everything(), -(Directors:Runtime)) %>% 
  bind_rows(series) %>% # pile lines that are in the same order
  select(!X) #Select everything that is not X that is present in both of them 

View(cinema)

# Now lets cut 95%
