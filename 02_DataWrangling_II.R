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

# filtering data with the basic R function
glimpse(ds_start)
filter_1 <- ds_start[ds_start$Time < 20,] 
head(filter_1)

filter_2 <- ds_start %>%  filter(Period == "Manhã" & between(Time, 20, 50))
glimpse(filter_2)

filter_3 <- ds_start %>%  filter(Period != "Manhã" | Time > mean(Time, na.rm = T))
glimpse(filter_3)

# you can filter among groups. For example the mean among each Period was 22.4 for Manhã and 48 for Tarde

Period_group <- ds_start %>% group_by(Period)  %>%  summarise(mean = mean(Time)) %>% 
  ungroup() %>% droplevels(.)
View(Period_group)
filter_4 <- ds_start %>% group_by(Period) %>% filter(Time > mean(Time, na.rm = T)) %>% 
  ungroup() %>% droplevels(.)
View(filter_4)

# Slice - it filters based on position
ds_start %>% slice(5:9)
ds_start %>% slice_head(n=10)
ds_start %>% slice_tail(n=3)
ds_start %>% slice_min(order_by = Distance, prop = 0.2) #prop is the percentage min
ds_start %>% slice_max(order_by = Distance, prop = 0.1)

#Join tables
# First, lets import another database
dataset_merge <- read_excel("(1.3) Dataset Aula Data Wrangling (Join).xls")

# we need to change the key name to match the other database
dataset_merge <- dataset_merge %>% rename(Student = Estudante)
head(dataset_merge)

#now we can left join

dataset_start %>% left_join(dataset_merge, by = 'Student')

#now we can right join

dataset_start %>% right_join(dataset_merge, by = 'Student')


#now we can inner join

dataset_start %>% inner_join(dataset_merge, by = 'Student')

#now we can full join

dataset_start %>% full_join(dataset_merge, by = 'Student')

# Other types of JOIN: Semi and Anti (They do not join!)
# Semi join compare but doesnt join datasets, it will display the xerything that is in dataset_start, and also in merge

dataset_start %>% semi_join(dataset_merge, by = 'Student')

#anti join will show the rows that are not in the merged dataset, this case Antonio.

dataset_start %>% anti_join(dataset_merge, by = 'Student')


#--------------------Bind-------------------------------------------------------

# Existem formas simples de combinar datasets, adequados em casos particulares
#  "bind" mix datasets without a key
# this means variables and observations must be in the same order

# some examples

dataset_bind_1 <- tibble(var1 = c("obs1", "obs2", "obs3", "obs4"),
                         var2 = 1:4,
                         var3 = 10:13)

dataset_bind_2 <- tibble(var4 = c("obs1", "obs2", "obs3", "obs4"),
                         var5 = 100:103)

dataset_bind_3 <- tibble(var6 = c("obs50", "obs51", "obs52", "obs53"),
                         var7 = 1500:1503)

dataset_bind_4 <- tibble(var1 = c("obs5", "obs6", "obs7", "obs8", "obs9"),
                         var2 = 5:9,
                         var3 = 14:18)

# Bind cols (variables): same number of oservations and the same order

dataset_bind_col <- bind_cols(dataset_bind_1, dataset_bind_2)

# this case is wrong, observations do not match

dataset_bind_1 %>% bind_cols(dataset_bind_3)

# bind rows (observations): var in the same order
dataset_bind_linhas <- bind_rows(dataset_bind_1, dataset_bind_4)


#=====================Exercise 2.1. ====================#


covid_raw <- read.csv("E:/DataScienceMBA/RStudio/DataScienceMBA/DataSource/(2.2) WHO COVID-19 Global Table.csv", 
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

# lets merge with the PIB database

PIB2019 <- read_excel("E:/DataScienceMBA/RStudio/DataScienceMBA/DataSource/(2.3) WBD PIB per Capita.xls")
glimpse(PIB2019)
glimpse(covid_ds)

covid_ds_2 <- PIB2019 %>% rename(country = "Country Name") %>% 
  right_join(covid_ds, by = "country") %>% 
  select(everything(), -`Country Code`) %>% 
  rename(income = 'Income group') %>% 
  mutate(income = recode(income,
                         "High income" = "High", 
                         "Upper middle income" ="Upper middle",
                         "Lower middle income" = "Lower middle", 
                         "Low income" = "Low"))
head(covid_ds_2)

#now lets group by the new info

covid_ds_3 <- covid_ds_2 %>% group_by(income) %>% 
  summarise(ave_cases = mean(cases_relative, na.rm = T), 
            sd_cases = sd(cases_relative, na.rm = T), 
            obs = n()) %>% 
  ungroup() %>% droplevels(.) 
head(covid_ds_3)

#plot
glimpse(covid_ds_3)

covid_ds_3$income <- factor(covid_ds_3$income ,levels =  c('High', 'Upper middle', 'Lower middle', 'Low')) 

 covid_ds_3 %>% 
  ggplot()+ 
  geom_col(aes(x = income, y = ave_cases), fill = "yellow") +
  labs(x = "PIB Income Group", 
       y = "Average of Relative Cases",
       title = "Covid Cases per PIB")



#=====================Exercise 6.1 ====================#
# Database with info about movies and series 
movies <- read.csv("E:/DataScienceMBA/RStudio/DataScienceMBA/DataSource/(6.2) Filmes Streaming.csv")
series <- read.csv("E:/DataScienceMBA/RStudio/DataScienceMBA/DataSource/(6.3) Séries Streaming.csv")

glimpse(movies)
glimpse(series)

# select all variables that are common to both of them
cinema <- movies %>% select(everything(), -(Directors:Runtime)) %>% 
  bind_rows(series) %>% # pile lines that are in the same order
  select(!X) #Select everything that is not X that is present in both of them 

View(cinema)

# Transforming the string to numeric (Rates in the platforms)
?str_sub
num_IMDb = as.numeric(str_sub(cinema$IMDb, 1 ,3))
num_IMDb

num_Rotten = as.numeric(str_sub(cinema$Rotten, 1 ,2))
cinema <- cinema %>% mutate(num_IMDb, num_Rotten)

# Create a dummy for the type of media
cinema_type <- cinema %>%  mutate(Type = replace(Type, Type ==0 , "series"), 
                             Type = replace(Type, Type == 1, "movie")) %>% 
  group_by(Type) %>% 
  summarise(aveIMDb = mean(num_IMDb, na.rm = T), 
            aveRotten = mean(num_Rotten, na.rm = T), 
            p95IMDb = quantile(num_IMDb, probs = 0.95, type = 5, na.rm = T),
            p95Rotten = quantile(num_Rotten, probs = 0.95, type = 5, na.rm = T)) %>% 
  ungroup() %>% droplevels(.)

View(cinema_type)
# Now lets cut 95% to create a database with the best movies and series


top_series <- cinema %>% filter(Type == 1) %>% 
mutate(topIMDB = cut(num_IMDb, 
                     c(-Inf, quantile(num_IMDb, probs = 0.95, type = 5, na.rm = T), Inf), #percentile 95
                     c(0,1)),
       topRotten = cut(num_Rotten,
                       c(-Inf, quantile(num_Rotten, probs = 0.95, type = 5, na.rm=T), Inf),
                       c(0,1))) %>% 
filter(topIMDB==1 & topRotten ==1)
  
View(top_series)


top_movies <- cinema %>% filter(Type == 0) %>% 
  mutate(topIMDB = cut(num_IMDb, 
                       c(-Inf, quantile(num_IMDb, probs = 0.95, type = 5, na.rm = T), Inf), #percentile 95
                       c(0,1)),
         topRotten = cut(num_Rotten,
                         c(-Inf, quantile(num_Rotten, probs = 0.95, type = 5, na.rm=T), Inf),
                         c(0,1))) %>% 
  filter(topIMDB==1 & topRotten ==1)

View(top_movies)

#=====================Exercise 2.3. ====================#


