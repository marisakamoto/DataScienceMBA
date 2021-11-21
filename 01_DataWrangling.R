# November 09th, 2021
# Subject: Data Wrangling

#to install packages do in te console: install.packages('tidyverse')
library(tidyverse)
library(readxl)
library(dplyr)

#reading the data
dataset_start <- read_excel("E:/DataScienceMBA/RStudio/DataScienceMBA/02_DataWrangling/(1.2) Dataset Aula Data Wrangling.xls")


#Visualization

head(dataset_start, n = 10)
View(dataset_start)
str(dataset_start)
glimpse(dataset_start)
print(dataset_start)
dim(dataset_start)
names(dataset_start)

# To print just one variable
dataset_start$`Período do dia`
dataset_start$Estudante

# Rename dataset columns to a simpler one


new_name <- c('Student', 
              'TimeToSchool', 
              'DistanceToSchool',
              'TrafficLight', 
              'Period', 
              'DrivingProfile')

names(dataset_start)<-new_name

head(dataset_start)

dataset_renamed <- rename(dataset_start,
                          MinToSchool = TimeToSchool, 
                          KmToSchool = DistanceToSchool)
                          
#Pipe shortcut Ctrl + Shift+M
dataset_pipe <- dataset_renamed %>% rename(TimeToSchool = MinToSchool, 
                          DistanceToSchool = KmToSchool)

#Mutate: Add new variables to the dataset or change the contents of a variable

new_var_1 <- 1:10
new_var_2 <- 11:20
print(new_var_1)

dataset_mutate <- mutate(dataset_start, new_var_1, new_var_2)
head(dataset_mutate)

dataset_mutate %>% rename(seq = new_var_1) %>% mutate(DoubleTimeToSchool = 2 * TimeToSchool)

#Replace (Change the content of a variable in itself)

dataset_replace <- dataset_renamed %>% mutate(DrivingProfile = replace(DrivingProfile, DrivingProfile == 'calmo', 'relax'),
                                              DrivingProfile = replace(DrivingProfile, DrivingProfile == 'moderado', 'moderate'),
                                              DrivingProfile = replace(DrivingProfile, DrivingProfile == 'agressivo', 'agressive'),
                                              
)
head(dataset_replace)

#Recode (Change the content of a variable in another var)

dataset_recode <- dataset_renamed %>% mutate(stressLevel = 
                                               recode(DrivingProfile, 
                                                      'calmo' = 1,
                                                      'moderado' = 2,
                                                      'agressivo' = 3),
                                             afternoon = recode(Period, 'Manhã' = 0, 'Tarde' = 1))
head(dataset_recode)

#Create a dummy base

dummy_profile <- mutate(dataset_recode, 
                        Relax = recode(stressLevel, '1' = 1, '2' = 0, '3' = 0),
                        Moderate = recode(stressLevel, '1' = 0, '2' = 1, '3' = 0),
                        Agressive = recode(stressLevel, '1' = 0, '2' = 0, '3' = 1))

View(dummy_profile)


#Transmute: include variables and exclude others

dataset_exclude <- dataset_start %>% transmute(Student, TimeToSchool, DistanceToSchool, DrivingProfile,
                                               new_var_1)
View(dataset_exclude)

#cut: classify the data

dataset_time <- dataset_exclude %>% mutate(time_rank = cut(TimeToSchool, 
                                                         c(0, median(TimeToSchool), Inf), 
                                                         c('faster', 'slower')),
                                           dist_rank = cut(DistanceToSchool, 
                                                           c(0, median(DistanceToSchool), Inf), 
                                                           c('near', 'far')))
View(dataset_time)

#Slice: raw R
select_1<- dataset_time[,c("Student", "time_rank")]
print(select_1)

select_2<- dataset_time[ 2:5 ,1:4]
print(select_2)

#Select: used to extract selected variabes or to reposition variables
base_select_1<- dataset_time %>%select(Student, time_rank)
print(base_select_1)

base_select_2<- dataset_time %>%select(everything(), -DrivingProfile)
print(base_select_2)

base_select_3<- dataset_time %>%select(Student:DistanceToSchool)
print(base_select_3)

base_select_4<- dataset_time %>%select(starts_with("S")) #print variables that start with S
print(base_select_4)

#How we can reposition variables:

dataset_time %>% select(Student, time_rank, dist_rank, everything())

# Or by relocating

dataset_time %>% relocate(DistanceToSchool, .after = dist_rank) %>% 
                          relocate(TimeToSchool, .after = time_rank)

# Pull: similar to select but return an array:
array_pull <- dataset_time %>% pull(var = 1)
array_pull

#======================Example 2.2 ==================================

covid_source <- read.csv('E:/DataScienceMBA/RStudio/DataScienceMBA/02_DataWrangling/(2.2) WHO COVID-19 Global Table.csv',
                         header = TRUE,
                         sep = ',',
                         dec = '.')
dim(covid_source)
names(covid_source)
head(covid_source)
View(covid_source)

covid_source <- covid_source %>% rename(country = 1,
                                        region = 2,
                                        casesCumTotal = 3,
                                        casesCum10000 = 4,
                                        newCasesLastWeek = 5,
                                        newCasesLastWeek10000 = 6,
                                        newCasesLastDay = 7,
                                        deathsTotal = 8,
                                        deaths10000 = 9,
                                        deathsLastWeek = 10,
                                        deathsLastWeek10000 = 11,
                                        deathsLastDay = 12,
                                        transmission = 13)
#visualization
table(covid_source$casesCum10000)
unique(covid_source$transmission)
View(covid_source)

#cleaning data
covid_source <- covid_source[-c(1),] #deletes first line
covid_source <- covid_source[!(covid_source$country == 'Other'),] #deletes by name

#slicing the data
covid_source <- covid_source %>%  mutate(relativeCases =
                                           cut(casesCum10000,
                                               c(-Inf, quantile(covid_source$casesCum10000,
                                                                type = 5,
                                                                probs = c(0.25, 0.5, 0.75),
                                                                TRUE),
                                                 Inf),
                                               c('quartileFirst', 'quartileSecond', 'quartileThird', 'quartileFourth')
                                           ))
#type = 5, you can do F1 to get assistance from help

table(covid_source$relativeCases)


#manipulate the data to exclude deathsDay and add groups to the beginning to the dataset
covid_source <- covid_source %>% select(country, region, relativeCases, everything(),-deathsLastDay)
View(covid_source)

#EXAMPLE 3 ================


championsLeague <- read.csv('E:/DataScienceMBA/RStudio/DataScienceMBA/02_DataWrangling/(5.2) Champions League 2020-2021.csv',
                         header = TRUE,
                         sep = ',',
                         dec = '.')
championsLeague <- championsLeague %>% 
  mutate(winner = case_when(
    c(championsLeague$team_home_score - championsLeague$team_away_score) == 0 ~'tie',
    c(championsLeague$team_home_score - championsLeague$team_away_score) > 0 ~'house',
    c(championsLeague$team_home_score - championsLeague$team_away_score) < 0 ~'visitor')) %>% 
  relocate(winner, .after = team_away_score)

table(championsLeague$winner)

