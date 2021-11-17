#Data Wrangling II - 16/nov/21

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

