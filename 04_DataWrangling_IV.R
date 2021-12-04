#Aula 5 - 30Nov = Function
# Funções - Referência: (https://r4ds.had.co.nz/functions.html)

# Exemplo: todo dia atualizamos milhares de valores somando 17 e dividindo por 2
# Ao invés de repetir a mesma conta toda vez, poderíamos criar uma função:
update <- function(old) {
  new <- ((old + 17)/2)
  return(new)
}
update(1)
update(2)
update(3)

update(1:3)

# a function with two parameters, calculating  body mass index:

bmi <- function(weight, height){
  index <- (weight / height^2)

  if (index < 17) {
    print('< 17 Highly underweight')
  }
  else if (index < 18.49) {
    print('< 18.49 Underweight')
  }
  else if (index < 24.99) {
    print('< 24.99 Regular weight')
  }
  else if (index < 29.99) {
    print('< 29.99 Overweight')
  }
  else if (index < 34.99) {
    print('< 34.99 Obesity I')
  }
  else if (index < 39.99) {
    print('< 39.99 Obesity II severe')
  }
  else  {
    print('>40 Obesity III morbid')
  }
  return(index)
}
bmi (64,1.62)
bmi (80,1.80)


#Other example

max_100 <- function(old){
  new <- (old*1.2)
  if(new <= 100) {
    return(new)
  }
  else return(100)
}
max_100(20)
max_100(90)

#mixing functions in a function

percentile_var <- function(x){
  perc <- quantile(x, probs = c(0.25,0.5,0.75), type = 5, na.rm = T)
  return(perc)
}

percentile_var(cars$speed)
percentile_var(cars$dist)

#package purrr creates functions that make iterations easier
library(purrr)
library(readxl)
school_transport <- read_excel("(1.2) Dataset Aula Data Wrangling.xls")

new_names <- c("Obs",
                 "Time",
                 "Distance",
                 "TrafficLights",
                 "Period",
                 "Profile")
names(school_transport) <- new_names

head(school_transport)
input <- c('Time', 'Distance', 'TrafficLights')

#map creates lists
#map_dbl double
#map_lgl: logical
#map_chr: char
map_dbl(school_transport[input], mean, na.rm = T)
map_dbl(school_transport[input], median, na.rm = T)
map_dbl(school_transport[input], sd, na.rm = T)
map(school_transport[input], percentile_var)

#summary main statistics ~ calls a function
map(school_transport[input], ~summary(. , quantile.type = 5))

coef_var <- function(x){
  cd <- ((sd(x, na.rm = T))/(mean(x, na.rm = T)))*100
  return(cd)
}

map(school_transport[input], coef_var)

#anotehr way to do this is:
map(school_transport[input], ~(sd(., na.rm = T)/mean(., na.rm = T))*100)

#return row 5
map(school_transport, 5)
#identify the elements in each variable
map(school_transport, typeof)

#identify the unique elements
map(school_transport, unique)

#map can also get multiple inputs. If it is only 2, use map2.
#lets create variables with the following ave and sd
mean_var <- list(5,10,15)
sd_var <- list(1,2,3)

#rnorm function generates random numbers based on the statistics given
#mean_var and sd_var is an argument that varies, thats why it comes before the rnorm, n=5 is fixed, therefore goes after in the order.
map2(mean_var, sd_var, rnorm, n=5)

#more than 2 inpust, we will use pmap
size_var <- list(10,5,8)
parameters <- list(mean_var, sd_var, size_var)
pmap(parameters, rnorm)
#this way it creates a list with (1): mean = 5, sd = 1, size= 10,....

#to avoid mistakes, name the arguments
parameters2 <- list(mean = mean_var, sd = sd_var, n = size_var)
pmap(parameters2, rnorm)

#At last, you can change the function in the map
change_function <- list("rnorm", "rpois")
parameters3 <- list(
  list(mean = 10, sd= 2, n = 10),
  list(lambda = 10, n = 20)
)
invoke_map(change_function, parameters3)

?rpois

