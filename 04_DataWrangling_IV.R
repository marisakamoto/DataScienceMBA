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

map_dbl(school_transport[input], mean, na.rm = T)



