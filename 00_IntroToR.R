#capital letters matter
object <- 27
object

Object <- 21
Object

#important values

TRUE #logical
FALSE #logical
T #logical
F #logical
NA #logical
NULL # it desconsider the value
Inf #numeric +infinite
-Inf #numeric -infinite

#important functions
round(3.14152)
round(3.14152, digits = 3)
?round
args(round)

#coertion: if you try to mix numeric and text types, R will rank them in char, numeric and logical
#if you have one char value in a multiple array of numeric it wil change all type to char
names_ages <- c('mari', 27, 'john', 60, 'andre', 45)
names_ages
class(names_ages)

numeric_bool <- c(2, 55, TRUE, FALSE)
class(numeric_bool)
numeric_bool

#categorical variables
blood_type <- c('A+','A-','B+','B-','AB+','AB-','O+','O-')
blood_type <- factor(blood_type)
blood_type

#categorical variables ordered 
education_level <- c('primary', 'secondary', 'higher education')
education_level <- factor(education_level,
                          levels =  c('primary', 'secondary', 'higher education'))
education_level #if you dont specify R will order in ASCII

#creating a dataset

company <- c('Company A', 'Company B', NA, 'Company C', 'Company D')
employers <- c(100, 4000, 200, 15, 35000)
stock_precence <- c(F, T, NA, T, T)
national <- c(0,1,1,NA,0)
ceo_name <- c(NA, 'Joana', 'Carolina','Mary', 'Elizabeth')

##Verify if has the same length
length(company)
length(employers)
length(stock_precence)
length(national)
length(stock_precence)

df_companies <- data.frame( company, employers, stock_precence, national, ceo_name)
df_companies
View(df_companies) #Use only if the data set is small

## rename the variables

df_companies <- data.frame(company,employees= employers, 
                           stock_exchange = stock_precence, brazil_hq = national
                           ,ceo_name)

# Saving objects data from R
save(df_companies, file = "df_companies.RData")
# To load it again
load('df_companies.RData')

load('spam.RData')
head(spam)


