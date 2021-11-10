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