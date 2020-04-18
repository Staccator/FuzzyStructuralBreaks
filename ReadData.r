read.table("time_series.xlsx", sep="\t")
read.csv("time_series.xlsx")

##Zainstalowanie paczki do łatwiejszego przegladania exceli
install.packages("readxl")
##Dołaczenie biblioteki
library("readxl")


my_data <- read_excel("time_series.xlsx", sheet = "Data")
head(my_data)
str(my_data)

##Czytanie jednej cechy danego państwa
fr_data<-my_data[my_data$country=="France",6]
fr_vector<-simplify2array(fr_data)
plot(fr_vector)



data<-my_data[my_data$country=="Aruba",6]
data[]
##Pozbycie sie NA
d<-na.exclude(data)
vec<-simplify2array(d)
plot(vec)

##Interpolacja na podstawie punktow, plus zageszczenie punktow
fun<-approxfun(vec)
dziedzina<-seq(0,50,by=0.01)
plot(fun(dziedzina))


