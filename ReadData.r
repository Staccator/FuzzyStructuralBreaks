#Pakiet ulatwiajacy czytanie z plikow .xls
#install.packages("readxl")
library("readxl")

world_data <- read_excel("time_series.xlsx", sheet = "Data")

#Pobranie danych gdp danego panstwa
GetCountryGdp <- function(country){
  na.exclude(simplify2array(world_data[world_data$country==country,6]))
}

GetFirstYear <- function(country){
  data <- simplify2array(world_data[world_data$country==country,6])
  start <- which.min(is.na(data))
  1949 + start
}

country <- "Uruguay"
gdp_data<-GetCountryGdp(country)
first_year <- GetFirstYear(country)

vector <- gdp_data
vector_length <- length(gdp_data)


# Change percentageData to FALSE to get gdp every year (instead of percentage change of gdp)
percentageData<-TRUE

if(percentageData)
{
  vector[2:vector_length] <- (vector[2:vector_length] - vector[1:vector_length - 1])/vector[1:vector_length-1]
  vector[1]=0
  vector<-100*vector
}

#Interpolacja
gdp_function<-approxfun(vector, rule = 2)

##Podzial dziedziny na wezly 
##bedace srodkami Fuzzy Sets
domain <- seq(1, length(vector), length.out = 20)
##Dlugosc wezla
h<-domain[2]-domain[1]
