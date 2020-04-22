#Pakiet ulatwiajacy czytanie z plikow .xls
#install.packages("readxl")
library("readxl")

world_data <- read_excel("time_series.xlsx", sheet = "Data")

#Pobranie danych gdp danego panstwa
GetCountryGdp <- function(country){
  na.exclude(simplify2array(world_data[world_data$country==country,6]))
}

gdp_data<-GetCountryGdp("Uruguay")


vector <- simplify2array(gdp_data)
gdp_function<-approxfun(vector, rule = 2)

##Podzial dziedziny na wezly 
##bedace srodkami Fuzzy Sets
domain <- seq(1, length(vector), length.out = 50)
##Dlugosc wezla
h<-domain[2]-domain[1]

