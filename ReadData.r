##Zainstalowanie paczki do łatwiejszego przegladania exceli
#install.packages("readxl")
library("readxl")

world_data <- read_excel("time_series.xlsx", sheet = "Data")

GetCountryGdp <- function(country){
  na.exclude(simplify2array(world_data[world_data$country==country,6]))
}

spain_gdp<-GetCountryGdp("Spain")
plot(spain_gdp)

##Interpolacja na podstawie punktow, plus zageszczenie punktow
vector <- spain_gdp
gdp_function<-approxfun(vector, rule = 2)
domain<-seq(1, length(vector), by=0.1)
plot(fun(domain))

domain <- seq(1, length(vector), length.out = 15)

