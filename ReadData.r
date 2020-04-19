#install.packages("readxl")
library("readxl")

world_data <- read_excel("time_series.xlsx", sheet = "Data")

GetCountryGdp <- function(country){
  na.exclude(simplify2array(world_data[world_data$country==country,6]))
}

spain_gdp<-GetCountryGdp("Spain")
#plot(spain_gdp)

vector <- spain_gdp[1:20]
gdp_function<-approxfun(vector, rule = 2)
domain <- seq(1, length(vector), length.out = 15)
