source("ReadData.R")
source("CalculateBetasContinuous.R")
source("CalculateBetasDiscrete.R")
source("StructuralBreaks.R")
source("InverseTransform.R")

plot(vector, type="l", xlab = "Year", ylab = "GDP per year",
     ylim=c(min(vector), max(vector)))

lower <- min(vector)
height <- (max(vector) - min(vector)) / 5
upper <- lower + height

#Obliczenie kontekstu danych
context<-SetContext(vector)

#Obliczenie indeksow Fuzzy Sets, dla ktorych
#wystepuja przelomy w wykresie
breakthroughs_cont <- FindStructuralBreaks(betasOne_cont, context)
breakthroughs_discrete <- FindStructuralBreaks(betasOne_discrete, context)

breakthroughs_cont <- ((breakthroughs_cont-1)*h) + 1


#Rysowanie wykresow
for (year in breakthroughs_cont){
  xx <- c(year-h, year, year+h)
  yy <- c(lower, upper, lower)
  polygon(xx, yy, col="yellow")
}
domain
for (year in domain){
 segments(year -h, lower, year, upper, lwd=1, col="black")
  segments(year, upper, year + h, lower, lwd=1, col="black")
}

abline(h=lower, lwd=2, col="black")

lines(seq_along(vector), vector, lwd = 2)

inverseDiscrete<-FindInverseFTransform(betasZero_discrete,betasOne_discrete)
inverseContinuous<-FindInverseFTransform(betasZero_cont,betasOne_cont)


##plot(1:68,inverseDiscrete(1:68))
