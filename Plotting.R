source("ReadData.R")
source("CalculateBetasContinuous.R")
source("CalculateBetasDiscrete.R")
source("StructuralBreaks.R")

plot(vector, type="l", xlab = "Year", ylab = "GDP per year",
     ylim=c(min(vector), max(vector)))

lower <- min(vector)
height <- (max(vector) - min(vector)) / 5
upper <- lower + height

context<-SetContext(vector)

breakthroughs_cont <- FindStructuralBreaks(betasOne_cont, context)
breakthroughs_discrete <- FindStructuralBreaks(betasOne_discrete, context)
breakthroughs_cont
breakthroughs_discrete
breakthroughs_cont <- ((breakthroughs_cont-1)*h) + 1

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
