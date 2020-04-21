source("ReadData.R")
source("CalculateBetas.R")
source("StructuralBreaks.R")

plot(vector, type="l", xlab = "Year", ylab = "GDP per year",
     ylim=c(min(vector), max(vector)))

lower <- min(vector)
height <- (max(vector) - min(vector)) / 5
upper <- lower + height

context<-SetContext(vector)

breakthroughs <- FindStructuralBreaks(res,context,length(vector))
breakthroughs <- ((breakthroughs-1)*h) + 1

for (year in breakthroughs){
  xx <- c(year-h, year, year+h)
  yy <- c(lower, upper, lower)
  polygon(xx, yy, col="yellow")
}
domain
for (year in domain){
 segments(year -h, lower, year, upper, lwd=2, col="black")
  segments(year, upper, year + h, lower, lwd=2, col="black")
}

abline(h=lower, lwd=2, col="black")

lines(seq_along(vector), vector, lwd = 2)
