source("ReadData.R")
source("CalculateBetasContinuous.R")
source("CalculateBetasDiscrete.R")
source("StructuralBreaks.R")
source("InverseTransform.R")

plot(first_year + seq_along(vector), vector, type="l", xlab = "Year", ylab = paste("GDP per year for", country),
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
breakthroughs_discrete <- ((breakthroughs_discrete-1)*h) + 1

print('Betas Calculated with Continuous method')
betasOne_cont
print('Betas Calculated with Discrete method')
betasOne_discrete
print('Breathroughs Calculated with Continuous method')
breakthroughs_cont
print('Breathroughs Calculated with Discrete method')
breakthroughs_discrete


offset <- first_year
draw_triangles <- function(sets, color){
  for (year in sets){
    xx <- c(offset + year-h, offset + year, offset + year+h)
    yy <- c(lower, upper, lower)
    polygon(xx, yy, col=color)
  }
}

draw_triangles(breakthroughs_discrete, 'red4')
draw_triangles(breakthroughs_cont, 'seagreen')
draw_triangles(intersect(breakthroughs_discrete, breakthroughs_cont), 'yellow2')

for (year in domain){
 segments(offset + year -h, lower, offset + year, upper, lwd=1, col="black")
  segments(offset + year, upper, offset + year + h, lower, lwd=1, col="black")
}
abline(h=lower, lwd=2, col="black")

#Rysowanie wykresow
inverseDiscrete<-FindInverseFTransform(betasZero_discrete,betasOne_discrete)
inverseContinuous<-FindInverseFTransform(betasZero_cont,betasOne_cont)

func_domain <- seq_along(vector)
lines(offset + func_domain, inverseDiscrete(func_domain), lwd = 2, col = 'red')
lines(offset + func_domain, inverseContinuous(func_domain), lwd = 2, col = 'green')
lines(offset + func_domain, vector, lwd = 2)

legend("topleft", inset = .05, legend=c("Continuous Method", "Discrete Method", "Both"), title = "   Legend     ",
       bg='lightblue', lty=1, bty = "0", text.width = 15, y.intersp = 1.5,
       col=c("seagreen", "red4", "yellow2"), lwd=5, cex = 1)
