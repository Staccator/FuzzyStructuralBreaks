source("ReadData.r")

plot(vector, type="l", xlab = "Year", ylab = "GDP per year",
     ylim=c(min(vector), max(vector)))

lower <- min(vector)
height <- (max(vector) - min(vector)) / 5
upper <- lower + height

breathroughs <- c(3, 5, 9)
for (year in breathroughs){
  xx <- c(year-1, year, year+1)
  yy <- c(lower, upper, lower)
  polygon(xx, yy, col="yellow")
}

for (year in seq_along(vector)){
  segments(year -1, lower, year, upper, lwd=2, col="black")
  segments(year, upper, year + 1, lower, lwd=2, col="black")
}

abline(h=lower, lwd=2, col="black")
lines(seq_along(vector), vector, lwd = 2)
