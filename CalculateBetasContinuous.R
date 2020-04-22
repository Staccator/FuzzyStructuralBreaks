source("FuzzyPartition.r")
source("FuzzyTransformFunctions.r")
source("ReadData.r")
options(scipen=999)
h <- domain[2] - domain[1]
left_limits <- seq(from=domain[1]-h, length.out = length(domain), by=h)
right_limits <- seq(from=domain[1]+h, length.out = length(domain), by=h)
fuzzy_sets<-CreateFuzzySets(domain)
minus_cks<-CreateMinusCks(domain)
fuzzy_sets <- data.frame(matrix(unlist(fuzzy_sets), nrow=length(fuzzy_sets), byrow=T), stringsAsFactors=FALSE)
minus_cks <-data.frame(matrix(unlist(minus_cks), nrow=length(minus_cks), byrow=T), stringsAsFactors=FALSE)

tab<-cbind(fuzzy_sets,minus_cks,left_limits, right_limits)

betasZero_cont<-apply(tab, 1, FUN=function(x){
  integrate(multi(gdp_function, x[[1]]), x[[3]], x[[4]])[[1]]/
  integrate(x[[1]],x[[3]],x[[4]])[[1]]
})
betasOne_cont<-apply(tab, 1, FUN=function(x){
  integrate(multi(multi(x[[1]], x[[2]]), gdp_function), x[[3]], x[[4]])[[1]]/
    integrate(multi(multi(x[[1]],x[[2]]),x[[2]]),x[[3]],x[[4]])[[1]]
})






