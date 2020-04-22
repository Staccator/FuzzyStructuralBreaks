source("ReadData.R")
source("FuzzyPartition.R")
source("FuzzyTransformFunctions.R")

h <- domain[2] - domain[1]
nodes <- seq(from=domain[1], length.out = length(domain), by=h)
fuzzy_sets<-CreateFuzzySets(domain)
fuzzy_sets <- data.frame(matrix(unlist(fuzzy_sets), nrow=length(fuzzy_sets), byrow=T), stringsAsFactors=FALSE)
years<-1:length(vector)

tab<-cbind(fuzzy_sets, nodes)

betasZero_discrete<-apply(tab, 1, FUN=function(x)
  {
  sum(vector*(x[[1]](years)))/sum(x[[1]](years))
})


betasOne_discrete<-apply(tab, 1, FUN=function(x)
  {
  sum(x[[1]](years)*vector*(years-x[[2]]))/sum((years-x[[2]])*(years-x[[2]])*(x[[1]](years)))
})








