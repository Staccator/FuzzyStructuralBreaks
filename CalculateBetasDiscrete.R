source("ReadData.R")
source("FuzzyPartition.R")
source("FuzzyTransform.R")

h <- domain[2] - domain[1]
nodes <- seq(from=domain[1], length.out = length(domain), by=h)
fuzzy_sets<-CreateFuzzySets(domain)
fuzzy_sets <- data.frame(matrix(unlist(fuzzy_sets), nrow=length(fuzzy_sets), byrow=T), stringsAsFactors=FALSE)
years<-1:length(vector)

tab<-cbind(fuzzy_sets, nodes)

res_discrete<-apply(tab, 1, FUN=function(x)
  {
  sum(x[[1]](years)*vector*(years-x[[2]]))
})
res_discrete




