#integrate(fun,0,2)

multi <- function(f1, f2){
  result <- function(x){
    f1(x) * f2(x)
  }
}

minusck <- function(ck){
  result <- function (x){
    x - ck
  }
}

CreateMinusCks<-function(nodes)
{
  lapply(nodes,FUN = function(x){minusck(x)})
}

minusFuncs<-CreateMinusCks(1:30)
minusFuncs[[5]](4:10)


CalculateBetas<-function(nodes)
{
  fuzzy_sets<-CreateFuzzySets(nodes)
  #integrate(f,left,right)
}

a<-CreateFuzzySets(1:30)
b<-CreateFuzzySets(11:41)
list()

