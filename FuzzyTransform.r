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

CalculateBetas<-function(nodes)
{
  fuzzy_sets<-CreateFuzzySets(nodes)
  #integrate(f,left,right)
}
