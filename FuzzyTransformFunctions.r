#Pomocnicze funkcje do obliczenia Fuzzy Transform

#Zwraca funkcje g=f1*f2
multi <- function(f1, f2){
  result <- function(x){
    f1(x) * f2(x)
  }
}

#Zwraca funkcje g=x-ck
#ck - wezly, bedace srodkami Fuzzy Sets
minusck <- function(ck){
  result <- function (x){
    x - ck
  }
}

#Zwraca liste powyzszych funkcji (minusck)
CreateMinusCks<-function(nodes)
{
  lapply(nodes,FUN = function(x){minusck(x)})
}






