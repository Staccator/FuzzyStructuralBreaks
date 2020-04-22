source("CalculateBetasDiscrete.R")
source("CalculateBetasContinuous.R")
source("StructuralBreaks.R")

#Zwraca funkcje postaci a+b*f1
createFunction<-function(f1,a,b) 
{
  res<-function(x)
  {
    a+b*f1(x)
  }
}


#l - lista funkcji (f1, ... , fn)
#Zwraca funkcje g=f1+f2+...+fn
SumOfFunctions<-function(l) 
{
  res<-function(y)
  {
    l<-lapply(l,FUN=function(x){x(y)})
    l<-data.frame(matrix(unlist(l), nrow=length(l), byrow=T), stringsAsFactors=FALSE)
    colSums(l)
  }
}


##Funkcja wyznaczajaca odwrotna F-Transformate
##Jako argument przyjmuje wspolczynniki Beta0 i Beta1
##Mozna podac zarowno bety z metody dyskretnej jak i ciaglej

FindInverseFTransform<-function(betasZero,betasOne)
{
  ##Wyliczenie wektora transformaty (funkcji Fk)
  tab<-cbind(minus_cks,betasZero,betasOne)
  fuzzyTransform_cont<-apply(tab,1,FUN=function(x){
    createFunction(x[[1]],x[[2]],x[[3]])
  })
  fuzzyTransform_cont<-data.frame(matrix(unlist(fuzzyTransform_cont), 
                                         nrow=length(fuzzyTransform_cont), byrow=T), stringsAsFactors=FALSE)
  ##Obliczenie transformaty odwrotnej
  tab<-cbind(fuzzyTransform_cont,fuzzy_sets)
  l<-apply(tab,1,FUN=function(x){multi(x[[1]],x[[2]])})
  inverseTransform<-SumOfFunctions(l)
}


