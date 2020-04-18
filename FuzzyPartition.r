##a,b - min i max przedzialu, k-ile zbiorow rozmytych chcemy (im wiecej tym dokladniejsze wyniki)

f<-c(mean,sum)
f<-function(x){x}
f(5)
f(1:5)
FuzzySet<-function(x,left,right)
{
  mid=(right-left)/2+left
  mid
  x
  x[x<=left|x>=right]<-0
  
  
  a<-(1/(mid-left))
  a
  x[x>left&x<=mid]<-a*(x[x>left&x<=mid]-left)
  
  x[x>mid&x<right]<- -a*(x[x>mid&x<right]-mid)+1
  x
}
x<-seq(-3,15,by=0.1)
plot(x)

plot(FuzzySet(x,1,3))



