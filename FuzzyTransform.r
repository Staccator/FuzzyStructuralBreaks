fun<-function(x){x+5}
fun2<-function(x){2*x}
fun(2)*fun2(2)

##calka
integrate(fun,0,2)
?integrate

n<-10
left=0
right=100

CalculateBetaZero<-funtion(f,left,right,n)
{
  res<-numeric(n)
  nodes<-seq(left,right,length=n)
  integrate(f,left,right)
}