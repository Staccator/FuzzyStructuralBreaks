#Tworzy pojedynczy zbior rozmyty
#Na podstawie wartosci lewego i prawego wezla

FuzzySet<-function(left,right)
{
  h<-(right-left)/2
  mid=left+h
  a<-1/(mid-left)
  result<-function(x)
  {
    x[x<=left|x>=right]<-0
    x[x>left&x<=mid]<-a*(x[x>left&x<=mid]-left)
    
    x[x>mid&x<right]<- -a*(x[x>mid&x<right]-mid)+1
    return (x)
  }
}

#Tworzy liste zbiorow rozmytych
CreateFuzzySets<-function(nodes)
{
  h<-nodes[2]-nodes[1]
  lapply(nodes,FUN = function(x){FuzzySet(x-h,x+h)})
}

m<-CreateFuzzySets(1:30)
m[[1]](seq(1,20,by=0.2))




