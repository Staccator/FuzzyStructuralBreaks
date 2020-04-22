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
##Przypadek graniczny - pierwszy Fuzzy Set
FuzzySetLeftBorder<-function(left,right)
{
  h<-(right-left)/2
  mid=left+h
  a<-1/(mid-left)
  result<-function(x)
  {
    x[x<mid|x>=right]<-0
    x[x>mid&x<right]<- -a*(x[x>mid&x<right]-mid)+1
    return (x)
  }
}
##Przypadek graniczny - ostatni Fuzzy Set
FuzzySetRightBorder<-function(left,right)
{
  h<-(right-left)/2
  mid=left+h
  a<-1/(mid-left)
  result<-function(x)
  {
    x[x<=left|x>mid]<-0
    x[x>left&x<=mid]<-a*(x[x>left&x<=mid]-left)
    return (x)
  }
}

#Tworzy liste Fuzzy Sets
CreateFuzzySets<-function(nodes)
{
  h<-nodes[2]-nodes[1]
  n<-length(nodes)
  l1<-list(FuzzySetLeftBorder(nodes[1]-h,nodes[1]+h))
  l2<-list(FuzzySetRightBorder(nodes[n]-h,nodes[n]+h))
  length(nodes)
  nod<-nodes[2:(n-1)]
  l3<-lapply(nod,FUN = function(x)
    {FuzzySet(x-h,x+h)})
  c(l1,l3,l2)
}


m<-CreateFuzzySets(1:30)

#Przykladowe wywolanie
m[[30]](seq(28,33,by=0.2))




