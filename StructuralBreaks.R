source("ReadData.R")
#Ustawia kontekst danych, aby wiedziec jakie wartosci
#wspolczynnikow beta1 opisuja duze zmiany
SetContext<-function(data)
{
  n<-length(data)
  vR<-(max(data)-min(data))/(2*h) #h-odleglosc miedzy wezlami
  c(0,0.4*vR,vR)
}

#Funkcja zwraca indeksy fuzzy sets
#odpowiadajace miejscom na wykresie gdzie nastapil "structural break"
#sensitivity - z jaka czuloscia chcemy wykrywac breaki (sensitivity - 1 domyslnie)
#im wieksze sensitivity tym wiecej breaks powinnismy znalezc
FindStructuralBreaks<-function(betas, context, sensitivity=1)
{
  stopifnot(sensitivity>=0.1,sensitivity<=10)
  invSens<-1/sensitivity
  n<-length(betas)
  betas[1]<-betas[2]
  betas[n]<-betas[n-1]
  changeToPos<-which(diff(sign(betas))>0)
  changes<-betas[changeToPos+1]-betas[changeToPos]
  ind<-which(changes>0.5*context[2]*invSens)
  res1<-changeToPos[ind]
  
  changeToNeg<-which(diff(sign(betas))<0)
  changes2<-betas[changeToNeg]-betas[changeToNeg+1]
  ind2<-which(changes2>0.5*context[2]*invSens)
  res2<-changeToNeg[ind2]
  
  ver<-which(abs(betas)>1*context[2]*invSens)
  hor<-which(abs(betas)<0.2*context[2]*sensitivity)
  res3<-unique(intersect(ver,hor+1))
  
  res<-unique(c(res1,res2,res3))
  res<-res[order(res)]
}

