pom<-CreateFuzzySets(1:30)
pom2<-CreateMinusCks(1:30)
c<-simplify2array(pom)
class(c)
c<-unlist(pom)
class(c)
c<-as.vector(unlist(c))
as.data.frame(c)

df <- data.frame(matrix(unlist(pom), nrow=length(pom), 
                        byrow=T),stringsAsFactors=FALSE)
df2<-data.frame(matrix(unlist(pom2), nrow=length(pom2), 
                       byrow=T),stringsAsFactors=FALSE)
d<-cbind(df,df2)
ncol(d)
d[10,2][[1]](-5:10)
s<-apply(d,1,FUN=function(x){multi(x[1],x[2])})
s[1]
length(s)

