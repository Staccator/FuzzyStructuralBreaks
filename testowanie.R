pom<-CreateFuzzySets(1:30)
pom2<-CreateMinusCks(1:30)
c<-simplify2array(pom)
class(c)
c<-unlist(pom)
class(c)
c<-as.vector(unlist(c))
as.data.frame(c)

df <-data.frame(matrix(unlist(pom), nrow=length(pom), 
                        byrow=T),stringsAsFactors=FALSE)
df2<-data.frame(matrix(unlist(pom2), nrow=length(pom2), 
                       byrow=T),stringsAsFactors=FALSE)

pom2<-1:30
d<-cbind(df,df2)
d<-cbind(d,pom2)

s<-apply(d,1,FUN=function(x){multi(x[[1]],x[[2]])(x[[3]])})
s
length(s)



pom