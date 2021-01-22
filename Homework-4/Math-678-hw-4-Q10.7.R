library(knitr)
df=data.frame(c(0,0.3,0.4,0.7),c(0.3,0.0,0.5,0.8),c(0.4,0.5,0.0,0.45),c(0.7,0.8,0.45,0.0))
#print(d)
colnames(df)=c(paste('Col',1:4))
kable(df)
hcl<- hclust(dist(df))
plot(hcl)

plot(hclust(dist(df),method='single'),xlab='')
row.names(df)=c(2,1,4,3)
plot(hclust(dist(df)))
#Q3
#a
tb=data.frame(c(1,1,0,5,6,4),c(4,3,4,1,2,0))
colnames(tb)=c('X1','X2')
rownames(tb)=1:6
plot(tb,col=rainbow(6))
#b
set.seed(42)
labels=sample(1:6,6)
tb=cbind(tb,labels)
kable(tb)
#c
centroids=aggregate(.~labels,tb,mean)
colnames(centroids)=c('labels',paste('C.X',1:2))

kable(data.frame(tb[,-3],centroids[tb$labels,-1],labels=tb$labels))
#d
EuclideanDistance=function(v,z){
  sqrt(sum( (v-z)^2 ))
}

labels=apply(tb[,c(1,2)],1,
             function(x){
               dist=apply(centroids[tb$labels,c(2,3)],1,function(y){ EuclideanDistance(x,y) })
               which.min(dist[dist>0])
             }
)

kable(data.frame(tb[,-3],centroids[tb$labels,-1],old.labels=tb$labels,new.labels=labels))
plot(tb[,c(1,2)],col=rainbow(6)[labels])
#e
while( any(labels != tb$labels)){
  tb$labels=labels
  centroids=aggregate(.~labels,tb,mean)
  
  labels=apply(tb[,c(1,2)],1,
               function(x){
                 dist=apply(centroids[tb$labels,c(2,3)],1,function(y){ EuclideanDistance(x,y) })
                 which.min(dist[dist>0])
               }
  )
}
#f
plot(tb[,c(1,2)],col=rainbow(6)[tb$labels])
#Q9
#A
library(ISLR)
set.seed(2)
hc.complete = hclust(dist(USArrests), method="complete")
plot(hc.complete)
#b
cutree(hc.complete, 3)
table(cutree(hc.complete, 3))
#c
dsc = scale(USArrests)
hc.s.complete = hclust(dist(dsc), method="complete")
plot(hc.s.complete)
#d
cutree(hc.s.complete, 3)
table(cutree(hc.s.complete, 3))
table(cutree(hc.s.complete, 3), cutree(hc.complete, 3))
#Q10
#A
set.seed(42)
data= matrix(sapply(1:3,function(x){ rnorm(20*50,mean = 10*sqrt(x))  }),ncol=50)    # 20 obs. in each class with 50 features.
class=unlist(lapply(1:3,function(x){rep(x,20)}))
#B
pca.out = prcomp(x)
plot(pca.out$x[,1:2], col=2:4, xlab="Z1", ylab="Z2", pch=19) 
#C
set.seed(1)
kmeans.out=kmeans(data,3)

table(kmeans.out$cluster)

table(class)

plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)
#d
set.seed(1)
kmeans.out=kmeans(data,2)

table(kmeans.out$cluster)

table(class)

plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)
#E
set.seed(1)
kmeans.out=kmeans(data,4)

table(kmeans.out$cluster)

table(class)

plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)
#F
set.seed(1)
kmeans.out=kmeans(pr.out$x[,c(1,2)],3)

table(kmeans.out$cluster)

table(class)

plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)
#g
set.seed(1)
kmeans.out=kmeans(scale(data,center = T,scale = T),3)

table(kmeans.out$cluster)
 
table(class)

plot(pr.out$x[,c(1,2)],col=kmeans.out$cluster)

