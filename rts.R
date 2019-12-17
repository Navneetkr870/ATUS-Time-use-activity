d0d<-read.csv("dd1.csv") ## already formatted
dd<-dd[,-1]
library(reshape2)
## Code for short format 
dd<-dcast(dd,CASEID+YEAR~ACTIVITY,value.var = "DURATION",sum)
rownames(dd)<-dd$caseid
dd$x<-NULL
dd$year<-NULL

## Hierarchical clustering
dd$YEAR<-NULL
distances<-dist(dd[4:426], method = "euclidean")
cluster_act<-hclust(distances,method ="ward.D")
kk<-eclust(dd,"hclust",k=3,graph = F)
plot(cluster_act)
clustr_group<-cutree(cluster_act,k=3)
rect.hclust(cluster_act,3)
 
fviz_dend(cluster_act, k = 3, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)




## k-means clustering
set.seed(5)
kmc<-kmeans(dat,centers = 7,nstart = 20)
str(kmc)
## mean duration partitioned according to the activity type in each cluster
kmc$centers
library(flexclust)
## Used to predict 
kmcca<-as.kcca(kmc,dd[2:10])

## nstart:-This means that R will try 20 different random starting assignments
## and then select the one with the lowest within cluster variation. 

  
## Clustering packages

install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")

library(factoextra)
library(cluster) 
library(magrittr)
library(NbClust)


## Determining the optimal number of clusters

fviz_nbclust(n2[,2:22], kmeans, method = "gap_stat")
 ## modifying the iter param 
MyKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=30))
## using this in the fviz_nbclust

nb <- NbClust(dd, distance = "euclidean", min.nc = 2,
              max.nc = 5, method = "complete", index ="all")




## Cluster visualization
fviz_cluster(kmc, data =dd ,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),stand = F)


## PAM(Partition around medoids) or K-medoids
library(cluster)
clus<-pam(dd,3)
fviz_cluster(clus,stand = F)
  

## Clustering tendency 
?get_clust_tendency
get_clust_tendency(dd,n=500,seed = 12)

install.packages("NbClust")
library(NbClust)
## Finding the optimal number of clusters
opt<-NbClust(dd[2:426],distance = "euclidean",
        min.nc = 2, max.nc = 10, 
        method = "complete", index ="all") 


##################################################
##################################################

## To find the optimal number of clusters using different methods.
# Elbow method
fviz_nbclust(dat[,2:21], kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(dd[,2:426], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic method
fviz_nbclust(as.matrix(dat[,2:21]), MyKmeansFUN,  method = "gap_stat", nboot = 100,k.max = 10)+
  labs(subtitle = "Gap statistic method")

MyKmeansFUN <- function(x,k) {kmeans(x, k, iter.max=30)}

## Optimal number of clusters=7

##################################################
##################################################

## clustering validation stat
fviz_silhouette(kk)
op<-kk$silinfo$widths$sil_width<0
op1<-kk$silinfo$widths[op,]



## Using clValid package to assess the validity of the cluster.

install.packages("clValid")
library(clValid)

clValid(dd, 2:3, clMethods=c("hierarchical","kmeans","pam"),validation="internal")


## Internal validation relies on information in the data only, that is the characteristics of
## the clusters  which are compatness and separation.

## Connectivity 
## The degree of connectedness of the clusters and it should as small as possible.

## SILHOUETTE WIDTH
## This index defines compactness based on the pairwise distances between all elements in the 
## cluster, and separation based on pairwise distances between all points in the cluster and 
## all points in the closest other cluster

## The values as close to (+) 1 as possible are more desirable.


## Soft clustering methods
## using package cluster
library(cluster)
fuzzy<-fanny(dd,k=4,metric = "euclidean",memb.exp = 1.6)



## Levenshtein Distance
## Cost associated with the transformation of one string to another which usually involves
## insertion, deletion or subsitution. Based on the cost associated with the pair of strings
## we get an idea about the similarity between the two strings.




## Hierarchical sparse clustering
library(sparcl)

HierarchicalSparseCluster(as.matrix(dd[,c(-1,-3)]),method = "complete",wbound = NULL)
## Wbound is an important tuning parameter to determine the weights of the feature.
## Using the in-built permute method to find the optimal value of the tuning parameter.
## It is based on the gap statistic

HierarchicalSparseCluster.permute(x, nperms = 10, wbounds = NULL,
                                  dissimilarity=c("squared.distance",
                                                  "absolute.value"),standardize.arrays=FALSE)



## K-means sparse clustering 
km<-KMeansSparseCluster(as.matrix(dat[,2:21]), K=6, wbounds =1.570276, nstart = 20,silent =
                      FALSE, maxiter=6)
## Finding the weights
km_weights<- km[1][[1]]$ws

## Finding the value of the tuning parameter

KMeansSparseCluster.permute(as.matrix(dat[,2:21]), K=6, nperms = 25, wbounds = NULL,
                            silent = FALSE, nvals = 10, centers=NULL)


################## 
##################
1012013014015016017018019011001
Permutation  1 of  25
1012013014015016017018019011001
Permutation  2 of  25
1012013014015016017018019011001
Permutation  3 of  25
1012013014015016017018019011001
Permutation  4 of  25
1012013014015016017018019011001
Permutation  5 of  25
1012013014015016017018019011001
Permutation  6 of  25
1012013014015016017018019011001
Permutation  7 of  25
1012013014015016017018019011001
Permutation  8 of  25
1012013014015016017018019011001
Permutation  9 of  25
1012013014015016017018019011001
Permutation  10 of  25
1012013014015016017018019011001
Permutation  11 of  25
1012013014015016017018019011001
Permutation  12 of  25
1012013014015016017018019011001
Permutation  13 of  25
1012013014015016017018019011001
Permutation  14 of  25
1012013014015016017018019011001
Permutation  15 of  25
1012013014015016017018019011001
Permutation  16 of  25
1012013014015016017018019011001
Permutation  17 of  25
1012013014015016017018019011001
Permutation  18 of  25
1012013014015016017018019011001
Permutation  19 of  25
1012013014015016017018019011001
Permutation  20 of  25
1012013014015016017018019011001
Permutation  21 of  25
1012013014015016017018019011001
Permutation  22 of  25
1012013014015016017018019011001
Permutation  23 of  25
1012013014015016017018019011001
Permutation  24 of  25
1012013014015016017018019011001
Permutation  25 of  25
1012013014015016017018019011001

Tuning parameter selection results for Sparse K-means Clustering:
  Wbound # Non-Zero W's Gap Statistic Standard Deviation
1   1.2000            274             0                  0
2   1.6267            274             0                  0
3   2.2053            274             0                  0
4   2.9895            274             0                  0
5   4.0527            274             0                  0
6   5.4939            274             0                  0
7   7.4476            274             0                  0
8  10.0962            274             0                  0
9  13.6867            274             0                  0
10 18.5540            274             0                  0
Tuning parameter that leads to largest Gap statistic:  1.2
###########################################
###########################################
dd1<-read.csv("ken.csv")
library(reshape2)
dd<-dcast(dd1,CASEID+YEAR~ACTIVITY,value.var = "DURATION",sum)


## Grouping the variables("IT'S IS AN ART FORM")

g0<-as.data.frame(dd[,3])
names(g0)<-"10101"

## Personal care excluding sleeping 
g1<-dd[,4:12]

c1<-rowSums(g1)

## Activities related to household 
g2<-dd[,13:44]
c2<-rowSums(g2)



## Activities related Caring(Household)
g3<-dd[,45:77]
c3<-rowSums(g3)



## Activities related Caring(NON-Household)

g4<-dd[,78:110]
c4<-rowSums(g4)

## Activities related to working 
## Time spent on activities related to directly or indirectly to income generation

g5<-dd[,c(111:115,118,119,120:129)]

c5<-rowSums(g5)


## Time spent on other avtivities on work(socializing and relaxing in work)
g6<-dd[,c("50201","50202")]
c6<-rowSums(g6)


  


## Education
g8<-dd[,c(130:144)]
c8<-rowSums(g8)

## Consumer puchases
g9<-dd[,c(145:188)]
c9<-rowSums(g9)


## Eating and drinking
g10<-dd[,c(189:190)]
c10<-rowSums(g10)

## Socializing

g11<-dd[,c(191:194)]
c11<-rowSums(g11)


## Tobacco and drug abuse

g12<-as.data.frame(dd[,"120302"])
names(g12)[1]<-"120302"
c12<-g12

## Television and computer 
g13<-dd[,c("120303","120304","120307","120308")]
c13<-rowSums(g13)

## Relaxing and other leisure activities
g14<-dd[,c(195,199:200,c(203:219))]
c14<-rowSums(g14)

## Sports

g15<-dd[,c(220:251)]
c15<-rowSums(g15)

## Watching sports
g16<-dd[,(252:276)]
c16<-rowSums(g16)

## Spiritual
g17<-dd[,c(277:281)]
c17<-rowSums(g17)

## Volunteer
g18<-dd[c(282:306)]
c18<-rowSums(g18)

## Telephone
g19<-dd[,c(307:317)]
c19<-rowSums(g19)

## Travel
g20<-dd[,c(318:386)]
c20<-rowSums(g20)

g21<-dd[,c(387:392)]
c21<-rowSums(g21)

fo<-as.data.frame(cbind(dd$CASEID,g0,c1,c2,c3,c4,c5,c6,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20,c21))

fo<-cbind.data.frame(dd$CASEID,g0,g1,g2,g3,g4,g5,g6,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21)



write.csv(fo, "tt.csv",row.names = F)

dd<-read.csv("fo.csv")

###############################
###############################


dat<-as.matrix.data.frame(cbind(dd$CASEID,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,c20))

## Finding the optimal number of clusters 
# Gap statistic method
opt_clust<-fviz_nbclust(as.matrix(gia[1:1000,]), MyKmeansFUN,  method = "gap_stat", nboot = 100,k.max = 10)+
  labs(subtitle = "Gap statistic method")

MyKmeansFUN <- function(x,k) {kmeans(x, k, iter.max=30)}

## Optimal number of clusters=7 for the collapsed data.




library(sparcl)

## K-means sparse clustering


## Collapsed data 
tun_par<-KMeansSparseCluster.permute(as.matrix(fo[1:2000,c(-1,-23)]), K=2, nperms = 25, wbounds = NULL,
                            silent = FALSE, nvals = 10, centers=NULL)
tun_par

km<-KMeansSparseCluster(as.matrix(fo[1:2000,c(-1,-23)]), K=2, wbounds =1.2, nstart = 20,silent =FALSE, maxiter=6)
 

table(km[1][[1]]$Cs)


## Full data 

opt_clust1<-fviz_nbclust(as.matrix(weekday[,3:23]), MyKmeansFUN,  method = "gap_stat", nboot = 100,k.max = 10)+
  labs(subtitle = "Gap statistic method")
tun_par1<-KMeansSparseCluster.permute(as.matrix(weekday[,3:23]), K=3, nperms = 25, wbounds = NULL, silent = FALSE, nvals = 10, centers=NULL)


km1<-KMeansSparseCluster(as.matrix(weekday[,3:23]), K=3, wbounds =1.376435, nstart = 20,silent =FALSE, maxiter=6)
                          
table(km1[1][[1]]$Cs)


########################

## Hierarchical clustering
## Collapsed data


# Gap statistic method
hierar_clust<-fviz_nbclust(as.matrix(dat[1:1000,6:22]),hcut,  method = "gap_stat", nboot = 100,k.max = 10)+
  labs(subtitle = "Gap statistic method")



h1<-HierarchicalSparseCluster.permute(as.matrix(weekday[,3:23]), nperms = 10, wbounds = NULL,
                                  dissimilarity=c("squared.distance"
                                                  ),standardize.arrays=FALSE)
h1

h2<-HierarchicalSparseCluster(as.matrix(dat[,2:21]),method = "complete",wbound = 1.325611)


ColorDendrogram(h2$hc,cutree(h2$hc,5))


## Full data

H1<-HierarchicalSparseCluster.permute(as.matrix(dd[,3:425]), nperms = 10, wbounds = NULL,
                                      dissimilarity=c("squared.distance"
                                      ),standardize.arrays=FALSE)

H2<-HierarchicalSparseCluster(as.matrix(dd[,3:425]),method = "complete",wbound = 1.325611)



## Vanilla k-means

kmc<-kmeans(dat[,2:21],centers = 7,nstart = 20)
kmc$cluster
table(kmc$cluster)


## Vanilla hierarchichal 
distances<-dist(dat[,2:21], method = "euclidean")
cluster_act<-hclust(distances,method ="ward.D")
plot cluster_act


## Time series

################################################################
################################################################


## Using the chron package to generate the time series
library(chron)


nih<-seq(times("4:00:00"), times("6:00:00"), times("00:05:00"))

op1<-read.csv("er.csv")


opo<-read.csv("er.csv",nrows = 12)



#sample data
tseq<-seq(as.POSIXct("2016-05-15 00:20:00"), by="1 hour", length.out=50)
library(chron)
t<-times(format(tseq, "%T"))  #%T=%H:%M:%S


#group by 1 hour bins
#create 24 bins and cut
mybreaks<-seq(0, 1, 1/24)
out<-tapply(t, cut(t, breaks=mybreaks), length)
#create pretty names
names(out)<-format(as.chron(mybreaks[1:24]), "%T")

######################################################
## Creating the loop 

okk<-setNames(data.frame(matrix(ncol = 336, nrow = 0)), diss)


day_1<-times("23:00:00")+times("01:00:00")
p<-0
n1<-data.frame(matrix(ncol = 312, nrow = 0))
op1<-read.csv("er.csv")
op12<-op1[c(1:39),]


#######################################################################################

for (i in unique(dod$CASEID)){

dd2<-dod[dod$CASEID==i,]  
p<-p+1
  
for (j in nrow(dd2)) {
t1<-times("00:00:00")
for (k in 1:312) {

   
if (((times("4:00:00")+t1)>=dd2$START[j]) & ((times("4:05:00")+t1)<=dd2$STOP[j])) { 
  
n1[p,k]<-dd2$ACTIVITY[j]
  
   
  
}
t1<-t1+times("00:05:00")
  
  }    
 }
}

#######################################################################################



nih<-seq(times("4:00:00"), times("6:00:00"), times("00:05:00"))


format(as.chron(seq(0,1,(1/(24*60))*5)),"%T")

t1<-0
g<-vector()
t1<-times("00:00:00")
g<-times()
for (k in 1:313) {
  
  g[k]<-times("04:00:00")+t1 
  
     t1<-times("00:05:00")+t1
    
    
  }
  

##Skipping the iteration
for(n in 1:5) {
  if(n==3) {next} # skip 3rd iteration and go to next iteration

}



for (i in unique(op1$CASEID)){
  
dd<-op1[op1$CASEID==i,]  



#######################################################################

library(chron)
iop<-read.csv("er.csv")
op12<-read.csv("er.csv")
op12<-op12[c(1:39,236638:236657),]
day_1<-times("23:00:00")+times("01:00:00")

op12$START<-times(op12$START)
op12$STOP<-times(op12$STOP)

dod<-data.frame()

#########################################################################

for (i in unique(op12$CASEID)){
op<-op12[op12$CASEID==i,]
   
for (j in 1:nrow(op)) {

if (op[j,9]>op[j,10]){

  op[j,10]<-day_1+op[j,10]

for(k in j:(nrow(op)-1)){
  if (j>(nrow(op)-1)){break}
  op[k+1,10]<-day_1+op[k+1,10]
  op[k+1,9]<-day_1+op[k+1,9]
    }
  dod<-rbind(dod,op)
  break
}
  else {
    if (j==nrow(op)){dod<-rbind(dod,op)}
  }
 } 
}

#####################################################

 
n1<-data.frame(matrix(ncol = 312, nrow = 0))
op1<-read.csv("er.csv")
op12<-op1[c(1:12),]
dd2<-dod
 
n1<-setNames(data.frame(matrix(ncol = 336, nrow = 0)), diss)


for (j in 1:nrow(dd2)) {
  t1<-times("00:00:00")
for (k in 1:336) {
  
  
  if (round(as.numeric(times("4:00:00")+t1),3)>=round(as.numeric(dd2$START[j]),3) &round(as.numeric(times("4:05:00")+t1),3)<=round(as.numeric(dd2$STOP[j]),3)) {                  
    
    n1[1,k]<-dd2$ACTIVITY[j]
    
}
  t1<-t1+times("00:05:00")
}
}

format(as.chron(seq(0,1,(1/(24*60))*5)),"%T")


mybreaks<-seq(0, 1, 1/24)
out<-tapply(t, cut(t, breaks=mybreaks), length)

kok<-seq(1/6,4/3,(1/(24*60))*5)
kok<-times(kok)
diss<-cut(kok,breaks=kok)[-1]

 
ot<-times(c("4:00:00","4:05:00","4:10:00","4:15:00"))
ot
cut(t,breaks = times(c("04:05:00","04:10:00")))


t1<-times("00:00:00")

a1<-numeric()
a2<-numeric()

for (k in 1:336) {
 a1[k]<-times("4:00:00")+t1
 a2[k]<-times("4:05:00")+t1   
   t1<-t1+times("00:05:00")
}
a1<-round(a1,5)
a2<-round(a2,5)

library(lubridate)

minute(chron(times("4:32:00")))
