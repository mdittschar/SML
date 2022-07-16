library(flexclust)
library(Rtsne)

#------------------
# TASK 1
# b)
#------------------
#RandIndex function 
  RandIndex=function(cluster_1, cluster_2){
      a = abs(sapply(cluster_1, function(i) i - cluster_1))
      a[a > 1] = 1
      b = abs(sapply(cluster_2, function(i) i - cluster_2))
      b[b > 1] = 1
      falses = sum(abs(a - b))/2
      no.pairs = choose(dim(a)[1], 2)
      ri = 1 - falses/ no.pairs
      return(ri)
}

c1 = sample(1:3, size=6, replace=TRUE)
c2 = sample(1:3, size=6, replace=TRUE)

c1.1 = sample(1:5, size=10000, replace=TRUE)
c2.1 = sample(1:5, size=10000, replace=TRUE)


tab = table(c1, c2)#
tab.2 = table(c1.1,c2.1)

#own function
RandIndex(c1,c2)
RandIndex(c1.1,c2.1)

#libary function 
randIndex(tab, correct=TRUE, original=TRUE)
randIndex(tab.2, correct=TRUE, original=TRUE)


#------------------
# TASK 2
#
#
#------------------
#load data sets
data = read.csv("../wdbc.data",sep = ",")

#------------------
# a) hclust
#------------------
dd = dist((data[,3:ncol(data)]), method = "euclidean")
hc.average = hclust ( dd, method = "average")
plot (hc.average , main = "Average Linkage ",xlab = "", sub = "", cex = .9)

#------------------
# b) cutree, table 
#------------------

multicutree=function(x,k, d){
  cutree.out= cutree (x, k)
  tabs= table(cutree.out,factor(d$M))
  ri= randIndex(tabs, correct=TRUE, original=TRUE)
  print(paste("Number of clusters:", k))
  print(tabs)
  print(ri)
  return(cutree.out)
}
# loop to get matrixes for diffrent number of clusters

for (k in 2:6){
  set.seed(1)
  cu.tree= multicutree(hc.average,k,data)
  #print(cu.tree)
  
  if(k==3){
    cu.tree[cu.tree==1]= 'B'
    cu.tree[cu.tree==2|cu.tree==3]= 'M'
    #print(cu.tree)
    tab.c= table(cu.tree,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  
  else if(k==4){
    cu.tree[cu.tree==1]= 'B'
    cu.tree[cu.tree==2|cu.tree==3|cu.tree==4]= 'M'
    #print(cu.tree)
    tab.c= table(cu.tree,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else if(k==5){
    cu.tree[cu.tree==1|cu.tree==3|cu.tree==4|cu.tree==5]= 'M'
    cu.tree[cu.tree==2]= 'B'
    #print(cu.tree)
    tab.c= table(cu.tree,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else if(k==6){
    cu.tree[cu.tree==1|cu.tree==3|cu.tree==4|cu.tree==5|cu.tree==6]= 'M'
    cu.tree[cu.tree==2]= 'B'
   # print(cu.tree)
    tab.c= table(cu.tree,factor(data$M))
    #print(factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else{
     
  }
}

#------------------
# c) randIndex from the flexclust-package (
#------------------
# see above function multcutree!!


#------------------
# d) kmeans
#------------------

ks= c()
w_k= c()
for (k in 2:20){
  set.seed (1)
  km.out = kmeans (data[,3:ncol(data)], k, nstart = 25)
  ks= append(ks,k)
  w_k= append(w_k,km.out$tot.withinss)
}

plot(ks,(log(w_k)),ylab="Total Within Sum of Squares", xlab="number of clusters K")
abline(v=11, col="red", lwd=1, lty=2)
title("kmeans")
axis(1, at=seq(1,20, by=1))#

#best k = 11
set.seed(1)
kmeans.out= kmeans (data[,3:ncol(data)], 11, nstart = 25)
tab.kmeans = table(kmeans.out$cluster, factor(data$M))
tab.kmeans
randIndex(tab.kmeans)

#if we do adress the two clusters based on the confusionmatrix from above 
kmeans.out$cluster[kmeans.out$cluster==1|kmeans.out$cluster==3|kmeans.out$cluster==8|kmeans.out$cluster==11]= 'B'
kmeans.out$cluster[kmeans.out$cluster==2|kmeans.out$cluster==4|kmeans.out$cluster==5|kmeans.out$cluster==6|kmeans.out$cluster==7|kmeans.out$cluster==9|kmeans.out$cluster==10]= 'M'
tab.c= table(kmeans.out$cluster,factor(data$M))
print(tab.c)
ri= randIndex(tab.c, correct=TRUE, original=TRUE)
print(ri)
#------------------
# e) scale 
#------------------
dds = dist(scale(data[,3:ncol(data)]), method = "euclidean")
hc.average.s = hclust ( dds, method = "average")
plot (hc.average.s , main = " Average Linkage ",xlab = "", sub = "", cex = .9)

# loop to get matrixes for diffrent number of clusters
for (k in 2:6){
  set.seed(1)
  cu.tree.s=multicutree(hc.average.s,k,data)
  
  if(k==3){
    cu.tree.s[cu.tree.s==1|cu.tree.s==2]= 'B'
    cu.tree.s[cu.tree.s==3]= 'M'
    tab.c= table(cu.tree.s,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  
  else if(k==4){
    cu.tree.s[cu.tree.s==1|cu.tree.s==2]= 'B'
    cu.tree.s[cu.tree.s==3|cu.tree.s==4]= 'M'
    tab.c= table(cu.tree.s,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else if(k==5){
    cu.tree.s[cu.tree.s==1|cu.tree.s==2]= 'B'
    cu.tree.s[cu.tree.s==3|cu.tree.s==4|cu.tree.s==5]= 'M'
    tab.c= table(cu.tree.s,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else if(k==6){
    cu.tree.s[cu.tree.s==1|cu.tree.s==3]= 'B'
    cu.tree.s[cu.tree.s==2|cu.tree.s==4|cu.tree.s==5|cu.tree.s==6]= 'M'
    tab.c= table(cu.tree.s,factor(data$M))
    print(tab.c)
    ri= randIndex(tab.c, correct=TRUE, original=TRUE)
    print(ri)
  }
  else{
    
  }
}

ks.s= c()
w_k.s= c()
for (k in 2:20){
  set.seed (1)
  km.out.s <- kmeans (scale(data[,3:ncol(data)]), k, nstart = 25)
  ks.s= append(ks.s,k)
  w_k.s= append(w_k.s,km.out.s$tot.withinss)
}


plot(ks.s,(log(w_k.s)),ylab="Total Within Sum of Squares", xlab="number of clusters K")
abline(v=11, col="red", lwd=1, lty=2)
title("kmeans on normalized (scaled) data")
axis(1, at=seq(1,20, by=1))#


#set.seed(1)
#kmeans.out.7= kmeans (data[,3:ncol(data)], 7, nstart = 25)
#table(kmeans.out.7$cluster, factor(data$M))

#best k = 11
set.seed(1)
kmeans.out.s= kmeans (scale(data[,3:ncol(data)]), 11, nstart = 25)
tab.kmeans.s = table(kmeans.out.s$cluster, factor(data$M))

tab.kmeans.s
randIndex(tab.kmeans.s)

#if we do adress the two clusters based on the confusion matrix from above 
kmeans.out.s$cluster[kmeans.out.s$cluster==3|kmeans.out.s$cluster==4|kmeans.out.s$cluster==5|kmeans.out.s$cluster==7|kmeans.out.s$cluster==9|kmeans.out.s$cluster==10]= 'B'
kmeans.out.s$cluster[kmeans.out.s$cluster==1|kmeans.out.s$cluster==2|kmeans.out.s$cluster==6|kmeans.out.s$cluster==8|kmeans.out.s$cluster==11]= 'M'
tab.c= table(kmeans.out.s$cluster,factor(data$M))
print(tab.c)
ri= randIndex(tab.c)
print(ri)

#------------------
# f) 
#------------------

#------------------
# g) tsne
#------------------
for (p in c(5,10,20,50)){
  set.seed(1)
  tsne_out <- Rtsne(dd, perplexity=p,theta=0.0)
  
  plot(tsne_out$Y,col='black', asp=1)
  title(paste("t-sne (not normalized data), perplexity =",p ))
  
  plot(tsne_out$Y,col=factor(data$M), asp=1)
  title(paste("t-sne (not normalized data), perplexity =",p ))
  #print(paste("Perplexity: ", p))
  
}

# for scaled datasets
for (p in c(5,10,20,50)){
  set.seed(1)
  tsne_out.s <- Rtsne(dds, perplexity=p,theta=0.0)
  
  plot(tsne_out.s$Y,col='black', asp=1)
  title(paste("t-sne (normalized data), perplexity =",p ))
  
  plot(tsne_out.s$Y,col=factor(data$M), asp=1)
  title(paste("t-sne (normalized data), perplexity =",p ))
}

#------------------
# h) PCA
#------------------
set.seed(1)
pca.out = prcomp(data[,3:ncol(data)], center = TRUE,scale. = FALSE)
str(pca.out)
plot(x=pca.out$x[,"PC1"], y=pca.out$x[,"PC2"], xlab="PC1", ylab="PC2", col='black')
#cluster colored
plot(x=pca.out$x[,"PC1"], y=pca.out$x[,"PC2"], xlab="PC1", ylab="PC2", col=factor(data$M))
title("PCA on dataset (not scaled)")
#arrows(x0=rep(0,12), y0=rep(0,12), x1=pca$rotation[,"PC1"]*10, y1=white.pca$rotation[,"PC2"]*10, xlab="PC1", ylab="PC2")
#text(x=white.pca$rotation[,"PC1"]*10-.45, y=white.pca$rotation[,"PC2"]*10+.45, labels=colnames(wine.white)[1:11])
title("PCA on dataset (not scaled)")

#scaled data
pca.out.s = prcomp(data[,3:ncol(data)], center = TRUE,scale. = TRUE)
str(pca.out.s)
plot(x=pca.out.s$x[,"PC1"], y=pca.out.s$x[,"PC2"], xlab="PC1", ylab="PC2", col='black')
title("PCA on scaled/ normalized data")
#cluster colored plot
plot(x=pca.out.s$x[,"PC1"], y=pca.out.s$x[,"PC2"], xlab="PC1", ylab="PC2", col=factor(data$M))
title("PCA on scaled/ normalized data")

