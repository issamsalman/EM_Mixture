library(bnlearn)
library(dplyr)
library(missMethods)

DAG.random <- function(v, n, first, second) {

  #edges.max <- first * second 
  #edges.max <- v*(v-1)/2
  # Assert length(v)==1 && 1 <= v
  # Assert 0 <= n <= edges.max
  #index.edges <- lapply(list(1:(v)), function(k) rep(k*(k+1)/2, v-k)) 
  #index.edges <- 1:edges.max
  #index.edges <- index.edges[[1]] + 1:edges.max
  graph.adjacency <- matrix(0, ncol=v, nrow=v)
  #graph.adjacency[sample(index.edges, n)] <- 1
  for (i in (first+1):(v)){
    for(j in (sample(1:first, sample(1:n, 1), replace=FALSE))){
      graph.adjacency[j,i]=1
    }}
  return(graph.adjacency)
}

GetNumberSuiteAnyBase <- function(lengthSuite,base){
  nB <- length(base) # radix of your base
  nDigits <- floor(log(lengthSuite-1)/log(nB))+1 # the number of digits you'll need
  numberSuite <- ""
  for(iDigit in 1:nDigits){
    newDigit <- rep(base,each=nB^(iDigit-1),length.out=lengthSuite)
    numberSuite <- paste0(newDigit,numberSuite)
    numberSuite <- as.vector(numberSuite)
  }
  return(numberSuite)
}

bnn <- function(v, nedges, first, second) {
  #bi<-as.matrix(DAG.random(v=10,nedges=5, first=4, second=6))
  bi<-as.matrix(DAG.random(v,nedges, first, second))
  node <- GetNumberSuiteAnyBase(ncol(bi),LETTERS)
  colnames(bi) <- node
  row.names(bi) <- node
  bi <- as.matrix(bi)
   return((bi))
}

BN2O<-function(node_num, max_parents, first, second,data_size){

bi =bnn(node_num, max_parents, first, second)
diag(bi) <- 0
e = empty.graph(colnames(bi))
amat(e) = bi
graphviz.plot (e)

#on

cpt_1 = c(1, 0, 0.2, 0.8)
dim(cpt_1) = c(2, 2)
cpt_2 =c(1, 0, 0.2, 0.8, 0.2, 0.8, 0.04, 0.96)
dim(cpt_2) = c(2, 2,2)
cpt_3 =  c(1, 0, 0.2, 0.8, 0.2, 0.8, 0.04, 0.96, 0.2, 0.8, 0.04,0.96,0.04, 
           0.96, 0.008,0.992)
dim(cpt_3) = c(2,2, 2,2)
cpt_4 = c(1, 0, 0.2, 0.8, 0.2, 0.8, 0.04, 0.96, 0.2, 0.8, 0.04,0.96,0.04, 
          0.96, 0.008,0.992, 0.2, 0.8, 0.04, 0.96, 0.04, 0.96, 0.008,0.992,
          0.04, 0.96, 0.008,0.992, 0.008,0.992, 0.0016, 0.9984)
dim(cpt_4) = c(2,2,2,2,2)
lis = list()
for (i in colnames(bi)){
  print(i)
  if (sum(bi[,i])==0){
    a = matrix(c(0.5, 0.5), ncol = 2, dimnames = list(NULL, c("FALSE", "TRUE")))
    a = list(a)
    names(a) = i
    lis = append(lis, a)
    rm(a)
      }
  if (sum(bi[,i])==1){
    print(i)
    x=list(c("FALSE", "TRUE"))
    names(x)=i
    index <- which(bi[,i]==1, arr.ind=FALSE)
    y1 = list(c("FALSE", "TRUE"))
    names(y1)=row.names(bi)[index]
    y1 = append(x,y1)
    a1=cpt_1
    dimnames(a1) = y1
    a1=list(a1)
    names(a1)=i
    lis = append(lis, a1)
    rm(a1)
  }
  
  if (sum(bi[,i])==2){
    print(i)
    x=list(c("FALSE", "TRUE"))
    names(x)=i
    index <- which(bi[,i]==1, arr.ind=FALSE)
    y2 = (c("FALSE", "TRUE"))
    y2=replicate(length(index),y2, FALSE)
    names(y2)=row.names(bi)[index]
    y2 = append(x,y2)
    a2=cpt_2
    dimnames(a2) = y2
    a2=list(a2)
    names(a2)=i
    lis = append(lis, a2)
    rm(a2)
  }
  
  if (sum(bi[,i])==3){
    print(i)
    x=list(c("FALSE", "TRUE"))
    names(x)=i
    index <- which(bi[,i]==1, arr.ind=FALSE)
    y3 = (c("FALSE", "TRUE"))
    y3=replicate(length(index),y3, FALSE)
    names(y3)=row.names(bi)[index]
    y3 = append(x,y3)
    a3=cpt_3
    dimnames(a3) = y3
    a3=list(a3)
    names(a3)=i
    lis = append(lis, a3)
    rm(a3)
  }
  if (sum(bi[,i])==4){
    print(i)
    x=list(c("FALSE", "TRUE"))
    names(x)=i
    index <- which(bi[,i]==1, arr.ind=FALSE)
    y4 = (c("FALSE", "TRUE"))
    y4=replicate(length(index),y4, FALSE)
    names(y4)=row.names(bi)[index]
    y4 = append(x,y4)
    a4=cpt_4
    dimnames(a4) = y4
    a4=list(a4)
    names(a4)=i
    lis = append(lis, a4)
    rm(a4)
  }
}
dfit = custom.fit(e, dist = lis)
#write.net('D:/TARADATA_ISSAM/true/True_BN2O.net',dfit)

#sim = rbn(dfit, data_size, learning.test)

return(dfit)
}

miss_dat <- function(mis = 0.1, node_num = 25, 
                      file_name, max_parents = 3, first=10, second=15,data_size=15){
  dfit = BN2O(node_num, max_parents, first, second,data_size)
  write.net(paste('D:/TARADATA_ISSAM/true/',file_name, sep=''),dfit)
  data11 = rbn(dfit, data_size, learning.test)
  
  for (i in colnames(data11)){
    data11[,i] = as.integer(data11[,i]) -1
  }
  
  data11=delete_MCAR(data11, mis)
  write.table(data11,paste('D:/TARADATA_ISSAM/true/',file_name, '_', mis,
                           '.dat', sep=''), row.names = FALSE, sep=' ')
  return(data11)
}

#write.table(data11,
     #       "D:/TARADATA_ISSAM/BN2O_RandomMatrix/final/12-8_1/1_12-8_10.dat", 
    #        row.names = FALSE, sep=' ')
#write.table(data2,
 #           "D:/TARADATA_ISSAM/BN2O_RandomMatrix/final/12-8_1/1_12-8_5.dat", 
  #          row.names = FALSE, sep=' ')


