  library(bnlearn)
  library(graph)
  library(Rgraphviz)
  source("plot-graph-functions.R")
  
  # this is a model with intentinaly added one edge to test the program
  # true_model <- read.net('10_True_20_1a.net')
  # L1 <- 14
  # L2 <- 11
  # true_model <- read.net('10_True_20_1.net')
  true_model <- read.net("BN2O.net")
  L1 <- 5
  L2 <- 3
  
  nod = colnames(amat(true_model))
  nod.sorted <- sort(nod)
  e1 = empty.graph(nod.sorted)
  amat(e1) = amat(true_model)
  
  # files <- list.files(pattern = ".mat")
  files <- c("BN2O_A2_5000_10.mat")
  
  
  for (i in seq_along(files)) {
    bi=(assign(paste(files[i]), read.csv(files[i],header=FALSE, sep=" ")))
    colnames(bi)<- nod
    row.names(bi)<- nod
    bi<-as.matrix(bi)
    #empty graph
    e2 = empty.graph(nod.sorted)
    amat(e2) = bi
    
    # plot the true graph and the graph from files[i]
    name2 <- paste(unlist(strsplit(files[i], split='.', fixed=TRUE))[1],".pdf",sep="")
    name3 <- paste(unlist(strsplit(files[i], split='.', fixed=TRUE))[1],"-compared.pdf",sep="")
    two.graphs.cmp(e1=e1,e2=e2,nr.L1=L1,nr.L2=L2,name2=name2,name3=name3)
    
    # png (file =paste(substr(files[i],1,nchar(files[i])-4), ".png"), width = 550, height = 550)
    # graphviz.compare (cpdag(e1), cpdag(e2))
    # dev.off()
  }
  
  
