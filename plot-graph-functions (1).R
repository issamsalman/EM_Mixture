library(igraph)
library(fBasics)

# given an edge e in g1, return the corresponding edge in g2
matching_edge <- function(g1,e,g2) {
  name1 <- V(g1)[get.edges(g1,e)[1,1]]$name
  name2 <- V(g1)[get.edges(g1,e)[1,2]]$name
  E(g2)[get.edge.ids(g2,c(name1,name2))]
}
matching_edge_reversed <- function(g1,e,g2) {
  name1 <- V(g1)[get.edges(g1,e)[1,1]]$name
  name2 <- V(g1)[get.edges(g1,e)[1,2]]$name
  E(g2)[get.edge.ids(g2,c(name2,name1))]
}

# plot two graphs with the same layout of nodes
# the first graph is assumed to be bipartite with nodes 1:nr.L1 in the first layer
# and nodes (nr.L1+1):(nr.L1+nr.L2) in the second layer
two.graphs.cmp <- function(e1,e2,nr.L1=14,nr.L2=11,
                           name1="true-model.pdf",name2="learned-model.pdf",
                           name3="learned-model-compared.pdf"){
  f1 <- cpdag(e1)
  f2 <- cpdag(e2)
  # graphviz.plot(f1)
  # graphviz.plot(f2)
  a1 <- amat(f1)
  a2 <- amat(f2)
  # undirected edges
  g1 <- graph_from_adjacency_matrix(a1, mode = "directed")
  g2 <- graph_from_adjacency_matrix(a2, mode = "directed")
  E(g1)$color <- "black"
  E(g2)$color <- "black"
  
  g12d <- difference(g1,g2)
  a12 <- as_adjacency_matrix(g12d,sparse=FALSE)
  # remove from g12d edges that are in g2 present reversed or directed instead of undirected
  a12 <- Ramp(a12 - t(a2))
  g12 <- graph_from_adjacency_matrix(a12, mode = "directed")
  
  g21d <- difference(g2,g1)
  a21 <- as_adjacency_matrix(g21d,sparse=FALSE)
  # remove from g21d edges that are in g1 present reversed or directed instead of undirected
  a21 <- Ramp(a21 - t(a1))
  g21 <- graph_from_adjacency_matrix(a21, mode = "directed")
  
  # add edges from g12 to g3
  a3 <- a2+a12
  g3 <- graph_from_adjacency_matrix(a3, mode = "directed")
  E(g3)$color <- "black"
  E(g3)$lty = 1
  
  for (e in E(g21)) {
    eg <- matching_edge(g21,e,g3)
    E(g3)[eg]$color <- "blue"
    E(g3)[eg]$lty <- 3 
  }
  for (e in E(g12)) {
    eg <- matching_edge(g12,e,g3)
    E(g3)[eg]$color <- "red"
  }
  g21e <- difference(g21d,g21)
  for (e in E(g21e)){
    eg <- matching_edge_reversed(g21e,e,g3)
    E(g3)[eg]$color <- "orange"
    eg <- matching_edge(g21e,e,g3)
    E(g3)[eg]$color <- "orange"
  }
  g12e <- difference(g12d,g12)
  for (e in E(g12e)){
    eg <- matching_edge_reversed(g12e,e,g3)
    E(g3)[eg]$color <- "orange"
    eg <- matching_edge(g12e,e,g3)
    E(g3)[eg]$color <- "orange"
  }

  # layout the nodes 
  # LO = layout_with_drl(g)
  # LO = layout_with_dh(g)
  # LO <- layout_nicely(g)
  # LO <- layout_in_circle(g)
  # LO <- layout_with_gem(g)
  vertex_attr(g1, "type") <- c(rep(FALSE,nr.L1),rep(TRUE,nr.L2))
  LObp <- layout_as_bipartite(g1)
  v1 <- LObp[1:nr.L1,1]
  v2 <- LObp[(nr.L1+1):(nr.L1+nr.L2),1]
  o1 <- order(v1)
  oo1 <- order(o1)
  o2 <- order(v2)
  oo2 <- order(o2)
  if (length(V(g1)) != length(V(g2))) stop("The graphs have different number of nodes!")
  nr.V <- length(V(g1))
  if (nr.L1+nr.L2 != nr.V) stop(
    "The sum of the number of nodes in layer one and two differs from the total number of graph nodes!")
  LO <- matrix(0, nrow=nr.V,ncol=2)
  betas <- rev(pi/6+4*pi*((1:nr.L1)-1)/(6*(nr.L1-1)))
  for (i in 1:nr.L1){
    beta <- betas[oo1[i]]
    LO[i,] <- c(cos(beta),sin(beta))
  }
  betas <- 7*pi/6+4*pi*((1:nr.L2)-1)/(6*(nr.L2-1))
  for (i in 1:nr.L2){
    beta <- betas[oo2[i]]
    LO[i+nr.L1,] <- c(cos(beta),sin(beta))
  }
  
  # which edges should have arrows:
  #  0 ... no arrow
  #  1 ... backward arrow
  #  2 ... forward arrow
  arr1 <- a1
  arr1[a1*t(a1)==1] <- 0
  arr1 <- arr1 + arr1*upper.tri(arr1,diag = FALSE)
  arrows1 <- matrix(t(arr1)[t(a1) == 1], nrow = sum(a1), byrow = TRUE)
  arr2 <- a2
  arr2[a2*t(a2)==1] <- 0
  arr2 <- arr2 + arr2*upper.tri(arr2,diag = FALSE)
  arrows2 <- matrix(t(arr2)[t(a2) == 1], nrow = sum(a2), byrow = TRUE)
  arr3 <- a3
  arr3[a3*t(a3)==1] <- 0
  arr3 <- arr3 + arr3*upper.tri(arr3,diag = FALSE)
  arrows3 <- matrix(t(arr3)[t(a3) == 1], nrow = sum(a3), byrow = TRUE)
  
  pdf(file=name1)
  plot(g1, layout=LO, edge.arrow.mode=arrows1)
  dev.off()
  pdf(file=name2)
  plot(g2, layout=LO, edge.arrow.mode=arrows2)
  dev.off()
  pdf(file=name3)
  plot(g3, layout=LO, edge.arrow.mode=arrows3)
  dev.off()
}
