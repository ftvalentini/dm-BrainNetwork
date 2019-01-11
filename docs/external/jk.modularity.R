jk.modularity <- function(M, n) {

  diag(M)<-0
  tmp<-sort(as.vector(M),decreasing = TRUE)
  ro = tmp[n]
  M.b = (M>ro)
  netM <- graph.adjacency(M.b,mode="undirected")
  netM.cl.eb <- cluster_edge_betweenness(netM, directed = F, merges = T)
  
  mout = modularity(netM,netM.cl.eb$membership)
  return(mout)	
  
  }
  