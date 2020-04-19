graph.data.frame(cat,directed=attr(cat, "directed"),vertices=NULL)->cat.igraph
get.adjacency(cat.igraph)->cat.adj
mcl(x=cat.adj,inflation=2.8,addLoops = F,max.iter=1000,allow1 = T)->res
