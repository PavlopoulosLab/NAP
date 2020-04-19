library('igraph')

g = random.graph.game(500, p.or.m = 0.005, type = "gnp")
plot(g, layout = layout.fruchterman.reingold, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Random Network: G(N,p) model")
hist_g = hist(degree(g), main="random graph", col="Gray")


g1 = sample_smallworld(1,size=500,nei=4,p=0.03)
plot(g1, layout = layout.fruchterman.reingold, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Random Network: G(N,p) model")



h1= sample_pa (500) #scale-free
par(mfrow = c(1,1))
plot(h1, layout = layout.fruchterman.reingold, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Random Network: G(N,p) model")





