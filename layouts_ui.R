layouts<-c(
# "Auto"="Auto\tlayout.auto(igraph, dim=2)",
"Random"="Random\tlayout.random(igraph, dim=2)",
"Circle"="Circle\tlayout.circle(igraph)",
# "Sphere"="Sphere\tlayout.sphere(igraph)",
"Fruchterman-Reingold"="Fructerman\tlayout.fruchterman.reingold(igraph, dim=2)",
"Kamada-Kawai"="Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=2)",
"Spring"="Spring\tlayout.spring(igraph)",
# "Reingold-Tilford"="Reingold-Tilford\tlayout.reingold.tilford(igraph)",
"Fruchterman-Reingold Grid"="Fruchterman Grid\tlayout.fruchterman.reingold.grid(igraph)",
"Lgl"="Lgl\tlayout.lgl(igraph)"
# "Graphopt"="Graphopt\tlayout.graphopt(igraph)",
# "SVD"="SVD\tlayout.svd(igraph, d=shortest.paths(igraph))"
)

selected_layouts=c(
  "Lgl"="Lgl\tlayout.lgl(igraph)"
  )