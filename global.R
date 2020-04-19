if (!require(shiny))
  install.packages('shiny')
# make sure you load DT *after* shiny
if (!require(DT))
  install.packages('DT')
if (!require(uuid))
  install.packages('uuid')
#if(!require(plotly))install.packages("plotly")
if (!require(shinyBS))
  install.packages('shinyBS')
if (!require(networkD3))
  install.packages('networkD3')
if (!require(shinyjs))
  install.packages('shinyjs')
if (!require(magrittr))
  install.packages('magrittr')
if (!require(devtools))
  install.packages('devtools')
#if (!require(rcytoscapejs)) {
#    library(devtools)
#    devtools::install_github("cytoscape/r-cytoscape.js")
#    if (!require(htmlwidgets))
#        install.packages('htmlwidgets')
#}
if (!require(networkD3))
  install.packages('networkD3')
if (!require(MCL))
  install.packages('MCL')
if (!require(colourpicker))
  install.packages('colourpicker')
if (!require(threejs))
  install.packages("threejs")

library(BiocManager)
library(devtools)
#library(rcytoscapejs)
library(shiny)
library(shinyBS)
library(DT)
library(uuid)
library(networkD3)
library(magrittr)
library(shinyjs)
library(MCL)
library(colourpicker)
library(shinythemes)
library(visNetwork)
library(plotly)
library(networkD3)
library(threejs)

source("layouts_ui.R")
source("statistics.R")
source("netstats.R")
source("layout_choices.R")

#install.packages('shinyjqui')
#library(shinyjqui) ---> draggable objects, etc.


vranking_methods <- c(
  "Centralization degree" = "Centralization degree\tcentralization.degree(igraph)$res",
  "Centralization betweenness" = "Centralization betweenness\tcentralization.betweenness(igraph)$res",
  "Clustering coefficient" = "Clustering.coefficient\ttransitivity(igraph,type='local')",
  # "Alpha centrality"="Alpha centrality\talpha.centrality(igraph)",
  # "Kleinberg’s centrality"="Kleinberg centrality\tauthority.score(igraph)$vector",
  # "Topological sort"="Topological sort\ttopological.sort(igraph)",
  "Page rank" = "Page rank\tpage.rank(igraph)$vector",
  #"Betweenness centrality" = "Betweenness centrality\tbetweenness(igraph)",
  #"Bonacich power centrality scores" = "Bonacich power\tbonpow(igraph)",
  # "Closeness centrality"="Closeness centrality\tcloseness(igraph)",
  "Eccentricity" = "Eccentricity\teccentricity(igraph)",
  "Eigenvector centrality" = "Eigenvector centrality\tevcent(igraph)$vector",
  "Subgraph centrality" = "Subgraph centrality\tsubgraph.centrality(igraph)"
)

eranking_methods <- c("Betweenness centrality" = "Betweenness centrality\tedge.betweenness(igraph)")

if (!require(shiny)) install.packages("shiny")
# make sure you load DT *after* shiny
if (!require(DT)) install.packages("DT")
if (!require(igraph)) install.packages("igraph")
if (!require(plyr)) install.packages("plyr")
if (!require(d3Network)) install.packages("d3Network")
if (!require(uuid)) install.packages("uuid")
if (!require(lattice)) install.packages("lattice")
if (!require(PerformanceAnalytics)) install.packages("PerformanceAnalytics")
# if(!require(plotly))install.packages('plotly')
if (!require(shinyBS)) install.packages("shinyBS")
if (!require(networkD3)) install.packages("networkD3")
if (!require(shinyjs)) install.packages("shinyjs")
if (!require(devtools)) install.packages("devtools")
if (!require(randomcoloR)) install.packages("randomcoloR")
if (!require(Cairo)) install.packages("Cairo")
#if (!require(rcytoscapejs)) {
#    library(devtools)
#    devtools::install_github("cytoscape/r-cytoscape.js")
#    if (!require(htmlwidgets)) 
#        install.packages("htmlwidgets")
#}

# if (!require(GraphAlignment)){
#   source("https://bioconductor.org/biocLite.R")
#   biocLite("GraphAlignment")
# }

if (!require(VennDiagram)) install.packages("VennDiagram")
if (!require(reshape2)) install.packages("reshape2")
if (!require(dplyr)) install.packages("dplyr")
# install.packages("ggplot2")

library(devtools)
#library(rcytoscapejs)
library(shiny)
library(shinyBS)
library(DT)
library(igraph)
library(plyr)
library(d3Network)
library(uuid)
library(lattice)
# library(PerformanceAnalytics)
library(networkD3)
# library(plotly)
library(shinyjs)
library(Cairo)
library(randomcoloR)
library(MCL)
library(colourpicker)
# library(GraphAlignment)
library(VennDiagram)
library(reactlog) #debugging
library(visNetwork)
library(ggplot2)
library(reshape2) #new
library(dplyr) #new

# install.packages("r2d3")
# library(r2d3)

options(shiny.usecairo = F)
options(shiny.maxRequestSize=30*1024^2) #30 MB for uploaded networks
options(shiny.reactlog=TRUE) #debugging


max_edges = 50000
colors <- randomColor(50)


color_palette<- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#666666","#7FC97F",
           "#BEAED4","#FDC086","#FFFF99","#386CB0","#F0027F","#BF5B17","#A6CEE3","#1F78B4","#B2DF8A",
           "#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A","#B15928","#FBB4AE",
           "#B3CDE3","#CCEBC5","#DECBE4","#FED9A6","#FFFFCC","#E5D8BD","#FDDAEC","#F2F2F2","#B3E2CD",
           "#FDCDAC","#CBD5E8","#F4CAE4","#E6F5C9","#FFF2AE","#F1E2CC","#CCCCCC","#E41A1C","#377EB8",
           "#4DAF4A","#984EA3","#FFFF33","#A65628","#F781BF","#999999","#66C2A5","#FC8D62","#8DA0CB",
           "#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3","#8DD3C7","#FFFFB3","#BEBADA","#FB8072",
           "#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#095F02")




mapper<-function(value, istart, istop, ostart, ostop)
{ 
  return (ostart + (ostop - ostart) * ((value - istart) / (istop - istart)))
}

b64_1 <-base64enc::dataURI(file = "figures/NAP_Banner.png", mime = "image/png")
b64_2 <-base64enc::dataURI(file = "figures/upload.PNG", mime = "image/png")
b64_3 <-base64enc::dataURI(file = "figures/upload_layouts.PNG", mime = "image/png")
b64_4 <-base64enc::dataURI(file = "figures/upload_bipartite.PNG", mime = "image/png")
b64_5 <-base64enc::dataURI(file = "figures/upload_3d.PNG", mime = "image/png")
b64_6 <-base64enc::dataURI(file = "figures/topology_stat.PNG", mime = "image/png")
b64_7 <-base64enc::dataURI(file = "figures/topology_plots.PNG", mime = "image/png")
b64_8 <-base64enc::dataURI(file = "figures/ranking.PNG", mime = "image/png")
b64_9 <-base64enc::dataURI(file = "figures/ranking_plots.PNG", mime = "image/png")
b64_10 <-base64enc::dataURI(file = "figures/intersect_1.PNG", mime = "image/png")
b64_11 <-base64enc::dataURI(file = "figures/multilayer.PNG", mime = "image/png")
