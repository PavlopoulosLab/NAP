output<- function(){
  fileConn <- file(paste("output", Sys.Date(),".html", sep = ""), "w")
  igraph <- fetchFirstSelectedStoredIgraph()
  if (is.null(igraph)) 
    return()
  dataset <- fetchFirstSelectedStoredDataset()
  if (is.null(dataset)) 
    return()

  
 cat(sprintf("<head>
  <style> body { margin: 0; } 
  </style>

  <script src=\"//unpkg.com/3d-force-graph\"></script>
  <!--<script src=\"../../dist/3d-force-graph.js\"></script>-->
</head>


<body>
  <div id=\"3d-graph\"></div>

  <script>\n"), file = fileConn)

 cat(sprintf("
    const gData = 
    {
    \"nodes\": [\n"), file = fileConn)
 unique_source<- unique(dataset$Source)
 unique_target<- unique(dataset$Target)
 nodes<- c(as.character(unique_source), as.character(unique_target))
 unique_nodes<- unique(nodes)
 
 g<- 1
    for (i in 1:length(unique_nodes)) {
      if(is.bipartite(igraph)){
          if(bipartite_mapping(igraph)$type[i]==T){
            g<-1
          }
          if(bipartite_mapping(igraph)$type[i]==F){
            g<-2
          }
          if(i == length(unique_nodes)){
            cat(sprintf(paste("\t{\"id\":", "\"", unique_nodes[i],"\"", ",\"group\":", g, ",\"size\":", 1, "}"), sep= ""), file = fileConn)
          }else{
            cat(sprintf(paste("\t{\"id\":", "\"", unique_nodes[i], "\"", ",\"group\":", g, ",\"size\":", 1,"},\n"), sep= ""), file = fileConn)
          }
      }else{

      if(i == length(unique_nodes)){
        cat(sprintf(paste("\t{\"id\":", "\"", unique_nodes[i],"\"", ",\"group\":", g, ",\"size\":", 1, "}"), sep= ""), file = fileConn)
      }else{
        # g<- sample(1:10,1)
        cat(sprintf(paste("\t{\"id\":", "\"", unique_nodes[i], "\"", ",\"group\":", g, ",\"size\":", 1,"},\n"), sep= ""), file = fileConn)
      }
      }
    }
 
 cat(sprintf("],\n"), file = fileConn)
 
 cat(sprintf("\t\"links\": [\n"), file = fileConn)
 
 for (i in 1:nrow(dataset)) {
   if(is.weighted(igraph)){
     weight<- dataset$Weight[i]
   } else{
     weight<- 1
   }
     if(i == nrow(dataset)){
   cat(sprintf(paste("\t{\"source\":", "\"", dataset$Source[i], "\"", ",\"target\":", "\"",dataset$Target[i],"\"", ",\"value\":", weight,"}\n"), sep= ""), file = fileConn)
     }
     else{
       cat(sprintf(paste("\t{\"source\":", "\"", dataset$Source[i], "\"", ",\"target\":", "\"",dataset$Target[i],"\"", ",\"value\":", weight,"},\n"), sep= ""), file = fileConn)
       
     }
     }
 
 cat(sprintf("]\n};\n"), file = fileConn)
 
 
 cat(sprintf("let selectedNodes = new Set();\n"), file = fileConn)
 
 
 #.backgroundColor('#ffffff')
 cat(sprintf("const Graph = ForceGraph3D()
 (document.getElementById('3d-graph'))
 .graphData(gData)
 .nodeRelSize(9)
 //.nodeColor(node => selectedNodes.has(node) ? 'yellow' : 'lightblue')
 .onNodeClick((node, event) => {
   if (event.ctrlKey || event.shiftKey || event.altKey) { // multi-selection
     selectedNodes.has(node) ? selectedNodes.delete(node) : selectedNodes.add(node);
   } else { // single-selection
     const untoggle = selectedNodes.has(node) && selectedNodes.size === 1;
     selectedNodes.clear();
     !untoggle && selectedNodes.add(node);
   }
   Graph.nodeColor(Graph.nodeColor()); // update color of selected nodes
 })
 .onNodeDrag((node, translate) => {
   if (selectedNodes.has(node)) { // moving a selected node
     [...selectedNodes]
     .filter(selNode => selNode !== node) // don't touch node being dragged
              .forEach(node => ['x', 'y', 'z'].forEach(coord => node[`f${coord}`] = node[coord] + translate[coord])); // translate other nodes by same amount
          }
        })
        .onNodeDragEnd(node => {
          if (selectedNodes.has(node)) { // finished moving a selected node
            [...selectedNodes]
              .filter(selNode => selNode !== node) // don't touch node being dragged
     .forEach(node => ['x', 'y', 'z'].forEach(coord => node[`f${coord}`] = undefined)); // unfix controlled nodes
   }
 })"), file = fileConn)
 
 cat(sprintf(paste("
  .linkWidth('value')
                   "), sep=""),file = fileConn)
 
 cat(sprintf("
 .nodeLabel('id')
 .nodeAutoColorBy('group')
 .onNodeDragEnd(node => {
          node.fx = node.x;
          node.fy = node.y;
          node.fz = node.z;
        })
             "), file = fileConn)
 
 if(is.directed(igraph)){
   cat(sprintf("
    .linkDirectionalArrowLength('value')
    .linkDirectionalArrowRelPos(1);"), file = fileConn)
 } else{cat(sprintf(";"), file = fileConn)
            }
 
 

 cat(sprintf("
  </script>
</body>
"), file = fileConn)


     close(fileConn)
}