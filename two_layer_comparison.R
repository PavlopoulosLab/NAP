output_two_layer<- function(){
  fileConn <- file(paste("two_layer_comparison_", Sys.Date(),".html", sep = ""), "w")
  datasets <- fetchAllSelectedStoredDataset()
  
  if (length(datasets)<2) 
    return()
  dataset_1<- datasets[[1]]
  dataset_2<- datasets[[2]]
  
  unique_source1<- unique(dataset_1$Source)
  unique_target1<- unique(dataset_1$Target)
  nodes1<- c(as.character(unique_source1), as.character(unique_target1))
  unique_nodes1<- unique(nodes1)
  
  unique_source2<- unique(dataset_2$Source)
  unique_target2<- unique(dataset_2$Target)
  nodes2<- c(as.character(unique_source2), as.character(unique_target2))
  unique_nodes2<- unique(nodes2)
  
  merged_nodes<- c(as.character(nodes1), as.character(nodes2))
  unique_merged_nodes<- sort(unique(merged_nodes))
  
  inters_nodes<- intersect(unique_nodes1, unique_nodes2)
  
  
  ################ new with :1 ################
  dataset_1_source_new<- c(as.character(dataset_1$Source))
  dataset_1_source_new<-paste(dataset_1_source_new, ":1", sep = "")
  
  dataset_1_target_new<- c(as.character(dataset_1$Target))
  dataset_1_target_new<-paste(dataset_1_target_new, ":1", sep = "")
  
  df1<- data.frame("Source"= dataset_1_source_new, "Target"=dataset_1_target_new, "Colors" = "green")
  
  ################ new with :2 ################
  dataset_2_source_new<- c(as.character(dataset_2$Source))
  dataset_2_source_new<-paste(dataset_2_source_new, ":2", sep = "")
  
  dataset_2_target_new<- c(as.character(dataset_2$Target))
  dataset_2_target_new<-paste(dataset_2_target_new, ":2", sep = "")
  
  df2<- data.frame("Source"= dataset_2_source_new, "Target"=dataset_2_target_new, "Colors" = "red")
  
  ################ new with 3 ################
  if(length(inters_nodes)!=0){
  dataset_3_source_new<- c(as.character(inters_nodes))
  dataset_3_source_new<-paste(dataset_3_source_new, ":1", sep = "")
  
  dataset_3_target_new<- c(as.character(inters_nodes))
  dataset_3_target_new<-paste(dataset_3_target_new, ":2", sep = "")
  
  df3<- data.frame("Source"= dataset_3_source_new, "Target"=dataset_3_target_new, "Colors" = "yellow")
  } else{
    df3<- NULL
  }
  
  df_all<-rbind(df1, df2, df3)
  graph_df_all<- graph.data.frame(df_all)
  
  lay<- layout_choices(graph_df_all, lay)
  
  coord<- lay
  
  coord_as_vector_x<- as.vector(round(coord[,1]))
  coord_as_vector_y<- as.vector(round(coord[,2]))
  outliers_x<-boxplot(coord_as_vector_x,plot=F)$out
  outliers_y<-boxplot(coord_as_vector_y,plot=F)$out
  remove_outliers_x<- setdiff(coord_as_vector_x,outliers_x)
  remove_outliers_y<- setdiff(coord_as_vector_y,outliers_y)

  limit_max_x= max(remove_outliers_x)
  limit_min_x= min(remove_outliers_x)
  limit_max_y= max(remove_outliers_y)
  limit_min_y= min(remove_outliers_y)
  
  
  map_distance<-200
  if(length(merged_nodes)>2000){
    map_distance<-500
  }
  if(length(merged_nodes)>5000){
    map_distance<-800
  }
  
  map_x <- new.env(hash = TRUE)
  map_y <- new.env(hash = TRUE)
  for(i in 1:length(V(graph_df_all))){
    a<- names(V(graph_df_all)[i])
    x<- coord[i,1]
    y<- coord[i,2]
    if(x>limit_max_x || x< limit_min_x){
      x= runif(1, limit_min_x, limit_max_x)
    }
    if(y>limit_max_y || y< limit_min_y){
      y= runif(1, limit_min_y, limit_max_y)
    }
    xx<- mapper(x, limit_min_x, limit_max_x, -map_distance, map_distance)
    yy<- mapper(y, limit_min_y, limit_max_y, -map_distance, map_distance)
    
    map_x[[as.character(a)]]<- xx
    map_y[[as.character(a)]]<- yy
  }
  
  distance<- 400
  if(length(merged_nodes)>2000){
    distance<-800
  }
  if(length(merged_nodes)>5000){
    distance<-1200
  }
  
  middle_network==F
  map_z <- new.env(hash = TRUE)
  for(i in 1:length(unique_nodes1)){
    a<- paste(unique_nodes1[i], ":1", sep = "")
    map_z[[as.character(a)]]<- 0
  }
  for(i in 1:length(unique_nodes2)){
    a<- paste(unique_nodes2[i], ":2", sep = "")
    map_z[[as.character(a)]]<- distance
  }
  
  if(middle_network==T){
  for(i in 1:length(inters_nodes)){
    a<- paste(inters_nodes[i], ":1", sep = "")
    map_z[[as.character(a)]]<- distance/2
    a<- paste(inters_nodes[i], ":2", sep = "")
    map_z[[as.character(a)]]<- distance/2
  }
  }else{
    for(i in 1:length(inters_nodes)){
      a<- paste(inters_nodes[i], ":1", sep = "")
      map_z[[as.character(a)]]<- 0
      a<- paste(inters_nodes[i], ":2", sep = "")
      map_z[[as.character(a)]]<- distance
    }
  }
  
  map_colors <- new.env(hash = TRUE)
  for(i in 1:length(unique_nodes1)){
    a<- paste(unique_nodes1[i], ":1", sep = "")
    map_colors[[as.character(a)]]<- "blue"
  }
  for(i in 1:length(unique_nodes2)){
    a<- paste(unique_nodes2[i], ":2", sep = "")
    map_colors[[as.character(a)]]<- "red"
  }
  for(i in 1:length(inters_nodes)){
    a<- paste(inters_nodes[i], ":1", sep = "")
    map_colors[[as.character(a)]]<- "yellow"
    a<- paste(inters_nodes[i], ":2", sep = "")
    map_colors[[as.character(a)]]<- "yellow"
  }
  
  
  
  
  cat(sprintf("<head>
	<style> body { margin: 0; } </style>

	<script src=\"https://unpkg.com/3d-force-graph@1.60.12/dist/3d-force-graph.min.js\"></script>

</head>

<body>
  <div id=\"3d-graph\"></div>

  <script>
    const gData = {
		nodes: ["), file = fileConn)
  
  for (i in 1:length(V(graph_df_all))) {
    names_nodes<-names(V(graph_df_all)[i])
    if(i == length(V(graph_df_all))){
      cat(sprintf(paste("{\"id\":",i,",\"name\":\"",names_nodes,"\",\"propertyValue\":1,'fx':",map_x[[names_nodes]],", 'fy':",map_y[[names_nodes]],",'fz':",map_z[[names_nodes]],",\"color\":\"",map_colors[[names_nodes]],"\"}\n"), sep=""), file = fileConn)
    }else{
    cat(sprintf(paste("{\"id\":",i,",\"name\":\"",names_nodes,"\",\"propertyValue\":1,'fx':",map_x[[names_nodes]],", 'fy':",map_y[[names_nodes]],",'fz':",map_z[[names_nodes]],",\"color\":\"",map_colors[[names_nodes]],"\"},\n"), sep=""), file = fileConn)
    }
  }
  
  cat(sprintf("],
		links: ["), file = fileConn)
  
  compared_datasets<- rbind(df1, df2, df3)
  
  unique_names_nodes<-unique(names(V(graph_df_all)))
  
  for (i in 1:nrow(compared_datasets)) {
    if(i == nrow(compared_datasets)){
    cat(sprintf(paste("{\"source\":",which(unique_names_nodes %in% compared_datasets[i,1]),",\"target\":",which(unique_names_nodes %in%compared_datasets[i,2]),",\"value\":1}\n"), sep=""), file = fileConn)
    } else{
    cat(sprintf(paste("{\"source\":",which(unique_names_nodes %in% compared_datasets[i,1]),",\"target\":",which(unique_names_nodes %in%compared_datasets[i,2]),",\"value\":1},\n"), sep=""), file = fileConn)
    }
  }
  
  
  cat(sprintf("]};
    
	const Graph = ForceGraph3D()
		(document.getElementById('3d-graph'))
		.graphData(gData);
		//Graph.d3Force('charge').strength(-120);
  </script>
</body>"), file = fileConn)
  
}