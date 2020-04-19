netstats <- function(igraph,statistics){
	#   prepare<-as.data.frame(gsub("igraph","testing",statistics))
	if(length(statistics)==0)
		return(NULL)
	results<-list()
	for(i in statistics){
		tmp<-unlist(strsplit(i,"\t",fixed=T))
		description<-tmp[1]
		command<-tmp[2]
		# print(paste0("Calculating statistic: ",command))
		results[[description]]<-eval(parse(text=command))
	}
	return(data.frame(cbind(names(results),as.character(results))))
}


ranking <- function(igraph, method, nodeRanking = TRUE){
	if(length(method)==0)
		return(NULL)
	tmp<-unlist(strsplit(method,"\t",fixed=T))
	description<-tmp[1]
	command<-tmp[2]
	# print(paste0("Calculating ranking: ",command))
	res<-data.frame(as.vector(eval(parse(text=command))))
	if(nodeRanking)
		res$V2<-names(V(igraph))
	else{
		if(as.logical(is.directed(igraph))){
			edgesym <- "\U2794"
		}
		else{
			edgesym <- "\U2015"
		}
		res$V2<-apply(ends(igraph, E(igraph)), 1, paste, collapse=edgesym)
	}
	return(res[,c(2,1)])
}

