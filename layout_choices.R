layout_choices <- function(igraph,layouts_ui){
  if(length(layouts_ui)==0)
    return(NULL)
  results<-list()
  for(i in layouts_ui){
    tmp<-unlist(strsplit(i,"\t",fixed=T))
    description<-tmp[1]
    command<-tmp[2]
    results[[description]]<-eval(parse(text=command))
  }
  return(results[[description]])
}
