output_hive_plot<- function(){
  fileConn <- file(paste("output_hive_plot", Sys.Date(),".html", sep=""), "w")
  igraph <- fetchFirstSelectedStoredIgraph()
  if (is.null(igraph)) 
    return()
  dataset <- fetchFirstSelectedStoredDataset()
  if (is.null(dataset)) 
    return()
  
  cat(sprintf("
<!DOCTYPE html>
  <meta charset=\"utf-8\">
    
    <style>
    .axis {
      stroke: #000;
        stroke-width:1.5px;
    }
  
  .node {
    stroke:#000;
  }
  
  .link {
    fill: none;
    stroke-width: 1.5px;
    stroke-opacity: 0.8;
  }
  
  .link.turnedOn {
    stroke-width: 3px;
  }
  
  .link.turnedOff {
    stroke-opacity: 0.3;
    stroke-width: 1px;
  }
  
  .node.turnedOn {
    stroke: red;
    stroke-width: 3px;
  }
  .tooltip{
		position: absolute;
		width: 50px;
		height: 30px;
		padding: 2px;
		font: 12px sans-serif;
		background: lightgreen;
		border: 0px;
		pointer-events: none;
	}
	/*.link:hover{
		stroke: red;
		stroke-opacity:.9;
	}*/
  
  </style>
    
    <body>
    
    <script src=\"https://d3js.org/d3.v3.min.js\"></script>
    <p id=\"info\">Loading…
    <script src=\"https://d3js.org/d3.hive.v0.min.js\"></script>
    
    <script>
    
    var width = 700,
  height = 600,
  innerRadius = 40,
  outerRadius = 240;
  
  var angle = d3.scale.ordinal()
  .domain(d3.range(4))
  .rangePoints([0, 2 * Math.PI]),
  radius = d3.scale.linear()
  .range([innerRadius, outerRadius]),
  color = d3.scale.category10()
  .domain(d3.range(20));
  
  var tooltip = d3.select(\"body\").append(\"div\")
					.attr(\"class\", \"tooltip\")
					.style(\"opacity\", 0);
  
  var nodesByName = {},
    links = [],
    formatNumber = d3.format(\",d\"),
    defaultInfo;
  
  var nodes = [\n"), file = fileConn)
  
  dataset_with_numbers<- data.frame("Source"= c(dataset$Source)-1, "Target"=c(dataset$Target)-1)

  dataset_with_numbers<- dataset_with_numbers[order(dataset_with_numbers$Source),]

  unique_source<- unique(dataset$Source)
  unique_target<- unique(dataset$Target)
  
  nodes_names<- c(as.character(unique_source), as.character(unique_target))
  nodes_names<- sort(unique(nodes_names))
  # print(sort(unique_nodes))
  
  nodes<- c(unique_source, unique_target)
  unique_nodes<- sort(unique(nodes))
  # print(sort(unique_nodes))
  
  # for (i in 1:round((length(unique_nodes))/2)) {
  for (i in 1:round((length(unique_nodes)))) {
    g<- runif(1, min=0, max=1)
    # g<- g/10
    m<- sample(0:2,1)
    # if(i == round((length(unique_nodes))/2)){
    if(i == round((length(unique_nodes)))){
      cat(sprintf(paste("\t{name:","\"", nodes_names[i],"\",", "group:", 1,",x:", m,",y:", g, "}\n"), sep= ""), file = fileConn)
      
    }else{
      cat(sprintf(paste("\t{name:","\"", nodes_names[i],"\",", "group:", 1,",x:", m,",y:", g, "},\n"), sep= ""), file = fileConn)
      
    }
  }
  
    
  cat(sprintf("
    ];
  
  var links = [\n"), file = fileConn)
  
  for (i in 1:nrow(dataset)) {
    if(i == nrow(dataset)){
      cat(sprintf(paste("\t{source: nodes[",  dataset_with_numbers$Source[i], "], target: nodes[", dataset_with_numbers$Target[i] ,"]}\n"), sep= ""), file = fileConn)
    }
    else{
      cat(sprintf(paste("\t{source: nodes[",  dataset_with_numbers$Source[i], "], target: nodes[",dataset_with_numbers$Target[i] ,"]},\n"), sep= ""), file = fileConn)
      
    }
  }
  
    cat(sprintf("];
  
  // Initialize the info display.
  var info = d3.select(\"#info\")
                .text(defaultInfo = \"Showing \" + formatNumber(links.length) + \" dependencies among \" + formatNumber(nodes.length) + \" classes.\");
                
  var svg = d3.select(\"body\").append(\"svg\")
  .attr(\"width\", width)
  .attr(\"height\", height)
  .append(\"g\")
  .attr(\"transform\", \"translate(\" + width/2 + \",\" + height/2 + \")\");
  
  svg.selectAll(\".axis\")
  .data(d3.range(3))
  .enter().append(\"line\")
  .attr(\"class\", \"axis\")
  .attr(\"transform\", function(d) { return \"rotate(\" + degrees(angle(d)) + \")\" })
  .attr(\"x1\", radius.range()[0])
  .attr(\"x2\", radius.range()[1]);
  
  // draw links
  svg.selectAll(\".link\")
  .data(links)
  .enter().append(\"path\")
  .attr(\"class\", \"link\")
  .attr(\"d\", d3.hive.link()
        .angle(function(d) { return angle(d.x); })
        .radius(function(d) { return radius(d.y); }))
  .style(\"stroke\", function(d) { return color(d.source.x); })
  .on(\"mouseover\", linkMouseover)
  .on(\"mouseout\", mouseout);
  
  // draw nodes
    svg.selectAll(\".node\")
    .data(nodes)
  .enter().append(\"circle\")
    .attr(\"class\", \"node\")
    .attr(\"transform\", function(d) { return \"rotate(\" + degrees(angle(d.x)) + \")\"; })
    .attr(\"cx\", function(d) { return radius(d.y); })
    .attr(\"r\", 5)
    .style(\"fill\", function(d) { return color(d.x); })
    .on(\"mouseover\", function(d){
    	tooltip.transition()
    		.duration(200)
    		.style(\"opacity\", .9);
    	tooltip.html(\"Node - \" + d.name + \"<br/>\" + \"Type - \" + d.x)
    		.style(\"left\", (d3.event.pageX + 5) + \"px\")
    		.style(\"top\", (d3.event.pageY - 28) + \"px\");
    })
    .on(\"mouseout\", function(d){
    	tooltip.transition()
    		.duration(500)
    		.style(\"opacity\", 0);
    });
  
  // highlight link and connected nodes on mouseover
  function linkMouseover(d) {
    svg.selectAll(\".link\")
    .classed(\"turnedOn\", function(dl) {
      return dl === d;
    })
    .classed(\"turnedOff\", function(dl) {
      return !(dl === d);
    })
    svg.selectAll(\".node\")
    .classed(\"turnedOn\", function(dl) {
      return dl === d.source || dl === d.target;
    })
  }
  
  // highlight node and connected links on mouseover
  function nodeMouseover(d) {
    svg.selectAll(\".link\")
    .classed(\"turnedOn\", function(dl) {
      return dl.source === d || dl.target === d;
    })
    .classed(\"turnedOff\", function(dl) {
      return !(dl.source === d || dl.target === d)
    });
    d3.select(this)
    .classed(\"turnedOn\", true);
  }
  
  // clear highlighted nodes or links
  function mouseout() {
    svg.selectAll(\".turnedOn\").classed(\"turnedOn\", false);
    svg.selectAll(\".turnedOff\").classed(\"turnedOff\", false);
  }
  
  function degrees(radians) {
    return radians / Math.PI * 180 - 90;
  }
  
  
  </script>
"), file = fileConn)
  
  
  
  
  
  close(fileConn)
}