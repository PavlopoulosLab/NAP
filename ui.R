ui_options <- c("ui_table_line_height" = "80%")

ui_css <- paste0(
    '
    .box-panel {
    border-radius: 0 0 5px 5px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-style: none solid solid solid;
    }
    .box-panel-padding {
    padding: 20px 20px 20px 20px;
    }
    .tabBox-panel {
    padding: 20px 20px 20px 20px;
    border-width: 1px;
    border-color: #d7d7d7;
    border-radius: 0px 0px 8px 8px;
    border-style: none solid solid solid;
    background: #ffffff;
    }

    .centerBlock {
    float: none;
    margin: 0 auto;
    }

    ',
    ".dataTables_wrapper td {
    line-height: ",
    ui_options['ui_table_line_height'],
    ";
    }"
)

ui_dataTable_panel <- function(datasetName, pagination = TRUE) {
    return(
        parse(
            text = paste0("div(div(DT::dataTableOutput('", datasetName, "'), class='box-panel-padding'), class='box-panel')"
            )
        )
    )
}

fixedPage(
  # shinythemes::themeSelector(),  # <--- Add this somewhere in the UI
  theme = shinytheme("flatly"),
    useShinyjs(),
  tags$head(tags$script(src = "cyjs.js")),
  tags$img(src = b64_1),
    tags$head(tags$script(src = "cyjs.js")),
    navbarPage(
        "NAP(v2.0): The Network Analysis Profiler",
        header = tags$head(tags$style(type = "text/css", ui_css)),
        tabPanel(
            "Welcome",
            icon = icon("fas fa-door-open", lib = "font-awesome"),
            
            h2("Welcome to the Network Analysis Profiler"),
            strong(
                "a mediator for shaking hands between graph theory and systems biology."
            ),
            br(),
            br(),
            helpText(
                "Upload networks with up to few thousand edges and perform your analysis. NAP is designed to profile the topology of medium-scale networks with main emphasis in:"
            ),
            tags$ul(
                tags$li("Exploring topological features"),
                tags$li("Ranking nodes/edge by topological properties"),
                tags$li(
                "Simultaneously comparing topological features of several networks "
                ),
                tags$li("Plot the distribution of any topological feature"),
                tags$li("Plot any of the topological features against another"),
                tags$li("Use various layouts to see the network both in a dynamic/static way")
            )
        ),
        # tabPanel 'Welcome'
        tabPanel(
            "Upload File(s)",
            icon = icon("upload"),
            sidebarLayout(
                sidebarPanel(
                    bsAlert("tabUploadSideAlert"),
                    helpText("Please follow the steps below:"),
                    tags$ul(
                        tags$li("Load your network file in tab delimited format"),
                        tags$li("Choose the network type"),
                        tags$li("Give it a name"),
                        tags$li("Hit the ADD button")
                    ),
                    selectInput(
                        "uiLoadGraphOptionsInput",
                        "Choose input method",
                        c(
                            "File upload" = "oF",
                            "Random Barabasi-Albert" = "oR_Barbasi",
                            "Random Erdos-Renyi" = "oR_Erdos_Renyi",
                            "Random Watts-Strogatz small-world" = "oR_Watts_Strogatz",
                            "Random Bipartite" = "oR_Bipartite"
                        )
                    ),
                    uiOutput("uiLoadGraphOptionsOutput"),
                    div(
                        span(
                            actionButton("btnAddNetwork", "ADD", icon = icon("plus")), class = "input-group-btn"
                        ),
                        tags$input(
                            id = "networkName",
                            type = "text",
                            class = "form-control",
                            placeholder = "Type network name ..",
                            value = "Network name"
                        ),
                        class = "input-group"
                    ),
                    uiOutput("uiStoredGraphsOutputRadio"),
                    hr()
                ),
                mainPanel(
                    conditionalPanel(
                        condition = "input.availableNetworks == 0",
                        bsAlert("tabUploadMainAlert"),
                        uiOutput("uiStoredGraphsOutputSelectUpload"),
                        tabsetPanel(
                            tabPanel(
                                "Table View",
                                icon = icon("table"),
                                eval(ui_dataTable_panel('datasettab1'))
                            ),
                            # tabPanel("Visualization",
                            #     div(
                            #         div(
                            #             textOutput('info_vertices1'),
                            #             textOutput('info_edges1'),
                            #             visNetworkOutput('tabVizIgraphForce', width = 600, height = 600),
                            #             # plotOutput('tabVizIgraphForce'),
                            #             class = 'box-panel-padding'
                            #         ),
                            #         class = 'box-panel'
                            #     )
                            # ),
                            tabPanel(
                              "Network 2D",
                              icon = icon("sitemap"),
                              # sidebarLayout(
                              #   sidebarPanel(
                                  bsAlert("tabLayoutsMainAlert"),
                              br(),
                                  selectInput(
                                    'combobox_layouts',
                                    'Choose the layout:',
                                    choices = sort(
                                      c(
                                        "Random" = "random",
                                        "Circle" = "circle",
                                        "Sphere" = "sphere",
                                        "Fruchterman-Reingold" = "fruchterman",
                                        "Kamada-Kawai" = "kamada",
                                        "Reingold-Tilford" = "reingold",
                                        "Grid" = "grid",
                                        "Lgl" = "lgl"
                                      )
                                    ),
                                    selected = "random"
                                  ),
                                conditionalPanel(condition = "input.availableNetworks == 0",
                                                 mainPanel(
                                                   # tabsetPanel( 
                                                   #   tabPanel("Plot of the Layout", 
                                                 
                                                 # div(
                                                 #   div(
                                                 visNetworkOutput(outputId = "plot_layout", width = 700, height = 700),
                                                 # class = 'box-panel-padding'
                                                 #   ),
                                                 # class = 'box-panel'
                                                 # ),
                                                 br(),
                                                 br(),
                                                 div(
                                                   div(
                                                 helpText("Download the coordinates of the Selected Layout."),
                                                 downloadButton("dwnld_layout", ""),
                                                 helpText("Download the two projection graphs."),
                                                 div(
                                                 style = "display: inline-block",downloadButton("dwnld_projections1", "")),
                                                 div(
                                                 style = "display: inline-block",downloadButton("dwnld_projections2", "")),
                                                 
                                                 
                                                 class = 'box-panel-padding'
                                                   )
                                                 )
                                )
                                )
                              # )
                            ),# tabPanel 'Layouts'
                            tabPanel(
                              "Network 3D",
                              icon = icon("sitemap"),
                              uiOutput("network3d")
                            )
                            # ,
                            # tabPanel(
                            #   "Hive Plot",
                            #   icon = icon("sitemap"),
                            #   uiOutput("hive_plot")
                            # 
                            # )
                            # ,
                            # tabPanel(
                            #   "Arc Diagram",
                            #   icon = icon("sitemap"),
                            #   radioButtons("grouping", label = "Group by:",
                            #                choices = c("Group", "Name", "Frequency"), inline = T,
                            #                choiceNames = c("Group", "Name", "Frequency")),
                            #   uiOutput("arc_diagram")
                            # 
                            # )
                        #,
                        #tabPanel("Vertices",
                        # div(
                        # textOutput('vertices1_label'),
                        # verbatimTextOutput('vertices1'),
                        # class='box-panel-padding')
                        #),
                        #tabPanel("Edges",
                        # div(
                        # textOutput('edges1_label') ,
                        # verbatimTextOutput('edges1'),
                        # class='box-panel-padding')
                        #)
                        )
                    )
                ) # mainPanel
            ) # sidebarLayout
        ),
        # tabPanel 'Upload File(s)'
        tabPanel(
            "Topology",
            icon = icon("fab fa-connectdevelop", lib = "font-awesome"),
            sidebarLayout(
                sidebarPanel(
                    bsAlert("tabTopologySideAlert"),
                    conditionalPanel(
                        condition = "input.statisticsMethodsMainTabsetPanel == 'tableView'",
                        uiOutput("uiStoredGraphsOutputSelectTopolopgy")
                    ),
                    conditionalPanel(
                        condition = "input.statisticsMethodsMainTabsetPanel == 'plotView'",
                        uiOutput("uiStoredGraphsOutputMultipleSelectTopolopgy")
                    ),
                    # helpText("Select the statistics you want in the analysis."),
                    checkboxGroupInput(
                        "statistics",
                        "The statistics:",
                        choices = statistics,
                        selected = selected_statistics
                    ),
                    div(
                        span(actionButton("btnStatSelectAll", "Select all", style = "float: right;"), class = "input-group-btn"),
                        span(actionButton("btnStatSelectNone", "Clear"), class = "input-group-btn"),
                        class = "input-group"
                    )
                ),
                conditionalPanel(
                    condition = "input.availableNetworks == 0",
                    mainPanel(
                        bsAlert("tabTopologyMainAlert"),
                        tabsetPanel(
                            id = "statisticsMethodsMainTabsetPanel",
                            tabPanel(
                                "Statistics",
                                value = "tableView",
                                icon = icon("table"),
                                eval(ui_dataTable_panel("statres", FALSE))
                            ),
                            tabPanel(
                                "The Plots",
                                value = "plotView",
                                icon = icon("bar-chart"),
                                div(
                                    div(
                                        uiOutput("statisticsMethodsPlotRender"),
                                        class = 'box-panel-padding'
                                    ), class = 'box-panel'
                                ),
                                sliderInput(
                                    "statisticsPlotPercentMagnify",
                                    "Adjust plot height (% taller)",
                                    min = 0,
                                    max = 100,
                                    value = 0
                                )
                                # ,
                                # span(
                                #     actionButton("btnRefreshPalette", "Refresh palette", icon = icon("refresh"))
                                # )
                            )
                        )
                    )
                )
            )
        ),
        # tabPanel 'Topology'
        # tabPanel("Clustering", 
        #          # icon = icon("fab fa-bezier-curve", lib = "font-awesome"),
        #          icon = icon("fas fa-braille", lib = "font-awesome"),
        #          sidebarLayout(
        #            sidebarPanel(
        #              bsAlert("tabClusteringSideAlert"),
        #              conditionalPanel( condition = "input.clusteringMethodsMainTabsetPanel == 'tableView'",
        #                                uiOutput("uiStoredGraphsOutputSelectClustering")
        #              ),
        #              conditionalPanel( condition = "input.clusteringMethodsMainTabsetPanel == 'plotView'",
        #                                uiOutput("uiStoredGraphsOutputMultipleSelectClustering")
        #              ),
        #              helpText("Select the inflation value for MCL."),
        #              sliderInput(
        #                "inflation","Default inflation value is 2.5.",
        #                value = 2.5,
        #                min = 2.0,
        #                max = 6.0,
        #                step = 0.1
        #              )
        #            ),
        #            conditionalPanel( condition = "input.availableNetworks == 0",
        #                              mainPanel(
        #                                bsAlert("tabClusteringMainAlert"),
        #                                tabsetPanel(id = "clusteringMethodsMainTabsetPanel",
        #                                            tabPanel("Clustering Results", value = "tableView", icon = icon("table"), eval(ui_dataTable_panel("clusteringres", FALSE)))
        #                                            
        #                                )
        #                              )
        #            )
        #          )
        # ),
        #tabPanel 'Ranking'
        tabPanel(
            "Ranking",
            icon = icon("sort-amount-desc"),
            sidebarLayout(
                sidebarPanel(
                    bsAlert("tabRankingSideAlert"),
                    conditionalPanel(
                        condition = "input.rankingMethodsMainTabsetPanel == 'tableView'",
                        uiOutput("uiStoredGraphsOutputSelectRanking"),
                        tabsetPanel(
                            id = 'rankingTableViewTabsetPanel',
                            tabPanel(
                                "Node ranking",
                                radioButtons("vrankingTableView", label = "Method", choices = vranking_methods),
                                class = 'tabBox-panel'
                            ),
                            tabPanel(
                                "Edge ranking",
                                radioButtons("erankingTableView", label = "Method", choices = eranking_methods),
                                class = 'tabBox-panel'
                            )
                        )
                    ),
                    conditionalPanel(
                        condition = "input.rankingMethodsMainTabsetPanel == 'plotView'",
                        uiOutput("uiStoredGraphsOutputMultipleSelectRanking"),
                        tabsetPanel(
                            id = 'rankingPlotViewTabsetPanel',
                            tabPanel(
                                "Node ranking",
                                checkboxGroupInput("vrankingPlotView", "Method", choices = vranking_methods),
                                class = 'tabBox-panel'
                            ),
                            tabPanel(
                                "Edge ranking",
                                checkboxGroupInput("erankingPlotView", "Method", choices = eranking_methods),
                                class = 'tabBox-panel'
                            )
                        )
                    )
                ),
                conditionalPanel(condition = "input.availableNetworks == 0",
                    mainPanel(
                        bsAlert("tabRankingMainAlert"),
                        tabsetPanel(
                            id = "rankingMethodsMainTabsetPanel",
                            tabPanel(
                                "Table View",
                                value = "tableView",
                                icon = icon("table"),
                                eval(ui_dataTable_panel("rankeddatasettab1"))
                            ),
                            tabPanel(
                                "Plots",
                                value = "plotView",
                                icon = icon("bar-chart"),
                                conditionalPanel(
                                    condition = "(input.rankingPlotViewTabsetPanel == 'Node ranking' && input.vrankingPlotView.length == 1) || (input.rankingPlotViewTabsetPanel == 'Edge ranking' && input.erankingPlotView.length == 1)",
                                    div(div(
                                    # plotOutput("rankingMethodsPowerLawScatter"),
                                    uiOutput("rankingMethodsScatterPlotRender1"),
                                    class = 'box-panel-padding'
                                    ), class = 'box-panel'),
                                    sliderInput(
                                        "scatterPlotMatrixPercentMagnify1",
                                        "Adjust plot height (% taller)",
                                        min = 0,
                                        max = 100,
                                        value = 0
                                    )
                                ),
                                #conditionalPanel
                                conditionalPanel(
                                    condition = "(input.rankingPlotViewTabsetPanel == 'Node ranking' && input.vrankingPlotView.length > 1) || (input.rankingPlotViewTabsetPanel == 'Edge ranking' && input.erankingPlotView.length > 1)",
                                    div(div(
                                    #plotlyOutput('rankingMethodsScatterPlot', height="auto"),
                                    #plotOutput('rankingMethodsScatterPlot'),
                                    uiOutput("rankingMethodsScatterPlotRender"),
                                    class = 'box-panel-padding'
                                    ), class = 'box-panel'),
                                    div(
                                        sliderInput(
                                            "scatterPlotMatrixPercentMagnify",
                                            "Adjust plot height (% taller)",
                                            min = 0,
                                            max = 100,
                                            value = 0
                                        ),
                                        checkboxInput('IsRankingRegressionEnabled', 'Regression line', T)
                                    )
                                ), #conditionalPanel
                                conditionalPanel(
                                  condition = "input.vrankingPlotView.length > 0 || input.erankingPlotView.length > 0",
                                  div(
                                    helpText("Download the plotted data."),
                                    style = "display: inline-block"
                                  ),
                                  div(
                                    downloadButton("dwnld_ranking_plot", ""),
                                    style = "display: inline-block"
                                  )
                                )
                            ) #tabPanel
                        ) #tabsetPanel
                    ) # mainPanel
                ) #conditionalPanel
            ) #sidebarLayout
        ),
        # tabPanel 'Ranking'
        tabPanel(
          "Intersection",
          icon = icon("fas fa-adjust", lib = "font-awesome"),
          sidebarLayout(
            sidebarPanel(
              bsAlert("tabIntersectionMainAlert"),
              uiOutput("uiStoredGraphsOutputIntersectionSelect1"),
              uiOutput("uiStoredGraphsOutputIntersectionSelect2"),
              wellPanel(
                h4("Options"),
                # div(
                #   span(
                #     actionButton("btnRefreshPalette2", "Refresh palette", icon = icon("refresh"))
                #   ),
                #   style="width: inherit; display: inline-flex; display: -webkit-inline-flex; display: -ms-inline-flex; justify-content: center; -webkit-justify-content: center; -ms-justify-content: center"
                # ),
                # helpText("Download intersection graph data."),
                # downloadButton("dwnld_intersection", ""),
                br(),
                helpText("Download the Intersecting Network."),
                downloadButton("dwnld_commNet", ""),
                helpText("Export 3D Multilayered network."),
                downloadButton("dwnld_multi_arena", ""),
                br()
              )
            ),
            conditionalPanel(
              condition = "input.storedGraphsOutputIntersectionSelect2 != input.storedGraphsOutputIntersectionSelect1",
              mainPanel(
                tabsetPanel(
                  tabPanel(
                    "Edges Intersection",
                    uiOutput("uiIntersectionVenn_Edges"),
                    eval(ui_dataTable_panel("venn_table_edges", FALSE))
                ),
                tabPanel(
                    "Nodes Intersection",
                    uiOutput("uiIntersectionVenn_Nodes"),
                    eval(ui_dataTable_panel("venn_table_nodes", FALSE))
                ),
                tabPanel(
                    "Intersecting Network (2D)",
                    br(),
                visNetworkOutput("IntersectionNet")
               
                ), #tabPanel
                tabPanel(
                  "Multilayer Intersection (3D)",
                  icon = icon("sitemap"),
                  br(),
                  selectInput(
                    'combobox_layouts2',
                    'Choose the layout:',
                    choices = layouts,
                    selected = selected_layouts,
                    multiple = FALSE
                  ),
                  checkboxInput("middle_network",label = "Place common nodes in a middle layer.", 
                                value = F),
                  uiOutput("two_layer_network")
                  
                )
                
                )#tabsetPanel
              )#mainPanel
            )#conditionalPanel
          )#sidebarLayout
        ),#tabPanel Intersection
        
        tabPanel(
            "Help/About",
            icon = icon("question"),
            tabsetPanel(
        tabPanel(
            "INPUT FILE",
            # icon = icon("fas fa-info", lib = "font-awesome"),
            br(),
            strong("The input file can be a network in tab delimited format."),
            br(),
            helpText(
                "The network file

NAP accepts as input, an obligatory, tab-delimited file, containing all network connections. This file must contain headers, namely: 'Source' and 'Target'. A 2-column file represents the connections between the nodes and refers to an unweighted network, whereas a third column can be used for weights. Each network can be handled as directed or undirected.
"
            ),
            pre(
                "
                Source  Target  Weight
                A       B       3
                A       C       3
                A       D       4
                B       C       1
                C       D       1
                A       E       4
                A       F       5
                D       D1      3
                A1      B1      3
                A1      C1      3
                A1      D1      4
                B1      C1      1
                C1      D1      1
                A1      E1      4
                A1      F1      5"
            )),
            tabPanel( "EXAMPLES",
                      br(),
            helpText("NAP comes with various network generation options. Users are able to generate:"),
            helpText(tags$ul(
              tags$li("A random scale-free network (Barabasi-Albert)"),
              tags$li("A completely random network without any special topology (Erdos-Renyi)"),
              tags$li("A random network with a small world topology (Watts-Strogatz)"),
              tags$li("A bipartite network"))),
            helpText("For any of the options, users can define the network sizeof by adjusting the number of nodes and choose whether it will be directed or undirected. Notably, users can upload or generate as many networks as they like. Every time a network is uploaded, a name must be given first. "),
            helpText("Download examples:"),
            downloadLink('gavin_2002', "1) Download Gavin_2002 network here"),
            br(),
            downloadLink('gavin_2006', "2) Download Gavin_2006 network here"),
            br(),
            br(),
            br()
            ),
        tabPanel( "THE UPLOAD TAB",
                  br(),
                      strong("Usage:"),
                      helpText("Users can upload or generate as many networks as they like. Every time a network is uploaded, a name can be given first. Once a network file has been named and uploaded, it will appear as an option in any of the NAP’s dropdown selection lists. Users can remove indifferent networks at any time."),
                      tags$img(src = b64_2),
                  br(),
                  br(),
                  strong("Table View:"),
                  helpText("Users can select a network or an annotation file at a time and see its content as an interactive table. Notably, one can search by suffix in the table, using the Search field."),
                  strong("Network 2D:"),
                  helpText("This Tab offers a dynamic network visualization in its simplest form. Nodes are connected with edges and their coordinates are calculated using any of the offered layouts. The network is fully interactive as zooming, dragging and panning are allowed either by using the mouse or the navigation buttons.
                           In addition, nodes can be selected and dragged anywhere on the plane, whereas the first neighbors of any node can be highlighted upon selection. Finally, the network view is automatically updated when a different network is selected."),
                  strong("Layouts:"),
                  helpText("Currently, NAP hosts the following layouts:"),
                  helpText(tags$ul(
                    tags$li("Fruchterman-Reingold: It places nodes on the plane using the force-directed layout algorithm developed by Fruchterman and Reingold."),
                    tags$li("Random: This function places the vertices of the graph on a 2D plane uniformly using random coordinates."),
                    tags$li("Circle: It places vertices on a circle, ordered by their vertex ids."),
                    tags$li("Kamada-Kawai: This layout places the vertices on a 2D plane by simulating a physical model of springs."),
                    tags$li("Reingold-Tilford: This is a tree-like layout and is suitable for trees or graphs without many cycles."),
                    tags$li("LGL: A force directed layout suitable for larger graphs."),
                    tags$li("Grid: This layout places vertices on a rectangular 2D grid."),
                    tags$li("Sphere: This layout places vertices on a rectangular 3D-like sphere.")
                  )
                    ),
                  tags$img(src = b64_3),
                  br(),
                  strong("Bipartite graphs:"),
                  helpText("In the case of bipartite graphs, users can export and subsequently visualize the two projection networks."),
                  tags$img(src = b64_4),
                  br(),
                  strong("Network 3D:"),
                  helpText("NAP offers a fully interactive 3D network visualization using a force-directed layout. Users can zoom-in and out and interactively drag and drop any node or the whole network"),
                  tags$img(src = b64_5)
            ),
        tabPanel( "THE TOPOLOGY TAB",
                  br(),
                  strong("Statistics"),
                  helpText("In this Tab users can explore various topological features of each network. Shortly, these are: "),
                  helpText(tags$ul(
                    tags$li("Number of Nodes: 
                    Shows the number of nodes in the network."),
                    tags$li("Number of Edges: 
                    Shows the number of connections in the network."),
                    tags$li("Diameter: 
                            Shows the length of the longest geodesic. The diameter is calculated by using a breadth-first search like method. The graph-theoretic or geodesic distance between two points is defined as the length of the shortest path between them."),
                    tags$li("Radius: 
                            The eccentricity of a vertex is its shortest path distance from the farthest other node in the graph. The smallest eccentricity in a graph is called its radius. The eccentricity of a vertex is calculated by measuring the shortest distance from (or to) the vertex, to (or from) all vertices in the graph, and taking the maximum."),
                    tags$li("Density:
                            The density of a graph is the ratio of the number of edges divided by the number of possible edges."),
                    tags$li("Average Path Length: 
                            The average number of steps needed to go from a node to any other."),
                    tags$li("Clustering Coefficient: 
                            A metric to show if the network has the tendency to form clusters."),
                    tags$li("Modularity:
                            This function calculates how modular is a given division of a graph into subgraphs."),
                    tags$li("Number of Self Loops: 
                            How many nodes are connected to themselves."),
                    tags$li("Average Eccentricity: 
                            The eccentricity of a vertex is its shortest path distance from the farthest node in the graph."),
                    tags$li("Average Eigenvector Centrality:
                            It is a measure of the influence of a node in a network."),
                    tags$li("Assortativity Degree:
                            The assortativity coefficient is positive is similar vertices (based on some external property) tend to connect to each, and negative otherwise."),
                    tags$li("Directed Acyclic Graph:
                            It returns True (1), if there are circular paths in the graph or False (0), elsewhere."),
                    tags$li("Average Number of Neighbors:
                            How many neighbors each node of the network has on average."),
                    tags$li("Centralization Betweenness: 
                            It is an indicator of a node's centrality in a network. It is equal to the number of shortest paths from all vertices to all others that pass through that node.Betweenness centrality quantifies the number of times a node acts as a bridge along the shortest path between two other nodes."),
                    tags$li("Centralization Closeness: 
                            It measures the speed with which randomly walking messages reach a vertex from elsewhere in the graph."),
                    tags$li("Centralization Degree: 
                            It is defined as the number of links incident upon a node."),
                    tags$li("Graph Mincut: 
                            Calculates the minimum st-cut between two vertices in a graph. The minimum st-cut between source and target is the minimum total weight of edges needed to remove to eliminate all paths from source to target."),
                    tags$li("Motifs-3:
                            Searches a graph for motifs of size 3."),
                    tags$li("Motifs-4:
                            Searches a graph for motifs of size 4.")
                  )
                  ),
                  br(),
                  tags$img(src = b64_6),
                  br(),
                  strong("The Plots"),
                  helpText("In this Tab, users can directly compare the topological features between any of the selected networks. Each network will be assigned a different color and each topological feature will be shown as a separate bar chart. "),
                  tags$img(src = b64_7)
        ),
        tabPanel( "THE RAKING TAB",
                  br(),
                  strong("Table View"),
                  helpText("In this Tab, users can rank a network’s nodes and edges according to some topological features and see them in an interactive table. "),
                  br(),
                  helpText("Nodes can be ranked by:"),
                  helpText(tags$ul(
                    tags$li("Centralization degree
"),
                    tags$li("Centralization betweenness 
"),
                    tags$li("Clustering coefficient
"),
                    tags$li("Page rank
"),
                    tags$li("Eccentricity
"),
                    tags$li("Eigenvector centrality
"),
                    tags$li("Subgraph centrality
")
                    )),
                  helpText("Edges are only ranked by:"),
                  helpText(tags$ul(
                    tags$li("Betweenness centrality"))),
                  tags$img(src = b64_8),
                  br(),
                  strong("Plots"),
                  helpText("Users can select one network at a time and see the distribution of each topological metric. In addition, users have the ability to generate a distribution plot showing any topological feature against any other within a selected network. A high-resolution 2D scatterplot is generated on the fly, displaying the distribution of a chosen topological parameter in a histogram-like view. Should the user desire to explore more than one topological parameter at a time, NAP gives the user the opportunity to generate on-the-fly advanced plots by pairwise comparing any topological feature of a network against any other feature within the same network. This matrix-like plot showing pairwise correlations of any combination between the selected topological features is not limited to the number of features to be plotted. The upper triangular part of the plot shows the numerical correlation between any pair of topological features whereas the lower-triangular part of the matrix the scatterplot of one feature against another."),
                  tags$img(src = b64_9)
                  ),
        tabPanel( "THE INTERSECTION TAB",
                  br(),
                  strong("Edge Intersection"),
                  helpText("Users can automatically extract the common edges between any selected pair of networks. Once two networks have been selected, a Venn diagram will be generated showing the edge overlap between the two selected networks. The common edges are also reported in Table view too."),
                  br(),
                  strong("Node Intersection"),
                  helpText("Users can automatically extract the common nodes between any selected pair of networks. Once two networks have been selected, a Venn diagram will be generated showing the node overlap. The common nodes are also reported in Table view."),
                  br(),
                  strong("Intersecting Network (2D)"),
                  helpText("This Tab is used to visualize and extract the intersecting network between any two selected networks. The network is fully interactive and can be exported as tab delimited file. "),
                  br(),
                  tags$img(src = b64_10),
                  br(),
                  strong("Intersecting Network (3D)"),
                  helpText("This tab uses a 3D multilayered view to show the intersection network between any two selected networks. Normally, the two layers are placed onto two different layers and common nodes are marked yellow. The nodes which only belong to the first network are colored red and the nodes which belong to the second network only, blue. Users can use a 3-layer representation to isolate the common nodes on a third middle layer. Notably, users can adjust the network’s layout using the various layouts which were mentioned previously."),
                  br(),
                  tags$img(src = b64_11)
                  
                  )
            
        
        ) #tabsetPanel
        ),#tabpanel Help
        tabPanel(
            "People",
            icon = icon("users"),
            strong("Main developers:"),
            tags$ul(
                tags$li("Georgios Pavlopoulos - BSRC Alexander Fleming"),
                tags$li("Theodosios Theodosiou - University of Crete"),
                tags$li("Mikaela Koutrouli - BSRC Alexander Fleming")
            ),
            br(),
            helpText("Please cite:"),
            tags$a(href="https://pubmed.ncbi.nlm.nih.gov/28705239/", "NAP: The Network Analysis Profiler, a Web Tool for Easier Topological Analysis and Comparison of Medium-Scale Biological Networks."),
            helpText("PMID:28705239"),
            br(),
            helpText("Related literature:"),
            tags$a(href="https://pubmed.ncbi.nlm.nih.gov/32083072/", "A Guide to Conquer the Biological Network Era Using Graph Theory"),
            helpText("PMID:32083072"),
            br(),
            helpText("Related software:"),
            tags$ul(
              tags$li(tags$a(href="http://bib.fleming.gr:3838/NORMA/", "NORMA-The network makeup artist: a web tool for network annotation visualization")
              )),
            br(),
            tags$ul(
              tags$li(tags$a(href="https://cytoscape.org/", "Cytoscape"))),
            br(),
            br()
            
        )
    )
)
