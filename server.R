# By default, Shiny limits file uploads to 5MB per file. You can modify
# this limit by using the shiny.maxRequestSize option. For example,# adding options(shiny.maxRequestSize=30*1024^2) to the top of server.R
# would increase the limit to 30MB. From within R, inside the app
# source dir, start the app with: shiny::runApp(getwd()) From a shell,# start the app with: R -e 'shiny::runApp('~/appsourcedir')'

read_data <- function(datapath, type = c("txt"), header = T, sep = "\t", quote = "\"", weighted = F) ({
    dataset1 <- read.table(datapath, header = header, sep = sep, quote = quote)
    # if(weighted)
    if (ncol(dataset1) == 2) {
        dataset1$V3 <- 1
    } else if (ncol(dataset1) > 3) {
        dataset1 <- dataset1[, 1:3]
    } else if (ncol(dataset1) != 3) 
        return(NULL)
    
    colnames(dataset1) <- c("Source", "Target", "Weight")
    # else colnames(dataset1) <- c('Source', 'Target')
    return(dataset1)
})

convert_to_igraph <- function(dataset1) ({
    directed_tf <- FALSE
    weighted_tf <- FALSE
    if (attr(dataset1, "directed")) {
        directed_tf <- attr(dataset1, "directed")
    }
    if (attr(dataset1, "weighted")) {
      weighted_tf <- attr(dataset1, "weighted")
    }
    igraph <- graph.data.frame(dataset1, directed = directed_tf, vertices = NULL)
    if (attr(dataset1, which = "weighted"))
      E(igraph)$weight <- dataset1$Weight
    if (attr(dataset1, which = "bipartite")) 
        V(igraph)$type <- V(igraph)$name %in% dataset1[, 1]
    return(igraph)
      })

EmptyDataset <- function(columns) {
    dataset <- data.frame(V1 = integer())
    lapply(columns[-1], function(x) dataset[, x] <<- integer())
    colnames(dataset) <- columns
    return(dataset)
}

ui_options <- c(ui_table_font_sz = "80%") #html size

options(shiny.error = browser) #debugging

pairs2 <- function(x, labels, panel = points, ..., lower.panel = panel, upper.panel = panel, diag.panel = NULL, text.panel = textPanel, label.pos = 0.5 + 
                     has.diag/3, cex.labels = NULL, font.labels = 1, row1attop = TRUE, gap = 1) {
  textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x, y, txt, cex = cex, font = font)
  localAxis <- function(side, x, y, xpd, bg, col = NULL, main, oma, ...) {
    if (side%%2 == 1) 
      Axis(x, side = side, xpd = NA, ...) else Axis(y, side = side, xpd = NA, ...)
  }
  localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
  localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
  localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
  localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
  dots <- list(...)
  nmdots <- names(dots)
  if (!is.matrix(x)) {
    x <- as.data.frame(x)
    for (i in seq_along(names(x))) {
      if (is.factor(x[[i]]) || is.logical(x[[i]])) 
        x[[i]] <- as.numeric(x[[i]])
      if (!is.numeric(unclass(x[[i]]))) 
        stop("non-numeric argument to 'pairs'")
    }
  } else if (!is.numeric(x)) 
    stop("non-numeric argument to 'pairs'")
  panel <- match.fun(panel)
  if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
    lower.panel <- match.fun(lower.panel)
  if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
    upper.panel <- match.fun(upper.panel)
  if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
    diag.panel <- match.fun(diag.panel)
  if (row1attop) {
    tmp <- lower.panel
    lower.panel <- upper.panel
    upper.panel <- tmp
    tmp <- has.lower
    has.lower <- has.upper
    has.upper <- tmp
  }
  nc <- ncol(x)
  if (nc < 2) 
    stop("only one column in the argument to 'pairs'")
  has.labs <- TRUE
  if (missing(labels)) {
    labels <- colnames(x)
    if (is.null(labels)) 
      labels <- paste("var", 1L:nc)
  } else if (is.null(labels)) 
    has.labs <- FALSE
  oma <- if ("oma" %in% nmdots) 
    dots$oma else NULL
  main <- if ("main" %in% nmdots) 
    dots$main else NULL
  if (is.null(oma)) {
    oma <- c(4, 4, 4, 4)
    if (!is.null(main)) 
      oma[3L] <- 6
  }
  opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
  on.exit(par(opar))
  dev.hold()
  on.exit(dev.flush(), add = TRUE)
  for (i in if (row1attop) 
    1L:nc else nc:1L) for (j in 1L:nc) {
      localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, type = "n", ...)
      if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
        box()
        # edited here... if (i == 1 && (!(j%%2) || !has.upper || !has.lower))
        # localAxis(1 + 2 * row1attop, x[, j], x[, i], ...) draw x-axis
        if (i == nc) 
          localAxis(1, x[, j], x[, i], ...)
        # draw y-axis
        if (j == 1) 
          localAxis(2, x[, j], x[, i], ...)
        # if (j == nc && (i%%2 || !has.upper || !has.lower)) localAxis(4, x[, #
        # j], x[, i], ...)
        mfg <- par("mfg")
        if (i == j) {
          if (has.diag) 
            localDiagPanel(as.vector(x[, i]), ...)
          if (has.labs) {
            par(usr = c(0, 1, 0, 1))
            if (is.null(cex.labels)) {
              l.wid <- strwidth(labels, "user")
              cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
            }
            text.panel(0.5, label.pos, labels[i], cex = cex.labels, font = font.labels)
          }
        } else if (i < j) 
          localLowerPanel(as.vector(x[, j]), as.vector(x[, i]), ...) else localUpperPanel(as.vector(x[, j]), as.vector(x[, i]), ...)
        if (any(par("mfg") != mfg)) 
          stop("the 'panel' function made a new plot")
      } else par(new = FALSE)
    }
  if (!is.null(main)) {
    font.main <- if ("font.main" %in% nmdots) 
      dots$font.main else par("font.main")
    cex.main <- if ("cex.main" %in% nmdots) 
      dots$cex.main else par("cex.main")
    mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
  }
  invisible(NULL)
}

panel.regression <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), cex = 1, col.regres = "red", ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    if (all(is.finite(x) & is.finite(y))) 
        abline(stats::lm(y ~ x), col = col.regres, ...)
}

scatterplotmatrix <- function(R, lowerPanelRegression, method = c("pearson", "kendall", "spearman"), ...) {
    x = checkData(R, method = "matrix")
    if (missing(method)) 
        method = method[1]
    panel.cor <- function(x, y, digits = 2, prefix = "", use = "complete", method, cex.cor, ...) {
        usr <- par("usr")
        on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r <- cor(x, y, use = use, method = method)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste(prefix, txt, sep = "")
        if (missing(cex.cor)) 
            cex <- 0.4/strwidth(txt)
        test <- cor.test(x, y, method = method)
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
        text(0.5, 0.5, txt, cex = cex * (abs(r) + 0.01)/2)
        text(0.8, 0.8, Signif, cex = cex * (abs(r) + 0.01)/2, col = 2)
    }
    # f <- function(t) { dnorm(t, mean = mean(x), sd = sd.xts(x)) }
    hist.panel = function(x, ...) {
        par(new = TRUE)
        hist(x, col = "#9dd4f2", border = "#3187b7", probability = TRUE, axes = FALSE, main = "", breaks = "FD")
        # lines(density(x, na.rm = TRUE), col = 'red', lwd = 1)
    }
    if (lowerPanelRegression) 
        lowerPanel <- panel.regression else lowerPanel <- points
    
    pairs2(x, gap = 0, lower.panel = lowerPanel, upper.panel = panel.cor, method = method, bg = "#c4e5f7", pch = 21, ...)
}


shinyServer(function(input, output, session) {
    reactiveVars <- reactiveValues()
    reactiveVars$StoredNetworks <- data.frame(id = character(), name = character(), stringsAsFactors = F)
    reactiveVars$SelectedStoredNetworksIds <- c()
    reactiveVars$layoutCoord <- NULL
    reactiveVars$plotData <- NULL
    reactiveVars$last_intersection_data <- NULL

    session$onSessionEnded(function() {
        snets <- isolate(StoredNets())
        if (nrow(snets) > 0) {
            unlink(c("*.rda", "*.zip"))
        }
    })
    
############ Read the files ################
loadNetworkFromFile <- function() {
    dataset1 <- NULL
    switch(input$uiLoadGraphOptionsInput, oF = {
        if (!is.null(input$file1)) {
            dataset1 <- read_data(input$file1$datapath)
        }
    }, oR_Barbasi = {
        dataset1 <- data.frame(get.edgelist(barabasi.game(n = as.integer(input$oR_selected_size), power = 1, directed = F)))
    }, oR_Erdos_Renyi = {
        n <- as.integer(input$oR_selected_size)
        dataset1 <- data.frame(get.edgelist(erdos.renyi.game(n = n, p.or.m = as.integer(n * log10(n)), type = "gnm", directed = F)))
    }, oR_Watts_Strogatz = {
        n <- as.integer(input$oR_selected_size)
        dataset1 <- data.frame(get.edgelist(sample_smallworld(1, n, 2, 0.05)))
    }, oR_Bipartite = {
        n <- as.integer(input$oR_selected_size)
        v <- sample(c(F, T), n, T)
        v1 <- sample(which(v == F))
        v2 <- sample(which(v == T))
        e <- unlist(mapply(c, v1, v2, SIMPLIFY = T))
        dataset1 <- data.frame(get.edgelist(make_bipartite_graph(v, e, directed = F)))
    })
        if (input$uiLoadGraphOptionsInput != "oF" && !is.null(dataset1)) {
            # if(input$weighted1) {
            if (!is.null(dataset1)) 
                dataset1$X3 <- sample(1:10, nrow(dataset1), replace = T)
            colnames(dataset1) <- c("Source", "Target", "Weight")
            # } else { colnames(dataset1) <- c('Source', 'Target') }
        }
        return(dataset1)
    }
    
    random_igraph_sizes <- c(`50` = 50, `500` = 500, `1000` = 1000)
    
    output$uiLoadGraphOptionsOutput <- renderUI({
        if (is.null(input$uiLoadGraphOptionsInput)) 
            return()
        
        # Depending on input$input_type, we'll generate a different UI
        # component and send it to the client.
        if (input$uiLoadGraphOptionsInput == "oF") {
            wellPanel(fileInput("file1", "1: Choose file to upload", 
                                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")), 
                      checkboxInput(inputId = "directed1", "Directed", value = FALSE), 
                      checkboxInput("bipartite1", "Bipartite", FALSE),
                      checkboxInput(inputId = "weighted1", "Weighted", value = FALSE))
        } else {
            div(list(div(wellPanel(checkboxInput(inputId = "directed1", label = "Directed", value = F)), class = "col-md-6"), div(wellPanel(checkboxInput(inputId = "weighted1", label = "Weighted", value = F)), class = "col-md-6"), div(wellPanel(radioButtons("oR_selected_size", label = "Number of nodes", choices = random_igraph_sizes)), class = "col-md-6")), class = "row")
        }
    })
    
    StoredNets <- reactive({
        return(reactiveVars$StoredNetworks)
    })
    
    SelectedStoredNets <- function() {
        if (length(reactiveVars$SelectedStoredNetworksIds) > 0) 
            return(StoredNets()[which(reactiveVars$StoredNetworks$id %in% 
                reactiveVars$SelectedStoredNetworksIds), ]) else if (nrow(StoredNets()) == 0 || is.na(StoredNets()[1, ])) 
            return(NULL) else {
            updateCheckboxGroupInput(session, "storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices = getStoredNetsChoices(), selected = getStoredNetsChoices()[1])
            return(StoredNets()[1, ])
        }
    }
    
    
    StoredNetsEmpty <- function() {
        return(nrow(reactiveVars$StoredNetworks) == 0)
    }
    
    getDatasetName <- function(id) {
        sn <- StoredNets()
        idi <- which(sn$id == id)
        if (is.null(sn) || nrow(sn) == 0 || length(idi) == 0) 
            return(NULL)
        return(sn[idi, ]$name)
    }
    
    fetchDataset <- function(nid) {
        retVal <- NULL
        if (length(nid) > 0) {
            retVal <- readRDS(paste0(nid, ".rda"))
            attr(retVal, "id") <- nid
        }
        return(retVal)
    }
    
    fetchFirstSelectedStoredDataset <- reactive({
        ssn <- SelectedStoredNets()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            return(fetchDataset(ssn[1, ]$id))
        } else {
            return(NULL)
        }
    })
    
    fetchFirstSelectedStoredIgraph <- function() {
        dataset <- fetchFirstSelectedStoredDataset()
        if (is.null(dataset)) 
            return(NULL) else return(convert_to_igraph(dataset))
    }
    
    fetchAllSelectedStoredDataset <- function() {
        ssn <- SelectedStoredNets()
        ids <- c()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            ret <- list()
            for (i in 1:nrow(ssn)) {
                ret[[i]] <- fetchDataset(ssn[i, ]$id)
                ids <- c(ids, ssn[i, ]$id)
            }
            names(ret) <- ids
            return(ret)
        } else {
            return(NULL)
        }
    }
    
    fetchMaxNSelectedStoredDataset <- function(N) {
        ssn <- SelectedStoredNets()
        if (!is.null(ssn) && nrow(ssn) > 0) {
            ret <- list()
            for (i in 1:nrow(ssn)) {
                if (i > N) 
                  break
                ret[[i]] <- fetchDataset(ssn[i, ]$id)
            }
            return(ret)
        } else {
            return(NULL)
        }
    }
    
    fetchMaxTwoSelectedStoredIgraphs <- function() {
        datasets <- fetchMaxNSelectedStoredDataset(2)
        if (is.null(datasets) || length(datasets) == 0) 
            return(NULL) else {
            ret <- list()
            for (i in length(datasets)) ret[[i]] <- convert_to_igraph(datasets[[i]])
            return(ret)
        }
    }
    
    doAddNetwork <- observeEvent(input$btnAddNetwork, {
        dataset <- loadNetworkFromFile()
        if (!is.null(dataset)) {
            nid <- UUIDgenerate(T)      #time-base UUID is generated
            nn <- input$networkName 
            cnt <- 1                    #count
            while (nn %in% reactiveVars$StoredNetworks$name) {  #reactiveVars: represents a single reactive variable. 
                cnt <- cnt + 1
                nn <- paste(input$networkName, cnt)      #paste: converts its arguments (via as.character) to character strings
            }
            df <- data.frame(id = nid, name = nn, stringsAsFactors = F)
            if (nrow(dataset) > 10000) {
                dataset <- dataset[1:10000, ]
            }
            attr(dataset, which = "directed") <- input$directed1
            attr(dataset, which = 'weighted') <- input$weighted1
            attr(dataset, which = "bipartite") <- input$bipartite1 || input$uiLoadGraphOptionsInput == "oR_Bipartite"
            reactiveVars$StoredNetworks <- rbind(reactiveVars$StoredNetworks, df)
            saveRDS(dataset, paste0(nid, ".rda"))
            if (length(reactiveVars$SelectedStoredNetworksIds) == 0) {
                reactiveVars$SelectedStoredNetworksIds <- c(nid)
            }
        } else createAlert(session, "tabUploadSideAlert", "fileUploadAlert", title = "ERROR !", style = "danger", content = paste0("An error occurred while trying to read your file. Please make sure that it is formatted according to the requirements."), append = FALSE)
    })
    
    doRemNetwork <- observeEvent(input$btnRemoveNetworks, {
        if (!is.null(input$availableNetworks)) {
            reactiveVars$StoredNetworks <- reactiveVars$StoredNetworks[-which(reactiveVars$StoredNetworks$id %in% 
                input$availableNetworks), ]
            nr <- nrow(reactiveVars$StoredNetworks)
            if (nr > 0) 
                reactiveVars$SelectedStoredNetworksIds <- reactiveVars$StoredNetworks[nr, ]$id
        }
    })
    
    getStoredNetsChoices <- function() {
        snets <- StoredNets()
        if (nrow(snets) == 0) 
            return(NULL)
        choices <- snets$id
        names(choices) <- snets$name
        return(choices)
    }
    
    # outputOptions(output, 'availableNetworks', suspendWhenHidden=FALSE)
    
    output$uiStoredGraphsOutputRadio <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(list(br(), wellPanel(h4("Available networks"), checkboxGroupInput("availableNetworks", label = "", choices = choices)), div(div(actionButton("btnRemoveNetworks", "REMOVE", icon = icon("minus")), class = "col-md-5 centerBlock"), class = "row text-center")))
    })
    
    uploadTabSetSelectedNetwork <- observeEvent(input$storedGraphsOutputSelectUpload, {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectUpload)
        }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectUpload <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectUpload", "Selected network", choices))
    })
    
    # topologyTabSetSelectedNetwork <-
    # observeEvent(input$storedGraphsOutputSelectTopolopgy, {
    # reactiveVars$SelectedStoredNetworksIds<-c(input$storedGraphsOutputSelectTopolopgy)
    # }, ignoreNULL = FALSE)
    
    topologyTableViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputSelectTopolopgy
        input$statisticsMethodsMainTabsetPanel
    }, {
        if (input$statisticsMethodsMainTabsetPanel == "tableView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            1 && reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputSelectTopolopgy)) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectTopolopgy)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectTopolopgy <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectTopolopgy", "Selected network", choices))
    })
    
    topologyPlotViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputMultipleSelectTopolopgy
        input$statisticsMethodsMainTabsetPanel
    }, {
        if (input$statisticsMethodsMainTabsetPanel == "plotView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            length(input$storedGraphsOutputMultipleSelectTopolopgy) && 
            all(reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputMultipleSelectTopolopgy))) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputMultipleSelectTopolopgy)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputMultipleSelectTopolopgy <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(checkboxGroupInput("storedGraphsOutputMultipleSelectTopolopgy", "Selected network(s)", choices))
    })
    
    ##########Clustering############	
    # clusteringTableViewSetSelectedNetwork <- observeEvent({input$storedGraphsOutputSelectClustering; input$clusteringMethodsMainTabsetPanel}, {
    #   if(input$clusteringMethodsMainTabsetPanel == "tableView" && ! (length(reactiveVars$SelectedStoredNetworksIds) == 1 && reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputSelectClustering)){
    #     reactiveVars$SelectedStoredNetworksIds<-c(input$storedGraphsOutputSelectClustering)
    #   }
    # }, ignoreNULL = FALSE)
    
    # output$uiStoredGraphsOutputSelectClustering <- renderUI({
    #   input$btnAddNetwork
    #   input$btnRemoveNetworks
    #   choices<-getStoredNetsChoices()
    #   if(is.null(choices)) return()
    #   return(selectInput("storedGraphsOutputSelectClustering", "Selected network", choices ))
    # })
    
    # clusteringPlotViewSetSelectedNetwork <- observeEvent({input$storedGraphsOutputMultipleSelectClustering; input$statisticsMethodsMainTabsetPanel}, {
    #   if(input$clusteringMethodsMainTabsetPanel == "plotView" && !(length(reactiveVars$SelectedStoredNetworksIds) == length(input$storedGraphsOutputMultipleSelectClustering) && all(reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputMultipleSelectClustering))){
    #     reactiveVars$SelectedStoredNetworksIds<-c(input$storedGraphsOutputMultipleSelectClustering)
    #   }
    # }, ignoreNULL = FALSE)
    
    # output$uiStoredGraphsOutputMultipleSelectClustering <- renderUI({
    #   input$btnAddNetwork
    #   input$btnRemoveNetworks
    #   choices<-getStoredNetsChoices()
    #   if(is.null(choices)) return()
    #   return(checkboxGroupInput("storedGraphsOutputMultipleSelectClustering", "Selected network(s)", choices ))
    # })
    
    rankingTableViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputSelectRanking
        input$rankingMethodsMainTabsetPanel
    }, {
        if (input$rankingMethodsMainTabsetPanel == "tableView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            1 && reactiveVars$SelectedStoredNetworksIds == input$storedGraphsOutputSelectRanking)) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectRanking)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectRanking <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectRanking", "Selected network", choices))
    })
    
    rankingPlotViewSetSelectedNetwork <- observeEvent({
        input$storedGraphsOutputMultipleSelectRanking
        input$rankingMethodsMainTabsetPanel
    }, {
        if (input$rankingMethodsMainTabsetPanel == "plotView" && !(length(reactiveVars$SelectedStoredNetworksIds) == 
            length(input$storedGraphsOutputMultipleSelectRanking) && all(reactiveVars$SelectedStoredNetworksIds == 
            input$storedGraphsOutputMultipleSelectRanking))) {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputMultipleSelectRanking)
        }
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputMultipleSelectRanking <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputMultipleSelectRanking", "Selected network", choices))
    })
    
    layoutPlotViewSetSelectedNetwork <- observeEvent(input$storedGraphsOutputSelectLayouts, {
            reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputSelectLayouts)
        }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputSelectLayouts <- renderUI({
        input$btnAddNetwork
        input$btnRemoveNetworks
        choices <- getStoredNetsChoices()
        if (is.null(choices)) 
            return()
        return(selectInput("storedGraphsOutputSelectLayouts", "Selected network", choices))
    })
    
    intersectionTabSetSelectedNetworks <- observeEvent({
      input$storedGraphsOutputIntersectionSelect1
      input$storedGraphsOutputIntersectionSelect2
      }, {
      reactiveVars$SelectedStoredNetworksIds <- c(input$storedGraphsOutputIntersectionSelect1, input$storedGraphsOutputIntersectionSelect2)
    }, ignoreNULL = FALSE)
    
    output$uiStoredGraphsOutputIntersectionSelect1 <- renderUI({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices()
      if (is.null(choices)) 
        return()
      return(selectInput("storedGraphsOutputIntersectionSelect1", "Selected first network", choices))
    })
    
    output$uiStoredGraphsOutputIntersectionSelect2 <- renderUI({
      input$btnAddNetwork
      input$btnRemoveNetworks
      choices <- getStoredNetsChoices()
      if (is.null(choices)) 
        return()
      return(selectInput("storedGraphsOutputIntersectionSelect2", "Selected second network", choices))
    })
    
    
    
    ######## Basic Info ################
    output$info_vertices1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of nodes in", igraphName, ":", vcount(igraph))
    })
    output$info_edges1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of intercations in", igraphName, ":", ecount(igraph))
    })
    
    
    ############################################## 
    
    output$vertices1_label <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of vertices in", igraphName, ":", vcount(igraph))
    })
    
    # Print the vertices
    output$vertices1 <- renderPrint({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return(invisible(""))
        cat(sort(V(igraph)$name, decreasing = FALSE), sep = "\t")
    })
    
    output$edges1_label <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        igraphName <- SelectedStoredNets()$name
        paste("Number of edges in", igraphName, ":", ecount(igraph))
    })
    
    output$edges1 <- renderPrint({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return(invisible(""))
        if (as.logical(is.directed(igraph))) {
            edgesym <- "<U+2794>"
        } else {
            edgesym <- "<U+2015>"
        }
        
        cat(gsub("\\|", edgesym, attr(E(igraph), "vnames")), sep = "\t")
        # get.edgelist(igraph) get.data.frame(igraph, what=c( 'edges'))
    })
    
    
    
    ################################################# 
    output$diameter1 <- renderText({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        diameter <- diameter(igraph)
        paste("Diameter of Graph 1:", diameter)
        # unique_vertices <- length(vertices)
    })
    
    
    ################################################ https://rstudio.github.io/DT/shiny.html
    ################################################ http://rstudio.github.io/DT/functions.html
    ################################################ https://cran.r-project.org/web/packages/DT/DT.pdf Print the data in
    ################################################ Table format ####################
    
    output$datasettab1 <- DT::renderDataTable({
        dataset <- fetchFirstSelectedStoredDataset()
        if (is.null(dataset)) 
            dataset <- EmptyDataset(c("Source", "Target", "Weight"))
        # if (nrow(dataset) == 0 || attr(dataset, 'weighted'))
        if (nrow(dataset) == 0 || attr(dataset, 'weighted')){
        return(datatable(dataset, rownames = FALSE) %>% formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Weight"))
        } else return(datatable(dataset, rownames = FALSE, options=list(deferRender = TRUE,
                                                                        scrollY = 500,
                                                                        scroller = TRUE,pageLength = 500,columnDefs = list(list(visible=FALSE, targets=c(2))))) %>% formatStyle(colnames(dataset), 
                                                                           fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Weight"))

    })
    
    # output$datasettab2 <- DT::renderDataTable({
    #     dataset <- Dataset2()
    #     if (is.null(dataset)) 
    #         dataset <- EmptyDataset(c("Source", "Target", "Weight"))
    #     # if (attr(dataset, 'weighted'))
    #     return(datatable(dataset, rownames = FALSE) %>% formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Weight"))
    #     # else return( datatable(dataset, rownames = FALSE) %>%
    #     # formatStyle(colnames(dataset), fontSize =
    #     # ui_options['ui_table_font_sz']) )
    # })
    ############################################### 
    
    ############ Plot the graphs ########### output$tabVizIgraphForce <-
    ############ renderSimpleNetwork({
    output$tabVizIgraphForce <- renderVisNetwork({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
        set.seed(123)
        igraph_visn <- toVisNetworkData(igraph)
        igraph_visn$edges$value <- E(igraph)$weight
        
        if(is.directed(igraph)){
        visNetwork(igraph_visn$nodes, igraph_visn$edges) %>%
          visNodes(size = 25, shape = "ellipse") %>%
          visEdges(arrows = 'to', smooth =T) %>%
          visOptions(highlightNearest = TRUE,
                     nodesIdSelection = TRUE) %>%
            visPhysics(enabled = F) %>%
          visInteraction(
            keyboard = TRUE,
            navigationButtons = TRUE,
            zoomView = TRUE,
            multiselect = TRUE,
            dragView = TRUE
          )
        }else{
          visNetwork(igraph_visn$nodes, igraph_visn$edges) %>%
            visNodes(size = 25, shape = "ellipse") %>%
            visOptions(highlightNearest = TRUE,
                       nodesIdSelection = TRUE) %>%
            visPhysics(enabled = F) %>%
            visInteraction(
              keyboard = TRUE,
              navigationButtons = TRUE,
              zoomView = TRUE,
              multiselect = TRUE,
              dragView = TRUE
            )
        }
    })
    
    rankingMethodsScatterPlot <- renderPlot({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return()
    })
    
    ################ Statistics ########
    
    stat_dataset <- function(dataset, datasetName) {
        res <- tryCatch({
            columns <- c("Statistics", paste0("Value for ", datasetName))
            if (is.null(dataset)) {
                dataset <- EmptyDataset(columns)
            } else {
                igraph <- convert_to_igraph(dataset)
                if (length(input$statistics) == 0) {
                  dataset <- EmptyDataset(columns)
                } else {
                  dataset <- netstats(igraph, input$statistics)
                  colnames(dataset) <- columns
                }
            }
            return(dataset)
        }, warning = function(w) {
            
        }, error = function(e) {
            cat(paste0("ERROR while executing stat_dataset: ", e, "\n"))
            return(NULL)
        }, finally = {
            
        })
        return(res)
    }
    output$statres <- DT::renderDataTable({
        dset <- fetchFirstSelectedStoredDataset()
        stat_dataset(dset, getDatasetName(attr(dset, "id")))
    }, options = list(paging = FALSE, dom = "t", rowCallback = JS("function(row, data) {", "var num = parseFloat(data[2]).toFixed(2);", "if(isNaN(num)){num = data[2];}", "$('td:eq(2)', row).html(num);", "}")))
    
    ########### Output Clustering#########
    clustering_dataset <- function(dataset){
      res <- tryCatch({
        columns<-c("Node","Cluster No.")
        if(is.null(dataset)){
          dataset<-EmptyDataset(columns)
        }else{
          igraph<-convert_to_igraph(dataset)
          if(length(input$statistics)==0){
            dataset<-EmptyDataset(columns)
          }else{
            adjacency<-as_adjacency_matrix(igraph, type ="both")
            loopsTF = F
            
            if( is.bipartite(igraph) | is.directed(igraph)){
              loopsTF = T
            }
            
            inflation_value = input$inflation
            mcl_res <- mcl(x=adjacency, max.iter = 1000, addLoops = loopsTF, inflation = inflation_value, allow1 = T)
            dataset <- data.frame(as.vector(V(igraph)),mcl_res$Cluster)
            colnames(dataset)<-columns
          }
        }
        return(dataset)
      }, warning = function(w){
      }, error = function(e){
        cat(paste0("ERROR while executing clustering_dataset: ",e, "\n"))
        return(NULL)
      }, finally = {
      })
      return(res)
    }
    # output$clusteringres <- DT::renderDataTable({
    #   clustering_dataset(fetchFirstSelectedStoredDataset())
    # }, options = list(paging = FALSE, dom = 't', rowCallback= JS(
    #   "function(row, data) {",
    #   "var num = parseFloat(data[2]).toFixed(2);",
    #   "if(isNaN(num)){num = data[2];}",
    #   "$('td:eq(2)', row).html(num);",
    #   "}"
    # )))
    
    
    ################ Ranking ###########
    
    rank_dataset <- function(dataset, rankingMethod, nodeRanking = T) {
        res <- tryCatch({
            if (nodeRanking) {
                columns <- c("Node", "Rank")
            } else {
                columns <- c("Edge", "Rank")
            }
            if (is.null(dataset) || length(rankingMethod) == 0) {
                dataset <- EmptyDataset(columns)
            } else {
                igraph <- convert_to_igraph(dataset)
                if (length(rankingMethod) == 0) {
                  dataset <- EmptyDataset(columns)
                } else {
                  dataset <- ranking(igraph, rankingMethod, nodeRanking)
                  colnames(dataset) <- columns
                }
            }
            return(dataset[order(-dataset$Rank), ])
        }, warning = function(w) {
            
        }, error = function(e) {
            cat(paste0("ERROR while executing rank_dataset: ", e, "\n"))
            return(NULL)
        }, finally = {
            
        })
        return(res)
    }
    
                           
    output$rankeddatasettab1 <- DT::renderDataTable({
        node_ranking_mode <- input$rankingTableViewTabsetPanel == "Node ranking"
        if (node_ranking_mode) 
            ranking_method <- input$vrankingTableView else ranking_method <- input$erankingTableView
        dataset <- fetchFirstSelectedStoredDataset()
        if (length(ranking_method) == 1 && !(is.null(dataset) || nrow(dataset) == 
            0)) {
            dataset <- rank_dataset(dataset, ranking_method, node_ranking_mode)
            ranking_method_name <- unlist(strsplit(ranking_method, "\t"))
            ranking_method_name <- ranking_method_name[length(ranking_method_name)]
            if (is.null(dataset) || nrow(dataset) == 0) 
                createAlert(session, "tabRankingMainAlert", "rankingAlert", 
                            title = "ERROR !", style = "danger", 
                            content = paste0("Our apologies, an error occured while calculating the '", ranking_method_name, "' ."), 
                            append = FALSE) else return(datatable(dataset, rownames = FALSE) %>% formatStyle(colnames(dataset), fontSize = ui_options["ui_table_font_sz"]) %>% formatRound("Rank")) 
        }
    })
    
    # doRefreshPalette <- observeEvent(input$btnRefreshPalette, {
    #     ncolors <- length(colors)
    #     colors <<- randomColor(ncolors)
    # })
    
    output$statisticsMethodsBarPlot <- renderPlotly({
        # input$btnRefreshPalette
        datasets <- fetchAllSelectedStoredDataset()
        if (length(datasets) == 0) 
            return()
        stat_res <- NULL
        for (dataset_i in 1:length(datasets)) {
            dataset <- datasets[[dataset_i]]
            datasetName <- getDatasetName(names(datasets)[dataset_i])
            dataset_stat_res <- stat_dataset(dataset, "")
            if (is.null(dataset_stat_res) || nrow(dataset_stat_res) == 
                0) 
                next
            if (is.null(stat_res)) {
                stat_res <- dataset_stat_res
                stat_res$network <- datasetName
            } else {
                dataset_stat_res$network <- datasetName
                stat_res <- rbind(stat_res, dataset_stat_res)
            }
        }
        if (is.null(stat_res)) 
            return()
        colnames(stat_res) <- c("statistic", "value", "network")
        stat_res$value <- as.numeric(as.character(stat_res$value))
        stat_res$network <- factor(stat_res$network)
        ncolors <- length(levels(stat_res$network))
        if (length(colors) < ncolors) 
            colors <<- randomColor(ncolors)
        plot.settings <- list(superpose.polygon = list(col = colors[1:ncolors], border = "transparent"), 
                              strip.border = list(col = "black"))
        
        ### plotly ###
        rows_of_plots<- 0
        plot_list = vector("list",length(unique(stat_res$statistic)))
        for(i in 1:nrow(stat_res)){
          data_i <- paste("data", i, sep = "")
          fig_i <- paste("fig", i, sep = "")
          data_i <- filter(stat_res, stat_res$statistic == stat_res$statistic[i])
         
          if(i==1:length(unique(data_i$network))){
            fig_i<- plot_ly(x = data_i$statistic,
                            y = data_i$value,
                            name = data_i$network,
                            color = data_i$network,
                            type = "bar", showlegend = T) 
            fig_i <- fig_i %>% layout(legend = list(orientation = 'h'))
            
          }else {
            fig_i<- plot_ly(x = data_i$statistic,
                            y = data_i$value,
                            name = data_i$network,
                            color = data_i$network,
                            type = "bar", showlegend = F)
          }
          
          
          
            plot_list[[i]] = fig_i
            
          if(i == length(unique(stat_res$statistic))){
            break
          }
        }
        if(any(length(plot_list)==(1:3))==T){
          rows_of_plots<- 1
        } 
        if(any(length(plot_list)==(4:6))==T){
          rows_of_plots<- 2
        } 
        if(any(length(plot_list)==(7:9))==T){
          rows_of_plots<- 3
        } 
        if(any(length(plot_list)==(10:12))==T){
          rows_of_plots<- 4
        } 
        if(any(length(plot_list)==(13:15))==T){
          rows_of_plots<- 5
        } 
        if(any(length(plot_list)==(15:18))==T){
          rows_of_plots<- 6
        } 
        if(any(length(plot_list)==(19))==T){
          rows_of_plots<- 7
        }
        withProgress(min = 0, max = 1, {
          incProgress(message = "Processing data into plot",
                      detail = "This may take a while...",
                      amount = .5)
        subplot(plot_list, nrows = rows_of_plots, which_layout = 1)
        })

    })
    
    output$statisticsMethodsPlotRender <- renderUI({
      plotlyOutput("statisticsMethodsBarPlot", height = paste0(as.integer(600 + 
            480 * (input$statisticsPlotPercentMagnify/100)), "px"))
    })
    
    doStatSelectAll <- observeEvent(input$btnStatSelectAll, {
        updateCheckboxGroupInput(session, "statistics", selected = statistics)
    })
    
    doStatSelectNone <- observeEvent(input$btnStatSelectNone, {
        updateCheckboxGroupInput(session, "statistics", selected = character(0))
    })
    
    
    # output$rankingMethodsScatterPlot <- renderPlotly({
    output$rankingMethodsScatterPlot <- renderPlot({
        node_ranking_mode <- input$rankingPlotViewTabsetPanel == "Node ranking"
        ranking_methods <- NULL
        p <- NULL
        reactiveVars$plotData <- NULL
        if (node_ranking_mode) 
            ranking_methods <- input$vrankingPlotView else ranking_methods <- input$erankingPlotView
        if (!is.null(ranking_methods) && length(ranking_methods) > 1) {
            datasets <- fetchMaxNSelectedStoredDataset(2)
            if (!is.null(datasets) && length(datasets) > 0) {
                ranking_methods_names <- unlist(strsplit(ranking_methods, "\t"))[seq(1, length(ranking_methods) * 2, 2)]
                if (length(datasets) > 1) {
                  # p<-plot(length(datasets))
                } else {
                  dataset <- rank_dataset(datasets[[1]], ranking_methods[1], node_ranking_mode)
                  if (is.null(dataset) || nrow(dataset) == 0) 
                    createAlert(session, "tabRankingMainAlert", "rankingAlert", title = "ERROR !", style = "danger", content = paste0("Our apologies, an error occured while calculating the '", ranking_methods_names[1], "' ."), append = FALSE)
                  colnames(dataset)[length(colnames(dataset))] <- ranking_methods_names[1]
                  for (i in 2:length(ranking_methods)) {
                    dataset_i <- rank_dataset(datasets[[1]], ranking_methods[i], node_ranking_mode)
                    if (is.null(dataset_i) || nrow(dataset_i) == 0) 
                      createAlert(session, "tabRankingMainAlert", "rankingAlert", title = "ERROR !", style = "danger", content = paste0("Our apologies, an error occured while calculating the '", ranking_methods_names[i], "' ."), append = FALSE) else {
                      dataset <- cbind(dataset, dataset_i$Rank)
                      colnames(dataset)[length(colnames(dataset))] <- ranking_methods_names[i]
                    }
                  }
                  p <- scatterplotmatrix(dataset[, -1], input$IsRankingRegressionEnabled)
                  reactiveVars$plotData<-dataset
                }
            }
        }
        if (!is.null(p)) {
            # pp <- ggplotly(p) pp
        } else {
          return(NULL)
        }
    })
    
    output$rankingMethodsScatterPlotRender <- renderUI({
        plotOutput("rankingMethodsScatterPlot", height = paste0(as.integer(480 + 
            480 * (input$scatterPlotMatrixPercentMagnify/100)), "px"))
    })
    
    ################ Testing Ranking Plots #############
    
    output$rankingMethodsScatterPlot1 <- renderPlot({
        node_ranking_mode <- input$rankingPlotViewTabsetPanel == "Node ranking"
        ranking_methods <- NULL
        p <- NULL
        reactiveVars$plotData <- NULL
        if (node_ranking_mode) 
            ranking_methods <- input$vrankingPlotView else ranking_methods <- input$erankingPlotView
        if (!is.null(ranking_methods) && length(ranking_methods) > 0) {
            datasets <- fetchMaxNSelectedStoredDataset(2)
            if (!is.null(datasets) && length(datasets) > 0) {
                ranking_methods_names <- unlist(strsplit(ranking_methods, "\t"))[seq(1, length(ranking_methods) * 2, 2)]
                if (length(datasets) > 1) {
                  # p<-plot(length(datasets))
                } else {
                  dataset <- rank_dataset(datasets[[1]], ranking_methods[1], node_ranking_mode)
                  if (is.null(dataset) || nrow(dataset) == 0) 
                    createAlert(session, "tabRankingMainAlert", "rankingAlert", title = "ERROR !", style = "danger", content = paste0("Our apologies, an error occured while calculating the '", ranking_methods_names[1], "' ."), append = FALSE)
                  colnames(dataset)[length(colnames(dataset))] <- ranking_methods_names[1]
                  if (length(ranking_methods) > 1) {
                    for (i in 2:length(ranking_methods)) {
                      dataset_i <- rank_dataset(datasets[[1]], ranking_methods[i], node_ranking_mode)
                      if (is.null(dataset_i) || nrow(dataset_i) == 0) {
                        # createAlert(session, 'tabRankingMainAlert', 'rankingAlert', title =
                        # 'ERROR 2!', style = 'danger', content = paste0('Our apologies, an
                        # error occured while calculating the '',ranking_methods_names[i],''
                        # .'), append = FALSE)
                      } else {
                        dataset <- cbind(dataset, dataset_i$Rank)
                        colnames(dataset)[length(colnames(dataset))] <- ranking_methods_names[i]
                      }
                    }
                  }
                  # p<-scatterplotmatrix(dataset[,-1]) print(str(dataset[,-1]))
                  
                  
                  # 
                  
                }
                # else
                
            }
            p <- plotmatrix(dataset[, -1], ranking_methods_names[1])
            reactiveVars$plotData<-dataset
        }
        if (!is.null(p)) {
            # pp <- ggplotly(p) pp
        } else {
            return(NULL)
        }
    })
    
    plotmatrix <- function(x, method_names) {
        x <- checkData(x, method = "matrix")
        h <- hist(x, breaks = "FD", col = "#c4e5f7", border = "#3187b7", main = paste("Histogram for", method_names), xlab = method_names, axes = F)
        axis(2)
        lb <- length(h$breaks)
        if (lb%%2 == 0) 
            bStart <- 2 else bStart <- 1
        
        if (lb > 2 && lb < 30) 
            bStep <- 2 else if (lb >= 30) {
            bStep <- as.integer(lb/30)
            bStart <- as.integer(((lb/bStep) - as.integer(lb/bStep)) * 
                bStep)
        } else {
            bStep <- 1
            bStart <- 1
        }
        axis(1, at = h$breaks[seq(bStart, lb, bStep)])
        # lines(density(x, na.rm = TRUE), col = 'red', lwd = 1) rug(x)
    }
    
    output$rankingMethodsScatterPlotRender1 <- renderUI({
        plotOutput("rankingMethodsScatterPlot1", height = paste0(as.integer(480 + 
            480 * (input$scatterPlotMatrixPercentMagnify1/100)), "px"))
    })
    
    observe({
      shinyjs::toggleState("dwnld_ranking_plot", !is.null(reactiveVars$plotData))
    })    
    
    output$dwnld_ranking_plot <- downloadHandler(filename = function() {
      igraph <- fetchFirstSelectedStoredIgraph()
      igraphName <- SelectedStoredNets()$name
      paste0(igraphName,"_ranking_data.txt")
    }, content = function(filename) {
      write.table(x = reactiveVars$plotData, file = filename, row.names = F,sep = "\t", quote = F)
    })

    ###################### End Testing Ranking #################
    
    ########## Obsolete Ranking method ##################
    output$rankingMethodsPowerLawScatter <- renderPlot({
        igraph <- fetchFirstSelectedStoredIgraph()
        if (is.null(igraph)) 
            return(NULL)
        d = degree(igraph, mode = "all")
        dd = degree.distribution(igraph, mode = "all", cumulative = FALSE)
        degree = 1:max(d)
        probability = dd[-1]
        # delete blank values
        nonzero.position = which(probability != 0)
        probability = probability[nonzero.position]
        degree = degree[nonzero.position]
        reg = lm(log(probability) ~ log(degree))
        cozf = coef(reg)
        power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
        alpha = -cozf[[2]]
        R.square = summary(reg)$r.squared
        
        plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", col = 1, main = "Degree Distribution")
        curve(power.law.fit, col = "red", add = T, n = length(d))
        mtext(substitute(alpha == a, list(a = round(alpha, 3))), side = 3, line = 0, adj = 1, cex = 1)
        mtext(substitute(R^2 == r, list(r = round(R.square, 3))), side = 3, line = 1, adj = 1, cex = 1)
    })
    
    ########### Layouts ###################
    output$ui_combobox_layouts_netnames <- renderUI({
        networks = SelectedStoredNets()$name
    })
    
    output$plot_layout <- renderVisNetwork({
      igraph <- fetchFirstSelectedStoredIgraph()
      set.seed(123)
      
      if (is.null(igraph)) {
          reactiveVars$layoutCoord <- NULL
          return(NULL)
        }
        
        if (is.bipartite(igraph)) {
          reactiveVars$layoutCoord <- "layout_as_bipartite"
        } else {
            reactiveVars$layoutCoord <- switch(input$combobox_layouts, 
                                               circle = "layout.circle", 
                                               grid = "layout.grid", 
                                               sphere = "layout.sphere", 
                                               random = "layout.random", 
                                               fruchterman = "layout.fruchterman.reingold", 
                                               kamada = "layout.kamada.kawai", 
                                               lgl = "layout.lgl", 
                                               reingold = "layout.reingold.tilford", 
                                               grid = "layout.fruchterman.reingold.grid")}
      
      igraph_visn <- toVisNetworkData(igraph)
      igraph_visn$edges$value <- E(igraph)$weight
      
      if(is.directed(igraph)){
        visNetwork(igraph_visn$nodes, igraph_visn$edges) %>%
            visNodes(size = 25, shape = "ellipse") %>%
          visEdges(arrows = 'to', smooth =T) %>%
              visIgraphLayout(layout = reactiveVars$layoutCoord) %>%
              visOptions(highlightNearest = TRUE,
                         nodesIdSelection = TRUE) %>%
              visInteraction(
                keyboard = TRUE,
                navigationButtons = TRUE,
                zoomView = TRUE,
                multiselect = TRUE,
                dragView = TRUE
              )
      }else{
          visNetwork(igraph_visn$nodes, igraph_visn$edges) %>%
            visNodes(size = 25, shape = "ellipse") %>%
          visIgraphLayout(layout = reactiveVars$layoutCoord) %>%
            visOptions(highlightNearest = TRUE,
                       nodesIdSelection = TRUE) %>%
            visInteraction(
              keyboard = TRUE,
              navigationButtons = TRUE,
              zoomView = TRUE,
              multiselect = TRUE,
              dragView = TRUE
            )
        }
        
    })  # plot_layout
    
    observe({
        igraph <- fetchFirstSelectedStoredIgraph()
        shinyjs::toggleState("dwnld_layout", !is.null(igraph) && !is.null(reactiveVars$layoutCoord))
    })
    
    output$dwnld_layout <- downloadHandler(filename = function() {
      igraph <- fetchFirstSelectedStoredIgraph()
      igraphName <- SelectedStoredNets()$name
        if (is.bipartite(igraph)) 
            paste0(igraphName, "_layout-bipartite.txt")
        else
            paste0(igraphName, "_layout-", input$combobox_layouts, ".txt")
    }, content = function(filename) {
      igraph <- fetchFirstSelectedStoredIgraph()
      igraphName <- SelectedStoredNets()$name
      reactiveVars$layoutCoord <- switch(input$combobox_layouts, 
                                         circle = "Circle\tlayout.circle(igraph)", 
                                         grid = "Grid\tlayout.grid(igraph)",
                                         sphere = "Sphere\tlayout.sphere(igraph)",
                                         random = "Random\tlayout.random(igraph, dim=2)", 
                                         fruchterman = "Fructerman\tlayout.fruchterman.reingold(igraph, dim=2)", 
                                         kamada = "Kamada-Kawai\tlayout.kamada.kawai(igraph, dim=2)", 
                                         lgl = "Lgl\tlayout.lgl(igraph)", 
                                         reingold = "Reingold-Tilford\tlayout.reingold.tilford(igraph)")
      lay<- layout_choices(igraph, reactiveVars$layoutCoord)
      names_nodes<- names(V(igraph))
      nodes_coords<- data.frame("Node"= names_nodes, "Coord.x"= lay[,1], "Coord.y"=lay[,2])
        write.table(x = nodes_coords, file = filename, row.names = F, quote = F, sep="\t")
    })
    
    observe({
        igraph <- fetchFirstSelectedStoredIgraph()
        shinyjs::toggleState("dwnld_projections1", !is.null(igraph) && is.bipartite(igraph) && !is.null(reactiveVars$layoutCoord))
        shinyjs::toggleState("dwnld_projections2", !is.null(igraph) && is.bipartite(igraph) && !is.null(reactiveVars$layoutCoord))
        shinyjs::toggleState("combobox_layouts", !is.null(igraph) && !is.bipartite(igraph))
    })
    
    
    output$dwnld_projections1 <- downloadHandler(filename = function() {
        paste0(SelectedStoredNets()$name, "_BipartiteProjection1.txt")
    }, content = function(filename) {
        igraph <- fetchFirstSelectedStoredIgraph()
        V(igraph)$type <- bipartite_mapping(igraph)$type
        projection <- bipartite_projection(igraph)
        proj1 <- (projection[[2]])
        write.table(get.data.frame(proj1), file = filename, col.names = c("Source", "Target", "Weight"),row.names = F, quote = F, sep = "\t")
    })
    output$dwnld_projections2 <- downloadHandler(filename = function() {
        paste0(SelectedStoredNets()$name, "_BipartiteProjection2.txt")
    }, content = function(filename) {
        igraph <- fetchFirstSelectedStoredIgraph()
        V(igraph)$type <- bipartite_mapping(igraph)$type
        projection <- bipartite_projection(igraph)
        proj2 <- (projection[[1]])
        write.table(get.data.frame(proj2), file = filename, col.names = c("Source", "Target", "Weight"), row.names = F, quote = F, sep = "\t")
    })
    
    
  #  output$plot_cytoscape <- renderRcytoscapejs({
  #    network<-fetchFirstSelectedStoredDataset()
  #    if(is.null(network))
  #    {
  #      reactiveVars$layoutCoord <- NULL
  #      return(NULL)
  #    }
  #    title <- input$net_name1
  #    
  #    edgeList <- network[, c("Source","Target")]
  #    all_nodes <- factor(unlist(list(edgeList$Source,edgeList$Target)))
  #    
  #    nodes <- levels(all_nodes)
  #    #print(nodes[1:10])
  #    
  #    # To add more info for edges and nodes later
  #    edgeData <- edgeList
  #    # lowercase 'source' and 'target' labels
  #    names(edgeData) <- c("source","target")
  #    
  #    # nodeData must have 'id' and 'name' columns" and be a data frame
  #    id <- nodes
  #    name <- nodes
  #    nodeData <- data.frame(id,name)
  #    
  #    #print(nodeData)
  #    
  #    cyNetwork <- createCytoscapeJsNetwork(nodeData, edgeData, nodeColor = input$nodeCol, nodeShape = input$nodeShape, edgeColor = input$edgeCol, edgeSourceShape = input$sarrowShape, edgeTargetShape = input$tarrowShape)
  #    rcytoscapejs(nodeEntries=cyNetwork$nodes, edgeEntries=cyNetwork$edges, layout = input$layout)
  #  })
    
    # observe({
    #   if (input$btnRefreshPalette2 == 0)
    #     return()
    #   isolate({
    #     btnRefreshPalette2Clicked <<- T
    #   })
    # })

    
    # observe({
    #   shinyjs::toggleState("dwnld_intersection", !is.null(reactiveVars$last_intersection_data))
    # })    
    # 
    # output$dwnld_intersection <- downloadHandler(filename = function() {
    #   paste0("intersection_data.csv")
    # }, content = function(filename) {
    #   write.csv(x = reactiveVars$last_intersection_data[[7]], file = filename, row.names = F, quote = F)
    # })
    
    output$uiIntersectionVenn_Edges <- renderUI({
      input$storedGraphsOutputIntersectionSelect1
      input$storedGraphsOutputIntersectionSelect2
      # input$btnRefreshPalette2
      # if(!(exists("btnRefreshPalette2Clicked") && btnRefreshPalette2Clicked == T))
      # {
        reactiveVars$last_intersection_data <- NULL
        datasets <- fetchAllSelectedStoredDataset()
        if (length(datasets) < 2) 
          return()
        igraph1 <- convert_to_igraph(datasets[[1]])
        igraph1name <- getDatasetName(attr(datasets[[1]], "id"))
        igraph2 <- convert_to_igraph(datasets[[2]])
        igraph2name <- getDatasetName(attr(datasets[[2]], "id"))
        if(attr(datasets[[1]], "directed") != attr(datasets[[2]], "directed"))
        {
          createAlert(session, "tabIntersectionMainAlert", "cannotIntersectAlert", title = "WARNING !", style = "warning", content = paste0("Impossible to calculate the intersection between a directed and an undirected graph."), append = FALSE)
        }
        else
        {
          g_sim <- graph.intersection(igraph1, igraph2, byname = "auto", keep.all.vertices = FALSE)
          if(length(E(g_sim))==0){
            reactiveVars$last_intersection_data <- list(igraph1, igraph2, 0, 0, igraph1name, igraph2name, 0)
          }else{
          R <- as.matrix(get.adjacency(g_sim, type="both"))
          exportdf <- as_data_frame(R)
          colnames(exportdf)[c(3,4)] <- c(igraph1name, igraph2name)
          reactiveVars$last_intersection_data <- list(igraph1, igraph2, g_sim, R, igraph1name, igraph2name, exportdf)
          }
        }
      # }
      # else
      #   btnRefreshPalette2Clicked <<- F
      
      if(!is.null(reactiveVars$last_intersection_data))
      {
        colors<-randomColor(2)
        ecount1 = length(E(reactiveVars$last_intersection_data[[1]]))
        ecount2 = length(E(reactiveVars$last_intersection_data[[2]]))
        if(length(E(g_sim))==0){
          ecountX=0
        }else{
        ecountX = length(E(reactiveVars$last_intersection_data[[3]]))
        }
        category <- c(reactiveVars$last_intersection_data[[5]], reactiveVars$last_intersection_data[[6]])
        return(
          div(
            fluidRow(
              column(8,
                     div(
                       h3("Edge intersection"),
                       renderPlot(
                         draw.pairwise.venn(area1 = ecount1,
                                            area2 = ecount2,
                                            cross.area = ecountX,
                                            category = category,
                                            fill = colors,
                                            ext.text = TRUE,
                                            ext.percent = c(0.1,0.1,0.1),
                                            ext.length = 0.6,
                                            label.col = rep("gray10",3),
                                            lwd = 2,
                                            cex = 2,
                                            fontface = rep("bold",3),
                                            fontfamily = rep("sans",3),
                                            cat.cex = 1.5,
                                            cat.fontface = rep("plain",2),
                                            cat.fontfamily = rep("sans",2),
                                            cat.pos = c(0, 180),
                                            print.mode = c("percent","raw")
                         )
                       ),
                       style="text-align: center; border-bottom: 2px solid grey"
                     )
                  )
            ),
            style="font-family: Serif"
          )
        )
      }
      
      
    })
    
    output$uiIntersectionVenn_Nodes <- renderUI({
      input$storedGraphsOutputIntersectionSelect1
      input$storedGraphsOutputIntersectionSelect2
      # input$btnRefreshPalette2
      # if(!(exists("btnRefreshPalette2Clicked") && btnRefreshPalette2Clicked == T))
      # {
        reactiveVars$last_intersection_data <- NULL
        datasets <- fetchAllSelectedStoredDataset()
        if (length(datasets) < 2) 
          return()
        igraph1 <- convert_to_igraph(datasets[[1]])
        igraph1name <- getDatasetName(attr(datasets[[1]], "id"))
        igraph2 <- convert_to_igraph(datasets[[2]])
        igraph2name <- getDatasetName(attr(datasets[[2]], "id"))
        if(attr(datasets[[1]], "directed") != attr(datasets[[2]], "directed"))
        {
          createAlert(session, "tabIntersectionMainAlert", "cannotIntersectAlert", title = "WARNING !", style = "warning", content = paste0("Impossible to calculate the intersection between a directed and an undirected graph."), append = FALSE)
        }
        else
        {
          g_sim <- graph.intersection(igraph1, igraph2, byname = "auto", keep.all.vertices = FALSE)
          if(length(E(g_sim))==0){
            reactiveVars$last_intersection_data <- list(igraph1, igraph2, 0, 0, igraph1name, igraph2name, 0)
          }else{
          R <- as.matrix(get.adjacency(g_sim, type="both"))
          exportdf <- as_data_frame(R)
          colnames(exportdf)[c(3,4)] <- c(igraph1name, igraph2name)
          reactiveVars$last_intersection_data <- list(igraph1, igraph2, g_sim, R, igraph1name, igraph2name, exportdf)
          }
        }
      # }
      # else
      #   btnRefreshPalette2Clicked <<- F
      
      if(!is.null(reactiveVars$last_intersection_data))
      {
        colors<-randomColor(2)
        vcount1 = length(V(reactiveVars$last_intersection_data[[1]]))
        vcount2 = length(V(reactiveVars$last_intersection_data[[2]]))
        if(length(E(g_sim))==0){
          vcountX=0
        }else{
        vcountX = length(V(reactiveVars$last_intersection_data[[3]]))
        }
        category <- c(reactiveVars$last_intersection_data[[5]], reactiveVars$last_intersection_data[[6]])
        return(
          div(
            fluidRow(
              column(8,
                     div(
                       h3("Node intersection"),
                       renderPlot(
                         draw.pairwise.venn(area1 = vcount1,
                                            area2 = vcount2,
                                            cross.area = vcountX,
                                            category = category,
                                            fill = colors,
                                            ext.text = TRUE,
                                            ext.percent = c(0.1,0.1,0.1),
                                            ext.length = 0.6,
                                            label.col = rep("gray10",3),
                                            lwd = 2,
                                            cex = 2,
                                            fontface = rep("bold",3),
                                            fontfamily = rep("sans",3),
                                            cat.cex = 1.5,
                                            cat.fontface = rep("plain",2),
                                            cat.fontfamily = rep("sans",2),
                                            cat.pos = c(0, 180),
                                            print.mode = c("percent","raw")
                         )
                       ),
                       style="text-align: center"
                     )
              )
            ),
            style="font-family: Serif"
          )
        )
      }
      
      
    })
    
    output$venn_table_edges <- DT::renderDataTable({
      net1 <- input$storedGraphsOutputIntersectionSelect1
      net2 <- input$storedGraphsOutputIntersectionSelect2
      set.seed(123)
      
      datasets <- fetchAllSelectedStoredDataset()
      if (length(datasets) < 2) 
        return()
      igraph1 <- convert_to_igraph(datasets[[1]])
      igraph2 <- convert_to_igraph(datasets[[2]])
      
      common_network<- intersection(igraph1,igraph2)
      common_network<- get.edgelist(common_network)
      common_edges <- as.data.frame(common_network)
      colnames(common_edges)<- c("Source", "Target")
      datatable(common_edges, caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Table: ', htmltools::em('Common connections between the two chosen networks')), rownames = FALSE, extensions = 'Responsive') %>% formatStyle(colnames(df),
                                                                                     fontSize = ui_options["ui_table_font_sz"])
    })
    
    output$venn_table_nodes <- DT::renderDataTable({
      net1 <- input$storedGraphsOutputIntersectionSelect1
      net2 <- input$storedGraphsOutputIntersectionSelect2
      set.seed(123)
      
      datasets <- fetchAllSelectedStoredDataset()
      if (length(datasets) < 2) 
        return()
      igraph1 <- convert_to_igraph(datasets[[1]])
      igraph2 <- convert_to_igraph(datasets[[2]])
      
      common_network<- intersection(igraph1,igraph2)
      common_network<- get.edgelist(common_network)
      common_nodes <- as.data.frame(common_network)
      common_nodes<- c(as.character(common_nodes$V1), as.character(common_nodes$V2))
      common_nodes<- unique(common_nodes)
      common_nodes<- data.frame("Nodes"=common_nodes, "Nodes_hidden"=common_nodes)

      datatable(common_nodes, caption = htmltools::tags$caption(
        style = 'caption-side: bottom; text-align: center;',
        'Table: ', htmltools::em('Common nodes between the two chosen networks')), rownames = FALSE, extensions = 'Responsive', options = list(
          columnDefs = list(list(visible=FALSE, targets=c(1)))
        )) %>% formatStyle(colnames(df),
                                                                                     fontSize = ui_options["ui_table_font_sz"])
    })
    
    
    output$IntersectionNet <- renderVisNetwork({
      net1 <- input$storedGraphsOutputIntersectionSelect1
      net2 <- input$storedGraphsOutputIntersectionSelect2
      set.seed(123)
      
      datasets <- fetchAllSelectedStoredDataset()
      if (length(datasets) < 2) 
        return()
      igraph1 <- convert_to_igraph(datasets[[1]])
      igraph2 <- convert_to_igraph(datasets[[2]])
      dataset_1<- datasets[[1]]
      dataset_2<- datasets[[2]]
      if(is.directed(igraph1) && is.directed(igraph2)){
        common_sources<- intersect(dataset_1$Source,dataset_2$Source)
        common_targets<- intersect(dataset_1$Target,dataset_2$Target)
        common_network<- data.frame("Source"=common_sources, "Target"=common_targets)
      }
      if(!is.directed(igraph1) && !is.directed(igraph2)){
      common_network<- intersection(igraph1,igraph2)
      common_network<-get.edgelist(common_network)
      }
      if(nrow(common_network)==0){
        showModal(modalDialog(
          title = "Important message",
          paste("There are not any common edges and nodes between the selected networks.", sep = "\t"),
          easyClose = T
        ))
      }else{
      common_network<- graph.data.frame(common_network)
      if(!is.directed(igraph1)){
      common_network<- as.undirected(common_network)
      }
      withProgress(min = 0, max = 1, {
        incProgress(message = "Processing data into plot",
                    detail = "This may take a while...",
                    amount = .1)
      visIgraph(common_network, layout = "layout.kamada.kawai") %>%
          visNodes(size = 25, shape = "ellipse") %>%
          # visEdges(arrows = 'to', smooth =F) %>%
          visOptions(highlightNearest = TRUE,
                     nodesIdSelection = TRUE) %>%
          visPhysics(enabled = F) %>%
          visInteraction(
            keyboard = TRUE,
            navigationButtons = TRUE,
            zoomView = TRUE,
            multiselect = TRUE,
            dragView = TRUE
          )
      
      })
      }
    })
    
    observe({
      shinyjs::toggleState("dwnld_commNet", !is.null(reactiveVars$last_intersection_data))
    })    
    
    output$dwnld_commNet <- downloadHandler(filename = function() {
      datasets <- fetchAllSelectedStoredDataset()
      igraph1 <- convert_to_igraph(datasets[[1]])
      igraph1name <- getDatasetName(attr(datasets[[1]], "id"))
      igraph2 <- convert_to_igraph(datasets[[2]])
      igraph2name <- getDatasetName(attr(datasets[[2]], "id"))
      
      paste(igraph1name,"_",igraph2name,"_commonNetwork.txt", sep = "")
    }, content = function(filename) {
      datasets <- fetchAllSelectedStoredDataset()
      igraph1 <- convert_to_igraph(datasets[[1]])
      igraph1name <- getDatasetName(attr(datasets[[1]], "id"))
      igraph2 <- convert_to_igraph(datasets[[2]])
      igraph2name <- getDatasetName(attr(datasets[[2]], "id"))
      common_network<- intersection(igraph1,igraph2)
      common_network<- get.edgelist(common_network)
      colnames(common_network)<- c("Source", "Target")
      
      write.table(x = common_network, file = filename, sep = "\t", row.names = F, quote = F)
    })
    
    output$network3d<- renderUI({
      source("network3d.R", local = T)
      withProgress(min = 0, max = 1, {
        incProgress(message = "Processing data into plot",
                    detail = "This may take a while...",
                    amount = .1)
      output()
      tags$iframe(
        srcdoc = paste(readLines(
          paste("output", Sys.Date(),".html", sep = "")
        ), collapse = '\n'),
        width = "100%",
        height = "850px"
      )
      })
    })
    
    # 
    # output$hive_plot<- renderUI({
    #   source("hive_plot.R", local = T)
    #   withProgress(min = 0, max = 1, {
    #     incProgress(message = "Processing data into plot",
    #                 detail = "This may take a while...",
    #                 amount = .1)
    #     output_hive_plot()
    #     tags$iframe(
    #       srcdoc = paste(readLines(
    #         paste("output_hive_plot", Sys.Date(),".html", sep = "")
    #       ), collapse = '\n'),
    #       width = "750",
    #       height = "700px"
    #     )
    #   })
    # })
    # output$arc_diagram<- renderUI({
    #   source("arc_diagram.R", local = T)
    #   grouping_button <- input$grouping
    #   if(grouping_button=="Group"){
    #     grouping_button<-1
    #   }
    #   if(grouping_button=="Name"){
    #     grouping_button<-2
    #   }
    #   if(grouping_button=="Frequency"){
    #     grouping_button<-3
    #   }
    #   
    #   withProgress(min = 0, max = 1, {
    #     incProgress(message = "Processing data into plot",
    #                 detail = "This may take a while...",
    #                 amount = .1)
    #     arc_diagram()
    #     tags$iframe(
    #       srcdoc = paste(readLines(
    #         paste("arc_diagram", Sys.Date(),".html", sep = "")
    #       ), collapse = '\n'),
    #       width = "100%",
    #       height = "700px"
    #     )
    #   })
    # })
    
    output$two_layer_network<- renderUI({
      input$storedGraphsOutputIntersectionSelect1
      input$storedGraphsOutputIntersectionSelect2
      lay<- input$combobox_layouts2

      if (input$middle_network == T) {
        middle_network = T
      }
      else if (input$middle_network == F) {
        middle_network = F
      }
      source("two_layer_comparison.R", local = T)
      withProgress(min = 0, max = 1, {
        incProgress(message = "Processing data into plot",
                    detail = "This may take a while...",
                    amount = .1)
        output_two_layer()
        tags$iframe(
          srcdoc = paste(readLines(
            paste("two_layer_comparison_", Sys.Date(),".html", sep = "")
          ), collapse = '\n'),
          width = "750",
          height = "700px"
        )
      })
    })
    
    observe({
      shinyjs::toggleState("dwnld_multi_arena", !is.null(reactiveVars$last_intersection_data))
    })    
    output$dwnld_multi_arena <- downloadHandler(filename = function() {
      input$storedGraphsOutputIntersectionSelect1
      input$storedGraphsOutputIntersectionSelect2
      
      paste0("multilayer.txt")
    }, content = function(filename) {
      
      lay<- input$combobox_layouts2
      source("multilayer_dfs.R", local = T)
      write.table(x = multilayer_dfs(), file = filename, row.names = F,sep = "\t", quote = F)
    })
    
    
    #--- Examples ---#
    gavin_2002 <- read.delim("test_input/GAVIN_2002_MCL.txt", header = F)
    gavin_2006 <- read.delim("test_input/GAVIN_2006_MCL.txt", header = F)
    
    output$gavin_2002 <- downloadHandler(
      filename = function() {
        paste("Gavin_2002.txt", sep = '')
      },
      content = function(file) {
        write.table(gavin_2002, file, row.names = F,col.names = F, sep = "\t", quote = F)
      }
    ) 
    output$gavin_2006 <- downloadHandler(
      filename = function() {
        paste("Gavin_2006.txt", sep = '')
      },
      content = function(file) {
        write.table(gavin_2006, file, row.names = F,col.names = F, sep = "\t", quote = F)
      }
    ) 
    
})#The End
