
#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# packages
library(shiny)
library(ggplot2) # Graphing
library(dplyr) # Data manipulation
library(magrittr) # Pipe operator
library(stargazer) # Pretty Regression Tables
library(rio) # universal file reading package
library(summarytools) # summary statistics
library(bnlearn) # bayesian networks
library(fastDummies) # helper function to convert factors to dummies
library(reshape)
library(plyr)
library(reshape2)
library(pryr) # where - to get environment
library(causaleffect)
library(igraph)
library(OneR) # binning function


# Define server logic to read uploaded data and run a regression or decision tree
shinyServer(function(input, output) {
  
  # Read in regression data
  readData <- reactive({
    
    # read data with rio's import function which will handle most common formats
    df <- import(input$dataFile$datapath)
    
    # change column names so they're well formed
    colnames(df) <- gsub(" ", "_", colnames(df))
    colnames(df) <- gsub("[^a-zA-Z0-9_]", "", colnames(df))
    colnames(df) <- gsub("__+", "_", colnames(df)) # two or more underscores is bad
    colnames(df) <- gsub("_", "", colnames(df)) # two or more underscores is bad
    
    # return data
    return(df)
  })
  
  # Read in regression data
  readTestData <- reactive({
    # read data with rio's import function which will handle most common formats
    df <- import(input$dataTestFile$datapath)
    # change column names so they're well formed
    colnames(df) <- gsub(" ", "_", colnames(df))
    colnames(df) <- gsub("[^a-zA-Z0-9_]", "", colnames(df))
    colnames(df) <- gsub("__+", "_", colnames(df)) # two or more underscores is bad
    colnames(df) <- gsub("_", "", colnames(df)) # two or more underscores is bad
    
    selected_vars <- df[,colnames(df) %in% input$variables]
    selected_vars  <- na.omit(selected_vars)
    # make sure all character columns are converted to factors

    selected_vars[sapply(selected_vars, is.character)] <- lapply(selected_vars[sapply(selected_vars, is.character)], as.factor)
    selected_vars[sapply(selected_vars, is.integer)] <- lapply(selected_vars[sapply(selected_vars, is.integer)], as.numeric)
  
    # If discretize == Yes then convert all to factors
    if (input$discretize == "Y") {
      selected_vars <- bin(selected_vars, nbins=20, method="content", na.omit = FALSE)
    }
    
    df <- selected_vars
    rm(selected_vars)
    
    # return data
    return(df)
  })
  
  # Return column names to UI to choose regression variables
  output$moreControls <- renderUI({
    # read data
    df <- readData()
    # List interactive controls
    tagList(
      # independent variables (explanatory variables)
      selectizeInput(inputId = "variables", 
                  label = "Select Variables", 
                  choices = colnames(df), 
                  multiple = TRUE)
    )
  })
  
  # cp dist controls
  output$cpDist <- renderUI({
    df <- readData()
    selected_vars <- df[,colnames(df) %in% input$variables]
    selected_vars  <- na.omit(selected_vars)
    df <- selected_vars
    tagList(
      selectizeInput(inputId="cpDistVar", 
                  label = "Cond. Prob. Dist. Var.", 
                  choices = colnames(df),
                  multiple = FALSE)
    )
  })
  
  output$textQuery <- renderUI({
    if (input$queryMethod=="Text") {
      tagList(
          textAreaInput(inputId="cpQuery", label = "What is the probability that ..."),
          # have to use quote() for factor levels
          textAreaInput(inputId="cpEvidence", label = "Given that ...")
      )
    }
  })
  
  # causal effect controls
  output$causalEffectControl <- renderUI({
    df <- readData()
    selected_vars <- df[,colnames(df) %in% input$variables]
    selected_vars  <- na.omit(selected_vars)
    df <- selected_vars
    tagList(
      selectInput(inputId = "y",
                  label = "Variables of interest given intervention:",
                  choices = colnames(df),
                  multiple = T),
      selectInput(inputId = "x",
                  label = "Variables acted on:",
                  choices = colnames(df),
                  multiple = T),
      selectInput(inputId = "z",
                  label = "Variables conditioned on:",
                  choices = colnames(df),
                  multiple = T)
    )
  })
  
  # bayesNet dataFrame needs to be processed a bit more
  getBayesDf <- reactive({
    df <- readData()
    # select variables for model
    selected_vars <- df[,colnames(df) %in% input$variables]
    selected_vars  <- na.omit(selected_vars)
    # make sure all character columns are converted to factors
    selected_vars[sapply(selected_vars, is.character)] <- lapply(selected_vars[sapply(selected_vars, is.character)], as.factor)
    selected_vars[sapply(selected_vars, is.integer)] <- lapply(selected_vars[sapply(selected_vars, is.integer)], as.numeric)
    
    # If discretize == Yes then convert all to factors
    if (input$discretize == "Y") {
      selected_vars <- bin(selected_vars, nbins=20, method="content", na.omit = FALSE)
    }
    
    return(selected_vars)
    
  })
  
  # get col_types
  coltypes <- reactive({
    df <- getBayesDf()
    
    col_types <- sapply(df, type_sum) %>% t() %>% as.data.frame()
    colnames(col_types) <- colnames(df)
    return(col_types)
  })
  
  
  # get col_types
  coltypes_for_lw <- reactive({
    df <- getBayesDf()
    if (input$method == "lw") {
      df <- df[,colnames(df)!=input$cpDistVar]
    }
    col_types <- sapply(df, type_sum) %>% t() %>% as.data.frame()
    colnames(col_types) <- colnames(df)
    return(col_types)
  })

  
  # select parameters for model
  output$paramSelection <- renderUI({
    if (input$queryMethod=="GUI") {
      df <- getBayesDf()
      col_types <- coltypes()
      #cols <- colnames(df)
      # split into var types
      nums <- df[,col_types[1,]=="dbl"] %>% as.data.frame()
      facts <- df[,col_types[1,]=="fct"] %>% as.data.frame()
      params <- list()
      # first slider types for nums
      if (nrow(nums) > 0) {
        params <- c(params, 
                    lapply(1:ncol(nums), function(i) { 
                       sliderInput(inputId = colnames(nums)[i],
                            label = colnames(nums)[i],
                            value = c(min(nums[,i]), max(nums[,i]) ),
                            min = min(nums[,i]),
                            max = max(nums[,i]))}) %>% as.list())
      }
      # then factor levels
      if (nrow(facts) > 0) {
        params <- c(params, 
                    lapply(1:ncol(facts), function(i) { 
                      selectInput(inputId = colnames(facts)[i],
                                  label = colnames(facts)[i],
                                  choices = levels(facts[,i]),
                                  selected = levels(facts[,i]),
                                  multiple = TRUE)}) %>% as.list())
      }
      params
    }
  }) 
  
  # parse arcs
  genArcs <- reactive({
    df <- readData()
    #selected_vars <- df[,colnames(df) %in% input$variables]
    #selected_vars  <- na.omit(selected_vars)
    #data <- selected_vars
    data <- df
    cols <- colnames(data)
    cols2 <- colnames(data)
    arcs <- expand.grid(cols, cols2, stringsAsFactors = FALSE)
    colnames(arcs) <- c("from", "to")
    arcs$from %<>% as.character()
    arcs$to %<>% as.character()
    arcs <- arcs[arcs$from!=arcs$to,]
    arcs$id <- paste0(arcs$from, " -> ", arcs$to)
    return(arcs)
  })

    # Bayesian Network Structure
  bayesNetStruc <- reactive({
    df <- getBayesDf()
    whitelist <- genArcs()
    whitelist <- whitelist[whitelist$id %in% input$arcs,]
    whitelist <- whitelist[,c("from", "to")]
    
    network <- empty.graph(colnames(df))
    arcs(network) <- whitelist %>% as.matrix()
    
    whitelist %<>% as.matrix()
    
    # structure learning
    if (input$learnStruc =="Y") {
      network <- rsmax2(df[,colnames(df)], whitelist = whitelist)
    }
   # plot(network)
    
    # structure learning the directed graph for hybrid networks with hill climbing
    #dag <- hc(df, whitelist = whitelist)
    # dag <- pdag2dag(dag, ordering = colnames(selected_vars))
    return(network)
  })
  
  # select causal relationships
  output$selectArcs <- renderUI({
      arcs <- genArcs()
      tagList(
        # arcs
        selectizeInput(inputId = "arcs", 
                       label = "Select Causal Relationships", 
                       choices = arcs$id,
                       multiple = TRUE)
    )
  })
  
  # plot Bayesian Network
  observeEvent(input$updateStructure, {
    output$bayesNetPlot <- renderPlot({
      isolate({
        graphviz.plot(bayesNetStruc(),
                      layout = "dot",
                      render = TRUE,
                      shape = "rectangle",
                      main = "Network Diagram")
      })
    })
  })
  # fit Bayesian Network
  fitBayesNet <- reactive({
    df <- getBayesDf()
    fit <- bayesNetStruc() %>% bn.fit(data=df, keep.fitted = TRUE)
    return(fit)
  })
  
  # query / cp dist params
  queryParams <- reactive({
    query <- input$cpQuery
    distVar <- input$cpDistVar
    cpEvidence <- input$cpEvidence
    return(as.character(c(query, distVar, cpEvidence)))
  })
  
  # evidence
  get_evidence <- reactive({
    df <- getBayesDf()
    col_types <- coltypes()
    
    # split into var types
    nums <- df[,col_types[1,]=="dbl"] %>% as.data.frame()
    facts <- df[,col_types[1,]=="fct"] %>% as.data.frame()
    
    # subset based on min and max or var levels
    subset_df <<- df
    if (ncol(nums) > 0 & nrow(subset_df > 0) & nrow(nums) > 0) {
      for(i in 1:ncol(nums)) {
        # between max and min
        subset_df <<- subset_df[(subset_df[,colnames(nums)[i]] >= input[[colnames(nums)[i]]][1]) & 
                          (subset_df[,colnames(nums)[i]] <= input[[colnames(nums)[i]]][2]), ]
      }
    }
    if (ncol(facts) > 0 & nrow(subset_df) > 0 & nrow(facts) > 0) {
      for(i in 1:ncol(facts)) {
        subset_df <<- subset_df[(subset_df[,colnames(facts)[i]] %in% input[[colnames(facts)[i]]]), ]
      }
    }
    
    rows_not_in_common  <- function(a1,a2) {
      evidence_vec <- rep(T, nrow(a1))
      for (i in 1:ncol(a1)) {
        evidence_vec <- evidence_vec & ((a1[,i] %in% a2[,i]))
      }
      return(evidence_vec)
    }
    
    evidence_vec <- rows_not_in_common(df, subset_df)
    return(evidence_vec)
  })
  
  # liklihood weighting evidence
  get_lw_evidence <- reactive({
    
    df <- getBayesDf()
    
    # exclude cpdistvar for liklihood weighting or else its sampled on the uniform distribution
    df <- df[,colnames(df)!=input$cpDistVar]
    
    col_types <- coltypes_for_lw()
    
    # split into var types
    nums <- df[,col_types[1,]=="dbl"] %>% as.data.frame()
    facts <- df[,col_types[1,]=="fct"] %>% as.data.frame()
    
    # preallocate named list
    lw_evidence <- as.list(colnames(df))
    names(lw_evidence) <- colnames(df)
    
    if (ncol(nums) > 0 & nrow(subset_df > 0) & nrow(nums) > 0) {
      for(i in 1:ncol(nums)) {
        # between max and min
        lw_evidence[[colnames(nums[i])]] <- c(input[[colnames(nums)[i]]][1], input[[colnames(nums)[i]]][2]) %>% as.numeric()
      }
    }
    if (ncol(facts) > 0 & nrow(subset_df) > 0 & nrow(facts) > 0) {
      for(i in 1:ncol(facts)) {
        lw_evidence[[colnames(facts[i])]] <- input[[colnames(facts)[i]]]
      }
    }
    
    return(lw_evidence)
  })
  
  
  output$num_text <- renderText({
    df <- getBayesDf()
    col_types <- coltypes()
    
    # split into var types
    nums <- df[,col_types[1,]=="dbl"] %>% as.data.frame()
    facts <- df[,col_types[1,]=="fct"] %>% as.data.frame()
    paste(input[[colnames(nums)[1]]][1], input[[colnames(nums)[1]]][2])
    paste(input[[colnames(facts)[1]]][1], input[[colnames(facts)[1]]][2])
    
  })
  
  # bayesNet query
  observeEvent(input$runQuery,{
    output$bayesNetQuery <- renderText({
      isolate({
        fit <- fitBayesNet()
        query_text <<- input$cpQuery[1]
        evidence_text <<- input$cpEvidence[1]
        if (input$queryMethod=="Text") {
          prob <- cpquery(fit,
                  event = eval(parse(text=query_text)),
                  evidence = eval(parse(text=evidence_text)), #evidence_vec,
                  #n = length(evidence_vec),
                  method=input$method,
                  debug=TRUE,
                  n=50000)
        }
        else {
          evidence_vec <<- get_evidence() %>% as.logical()
          prob <- cpquery(fit,
                          event = eval(parse(text=query_text)),
                          evidence = evidence_vec,
                          n = length(evidence_vec),
                          method=input$method,
                          debug=TRUE)
        }
        paste0("The probability that ", query_text, "\ngiven that ", evidence_text, "\nis approximately: ", round(prob*100, digits=2), "%")
    })
    })
  })
  
  # bayesNet CP Dist
  observeEvent(input$updateDist,{
    output$bayesNetCpDist <- renderPlot({
      isolate({
        
        fit <- fitBayesNet()
        if (input$queryMethod=="Text") {
          evidence_text <<- input$cpEvidence[1]
          condDist <<- cpdist(fit, 
                             nodes=input$cpDistVar,
                             evidence=eval(parse(text=evidence_text)),
                             #n=length(evidence_vec),
                             method=input$method,
                             debug = TRUE,
                             n= 50000) %>% as.data.frame()
        }
        else {
          if (input$method == "lw") {
            lw_evidence <<- get_lw_evidence()
            condDist <<- cpdist(fit, 
                                nodes=input$cpDistVar,
                                evidence=lw_evidence,
                                method=input$method,
                                debug = TRUE) %>% as.data.frame()
          }
          else {
            evidence_vec <<- get_evidence() %>% as.logical()
            condDist <<- cpdist(fit, 
                                nodes=input$cpDistVar,
                                evidence=evidence_vec,
                                n=length(evidence_vec),
                                method=input$method,
                                debug = TRUE) %>% as.data.frame()
          }
        }
        colnames(condDist)[1] <- "v1"
        if (is.factor(condDist[,1]) == FALSE) {
          condDist %>% ggplot(aes(v1, color="red", fill="red")) + geom_histogram() + ggtitle(paste(input$cpDistVar, "Conditional Probability")) + xlab(input$cpDistVar)
        }
        else {
          condDist %>% ggplot(aes(v1, color="red", fill="red")) + geom_histogram(stat = "count") + ggtitle(paste(input$cpDistVar, "Conditional Probability")) + xlab(input$cpDistVar)
        }
      })
    })
  })
  
  # causal inference
  causalEffect <- reactive({
    whitelist <- genArcs()
    whitelist <- whitelist[whitelist$id %in% input$arcs,]
    whitelist <- whitelist[,c("from", "to")]

    graph <- whitelist %>% as.matrix() %>% graph_from_edgelist()
    effect <- causal.effect(y=input$y, x=input$x, z=input$z, G=graph, simp = T, expr = T) 
  #  effect <- gsub("_", "", effect)
    return(effect)
  })
  
  # Effect Text
  output$effectText <- renderUI({
    withMathJax(
      p(paste0("Causal effect: $$", causalEffect(), "$$"))
    )
  })

  # Prediction var
  output$SelectPredictVar <- renderUI({
    df <- getBayesDf()
    tagList(
      selectInput(inputId = "predictVar",
                  label = "Variable to Predict",
                  choices = colnames(df))
    )
  })
  
  # Make predictions
  predictData <- reactive({
    fit <- fitBayesNet()
    if (input$trainInstead =="Y") {
      testData <- getBayesDf()
    }
    else {
      testData <- readTestData()
    }
    predictions <- predict(fit, node=input$predictVar, data=testData) %>% as.data.frame()
    return(predictions)
  })
   
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      predictions <- predictData()
      write.csv(predictions, file)
    }
  )
  
  output$plotPredictions <- renderPlot({
    preds <- predictData()
    colnames(preds)[1] <- "preds"
    if (input$trainInstead == "N") {
      preds %>% ggplot(aes(preds)) + geom_density()
    }
    else {
      df <- getBayesDf()
      df <- cbind(preds, df[,input$predictVar]) %>% as.data.frame()
      colnames(df)[2] <- "test"
      df %>% ggplot(aes(test, preds)) + geom_point() + geom_smooth(method = "lm")
    }
  })
})
