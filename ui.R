#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/

# packages
library(shiny)
library(shinythemes)

# Define UI for application that outputs the results of linear regression or decision tree
shinyUI(navbarPage(theme = shinytheme("darkly"),
                   # Application title
                   title = "Bayesian Network App",
                   # Bayesian Network
                   tabPanel("Model",
                            type = "tabs",
                            # Sidebar with stuff
                            sidebarLayout(
                              sidebarPanel(
                                # Upload data file
                                fileInput(inputId = "dataFile",
                                          label = "Choose Dataset (.csv or .xlsx)",
                                          accept = c(".csv", ".xlsx")),
                                p("The file should have the header on the first row."),
                                # Select Variables
                                uiOutput("moreControls"),
                                # Arc
                                uiOutput("selectArcs"),
                                actionButton(inputId="updateStructure", label="Update Network"),
                                radioButtons("discretize", "Discretize?", choices=c("Y", "N"), selected = "N", inline=T),
                                radioButtons("learnStruc", "Learn Structure?", choices=c("Y", "N"), selected = "N", inline=T)
                                
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                # suppress errors
                                #tags$style(type="text/css",
                                #           ".shiny-output-error { visibility: hidden;}",
                                #           ".shiny-output-error: before { visibility: hidden; }"),
                                # Tabs for each model type
                                tabsetPanel(type="tabs",
                                            # Bayesian Network
                                            tabPanel("Diagram",
                                                     plotOutput("bayesNetPlot"),
                                                     uiOutput("causalEffectControl"),
                                                     withMathJax(uiOutput("effectText")),
                                                     plotOutput("causal_plot")),
                                            tabPanel("Queries",
                                                     radioButtons(inputId="queryMethod", label="Query Method", choices=c("GUI", "Text"), inline=T),
                                                     uiOutput("paramSelection"),
                                                     uiOutput("textQuery"),
                                                     textOutput("num_text"),
                                                     splitLayout(
                                                       radioButtons(inputId="method", label="Est. Method", choices = c("lw","ls"), selected = "ls", inline=T),
                                                       actionButton(inputId="runQuery", label="Run Query"),
                                                       actionButton(inputId="updateDist", label="Update Distribution")),
                                                     p(textOutput("bayesNetQuery"), style="font-size: 20px"),
                                                     uiOutput("cpDist"),
                                                     plotOutput("bayesNetCpDist")),
                                            tabPanel("Predict",
                                                     # Upload data file
                                                     fileInput(inputId="dataTestFile",
                                                               label="Choose Test Dataset (.csv or .xlsx)",
                                                               accept=c(".csv", ".xlsx")),
                                                     radioButtons("trainInstead", "Use training data instead?", choices=c("Y", "N"), selected = "Y", inline=T),
                                                     uiOutput("SelectPredictVar"),
                                                     downloadLink("downloadData", "Download Predictions"),
                                                     plotOutput("plotPredictions"))
                                ) # tabset panel
                              ) # end Model panel
                            )
                   )
))
