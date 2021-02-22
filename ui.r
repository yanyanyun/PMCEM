##################################################### Integration ###########################################################
#load package
library(shiny)
library(rgdal)
library(leaflet)
library(shinycssloaders)
library(shinythemes)

# ui object

fluidPage(
  theme = shinytheme("lumen"),
  
  titlePanel(p("Pigeon Management Cost Effective Modelling")),
  
  
  leafletOutput(outputId = "map"),
  ###<headerPanel(""),>increase the grey space. <titlePanel(""),>does not.
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      
      navbarPage("Input"),
      #tags$style(type = 'text/css', '.navbar { background-color: #C0C0C0;
      #font-family: Helvetica;
      #font-size: 50px;
      #font-style: bold;
      #color: black; }'),
      
      
      fileInput(
        inputId = "dens",
        label = "Upload density. Choose csv file",
        accept = c(".csv")
      ),
      
      fileInput(
        inputId = "cost",
        label = "Upload cost. Choose csv file",
        multiple = TRUE,
        accept = c(".csv")
      ),
      
      
      textInput("gr", "Pigeon growth rate (per month)","0.02775"),
      
      textInput("expct", "Density must under____ pigeons/ha"),
      
      textInput("mth", "Achieve target in____months"),
      
      actionButton("calculate", "Submit", class = "butt"),
      #tags$head(tags$style(".butt{background-color:#C0C0C0;} .butt{color: black;} .butt{font-family: Helvetica}")),
    ),#end sidebarPanel
    
    mainPanel(
      ###<headerPanel(""),>increase the grey space. <titlePanel(""),>does not.
      titlePanel(""),
      navbarPage("Output"),
      
      p("Average pigeons/ha in selected cells"),
      verbatimTextOutput("aver", placeholder = TRUE),
      
      p("Cost for handling covariates"),
      verbatimTextOutput("sum_v", placeholder = TRUE),
      
      p("Cost for culling"),
      verbatimTextOutput("sum_c", placeholder = TRUE),
      
      p("Detailed management methods:"),
      downloadButton("downloadData",label = "Save as KML", class = "butt1"),
      #tags$head(tags$style(".butt1{background-color:#C0C0C0;} .butt1{color: black;} .butt1{font-family: Helvetica}"))
      
    )#end mainPanel
  )#end sidebarLayout
)#end UI
