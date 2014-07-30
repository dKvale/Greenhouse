########################
#
#  File: ui.R
#  Purpose: Chart trends and summarize chosen GHG data
#
########################

library(shiny)
library(dplyr)
library(rCharts)

shinyUI(fluidPage(
  tags$head(    
    tags$style(type='text/css', ".select { max-width: 190px; }"),
    tags$style(type='text/css', ".jslider { max-width: 250px; }"),
    tags$style(type='text/css', ".row-fluid { margin:auto; max-width: 100%; margin-top: 0px; margin-bottom:0; z-index:1; padding-top: 0px; padding-bottom: 0px; }"),
    tags$style(type='text/css', ".span4 { max-width: 250px; margin:0; min-width: 150px; }"),
    tags$style(type='text/css', ".span2 { max-width: 170px; margin:0; min-width: 150px; padding-left: 10px;}"),
    tags$style(type='text/css', ".span3 { max-width: 220px; padding-top: 0px; padding-left: 10px; margin-left: 0px;  margin-top: 0px; }"),
    tags$style(type='text/css', ".span12 { margin-left: 1px; padding-left: 5px; margin-bottom: -5px; margin-top: 0px; padding-top:0px; padding-bottom: 0px;}"),
    tags$style(type='text/css', ".hr { padding: 0px; max-height: 3px; margin-left: auto; margin-right: auto; width: 100%; height: 3px; margin-top: 10px; background-color: #D8D8D8; color: #D8D8D8; }"),
    tags$style(type='text/css', ".h1 { margin-left:5px; margin-bottom:0px; padding-bottom:0; font-size:26px;  }"),
    tags$style(type='text/css', ".h4 { margin-top: 0; padding-top:0; }"),
    tags$style(type='text/css', ".download { width: 82%; height: 18px; max-width: 100px; color: #0000FF; padding-top: 4px; margin:0px; margin-top: 4px; margin-bottom:0px}"),
    tags$style(type='text/css', ".container-fluid { margin-top: 0; margin-bottom: 0; padding-bottom:0; padding-top: 0; }"),
    tags$style(type='text/css', ".shiny-html-output { margin: auto; margin-top: 0;  padding-top:0; }")
    
  ),
  
  #Web Title
  title="Greenhouse Gas Emissions: 1990-2010",
  
  #Page Header
  
  fluidRow(
    column(12,
           h1("Greenhouse Gas Emissions",style="margin-left:9px; margin-top:15px; font-size:36px; margin-bottom:0px; padding-bottom:5px;")
           )),
  
  fluidRow(
    column(2,
           img(src="https://drive.google.com/uc?id=0B9Ub5HCtyNmkME55b1FwU3l1NkE", style="margin:auto; margin-top:10px; padding-top:10px; width:108%; height:180px; max-width:200px; max-height:190px;")       
    ),
    
    column(3,
           br(),
           h4("Gas:"), uiOutput("gasses"),
           h4("Years:"), uiOutput("yearRange")
           
                
    ),
    column(3,
           br(),   
           h4("Sector:"), uiOutput("sectors"),
           h4("Category:"), uiOutput("categories")
           
    ),
    column(3,
           br(),
           h4("Summary:"),  
           radioButtons("time", "",
                        list("Annual Total"="Emissions")),
                        #"Running Toal"="Average")),
           br(),
           h4("Download:", style="margin-top: 0px;"),
           downloadButton("download", label="Save Data", class="download")
    )   
  ),
  
  fluidRow(
    column(12, 
           hr(class="hr")   
    )),
  
  # Show 3 tabs: chart, map and summary of the dataset
  mainPanel(
    tags$head(
      tags$style(type='text/css', ".span8 { margin: 0; margin-bottom:0;  margin-top: 0; padding: 0px; width: 100%; }"),
      tags$style(type='text/css', ".checkbox { zindex:5; float=F; margin-top: -5px; margin-left: 9px; padding-bottom: 0px; margin-bottom: 25px;"),
      tags$style(type='text/css', ".span { margin-top: 0 px; margin-bottom: 0px; padding:0px; }"),
      tags$style(type='text/css', ".span6 { height:0px; margin-top: 0 px; margin-bottom: 0px; padding:0px; }"),
      tags$style(type='text/css', "#title { padding:0;  font-size:16px;  padding-left:17px; margin:0;  margin-top: 1px;}"),
      tags$style(type='text/css', "#para { padding:0;  font-size:14.5px; margin:0;  margin-left:32px; margin-top: 4px; margin-right: 14px;}"),
      tags$style(type='text/css', ".h5 { margin:0; margin-left: 12px; padding-top:0; padding-left:12px;  }")
      #tags$style(type='text/css', "#trends { margin: 0; margin-top: 1px; padding:0; width: 99.5%; }"),
      #tags$style(type='text/css', "#stackplot { margin: 0; margin-top: 1px; padding:0; width: 99.5%; }"),
      #tags$style(type='text/css', "#barplot { margin: 0; margin-top: 1px; padding:0; width: 99.5%; }")
    ),
    
    
    tabsetPanel(id ="tabs1",
      tabPanel("Stacked Bars", showOutput("stackplot", "highcharts")),
      tabPanel("Trend Lines",  showOutput("trends", "highcharts")),
      tabPanel("Bar Charts",   showOutput("barplot", "highcharts")),
      tabPanel("Data Table", checkboxInput("allData", label = "Show All Columns", value = F), dataTableOutput("table"))
      #tabPanel("About", h5("Greenhouse Gasses", id="title"), p("A greenhouse made of glass is often used for growing plants. During the day the sunlight penetrates its transparent walls and begins to heat the air inside.", id ="para"),
                   #     img(src=" http://climatekids.nasa.gov/review/greenhouse-effect/greenhouse1.jpg", style="margin-left:32px; margin-top:2px; padding-top:2px; padding-bottom:4px; width:260px; height:210px; max-width:260px; max-height:210px;"),       
                   #     h5("Climate Change", id="title"), p(""),br(),
                    #    h5("CO2 Equivalents", id="title"), p(""),br(),
                    #    h5("Current Data", id="title"), p(""),br(),
                    #    h5("Additional Resources", id="title"), p(""),br(),
               #tags$style('.title {min-width: 99.9%; width:99.9%; padding:0; height: 405px; }'))
      
    ),
  
    
    HTML("<hr noshade size='1'/>") )
  
  #fluidRow(
    #column(12,  
           #p("For more information visit the MPCA's *Climate Change* website at http://www.pca.state.mn.us/qzqh619.  For questions please contact Robert Steinwurst at rstein@state.mn.us. Last modified 7/2/2014.")
           
    #))
  
  
))
