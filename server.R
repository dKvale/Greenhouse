# server.R for Green House Gasses in Minnesota: 1990-2010

library(shiny)
library(dplyr)
library(rCharts)

options("digits"= 5)

# To Configure as 'large'
# shinyapps::configureApp("Greenhouse_Gas_Emissions", size="large")

#------------ Process original Table --------------#
## Read in GHG data and eliminate row numbers
#GHG.table <- read.csv(file="GHG_1990_2010.csv", check.names=F, header=T, stringsAsFactors=F, nrows=500 )
#GHG.table[,"Row number"] <- NULL

## Convert year columns to a single variable
#GHG.table <- melt(GHG.table, id = c("Sector", "Gas", "Activity", "Additional aggregation", "Category"),
#                  variable.name = "Year", value.name = "Emissions")

## Remove commas from emission numbers
#GHG.table$Emissions <- as.numeric(gsub(",", "", GHG.table$Emissions))

## Save edited long table
#write.csv(GHG.table, file="GHG_table_long.csv", row.names=F)

#------------ End of processing --------------#

# Load long table
GHG.table <- read.csv(file="GHG_table_long.csv", header=T, stringsAsFactors=F, nrows=7000 )

#saveRDS(GHG.table, file="GHG_1990_2010.rds")
#GHG.table <- readRDS(file="GHG_1990_2010.rds")

gas_list<-levels(as.factor(GHG.table$Gas))

###----    Server logic to summarize the selected dataset and map the monitor network
shinyServer(function(input, output, session) {
  
  # Generate Pollutant List
  output$gasses <- renderUI({
    selectInput("gas", "", selected = ifelse(is.null(input$gas),"CO2",input$gas), 
                choices =  c("All",gas_list)) 
    })
  
  getyearRange <- reactive({
    ranges <- c(2005,2010)
    if(!is.null(input$gas)) ranges <- range(filter(GHG.table, Gas == input$gas | input$gas =="All")$Year)
    ranges
  })
  
  
  value_has_not_changed <- reactiveValues(x=0)
  
  in_years <- reactive ({
    years <- c(2005,2010)
    ifelse(is.null(input$years), years<- c(1990,2010), years<- input$years)
    years
  })
  
  # Generate Years
  output$yearRange <- renderUI({
    newvalue <- c(2005,2010)
    ranges <- getyearRange()  
    slider <- isolate(in_years())
    if(!is.null(slider)){
      newvalue <- c(max(c(slider[1], ranges[1])), min(c(slider[2], ranges[2])))
      newvalue[1] = ifelse(newvalue[1] < getyearRange()[1] | newvalue[1] >= getyearRange()[2], getyearRange()[1], newvalue[1])
      newvalue[2] = ifelse(newvalue[2] > getyearRange()[2] | newvalue[2] <= getyearRange()[1], getyearRange()[2], newvalue[2])
      if(newvalue[1] == slider[1] & newvalue[2] == slider[2]) value_has_not_changed[["x"]] <-  isolate(value_has_not_changed[["x"]]) + 1
    }
    
    sliderInput("years", "", min=ranges[1], max= ifelse(ranges[1]==ranges[2],ranges[2]+1,ranges[2]), value=c(newvalue[1], ifelse(ranges[1]==ranges[2],newvalue[2]+1, newvalue[2])),  format="####", step=1)
    
  })
  
  
  # Return the requested dataset Filtered by Pollutant
  dataset <- reactive({
    w <- value_has_not_changed[["x"]] 
    years <- c(2005,2010)
    if(!is.null(input$years)) years <- input$years
    ifelse(is.null(isolate(input$gas)), gas <-  "CO2", gas <- input$gas)
    filter(GHG.table, Gas == gas | gas == "All", Year >= years[1], Year <= years[2])
    
  })
  
  # Generate Sectors List
  output$sectors <- renderUI({
    selectInput("sector", "", selected="All", choices=c("All", levels(as.factor(dataset()$Sector)))) })
  
  # Filter dataset to selected Sector
  dataset1 <- reactive({
    sector <- ifelse(is.null(input$sector), "All", input$sector)
    data <- filter(dataset(), sector == "All" | Sector == sector)
    data$Category <- as.character(data$Category)
    data
  })
  
  # Generate Category List
  output$categories <- renderUI({
    selectInput("category", "", selected = "All", choices = c("All", levels(as.factor(dataset1()$Category))))
  })
  
  # Filter dataset by selected Site ID
  dataset2 <- reactive({
    category <- ifelse(is.null(input$category), "All", input$category)
    filter(dataset1(), category == "All"| Category == category )  
  })
  
# Download Button
output$download <- downloadHandler(
  filename = function() { paste("MPCA_GHG_Data_1990_2010.csv", sep="") },
  content = function(con) {
               write.csv(GHG.table, con, row.names=F)
})

# Name of selected gas
#  get_name <- reactive({
#    if(nrow(dataset2())==0) return(NULL)
#    dataset2()$Name[1]
#  })

# Paste title together
titlez <- reactive({
  if(nrow(dataset2()) < 1) return("No data available for this selection.")
  ifelse(input$time=="Emissions", paste(isolate(input$gas), " Annual Emissions"), paste("Running Average", isolate(input$gas), "Emissions"))
}) 

output$stackplot<-renderChart({
  
  if(is.null(isolate(input$gas)) | is.null(dataset2()) ) {
    df <- data.frame(Emissions = rep(NA,11), Year= seq(from=2000, to=2010))
    suppressWarnings(h3 <- hPlot(x="Year", y ="Emissions", type="column", data=df))
    h3$addParams(dom ='barplot')
    h3$xAxis(title=list(text=""), labels=list(enabled=F), tickLength=0, lineWidth=0)
    h3$yAxis(title=list(text =""))
    h3$legend(enabled=F)
    h3$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0)
    
  }
  else if(nrow(dataset2()) > 0){
    stack2 <- dataset2()[,c("Year","Sector","Category", input$time)]
    if(is.null(input$sector) | input$sector=="All" & length(unique(stack2$Sector))>1) stack2$Category<-stack2$Sector
    
    #for(years in unique(stack2$Year)){ 
    # for(category in unique(stack2$Category)){
    #   if(nrow(filter(stack2, Category==category, Year==years))<1) stack2<-rbind(stack2, c(years, filter(stack2, Category==category)$Sector[1], category, 0))
    #}}
    
    stack2 <- group_by(stack2, Category, Year) %.% summarize(Emissions_Total=sum(Emissions))
    
    if(length(unique(stack2$Category))>1){
      all_zeros <- group_by(stack2, Category) %.% summarize(All_Years=sum(Emissions_Total))
      all_zeros <- filter(all_zeros, All_Years==0)
      stack2 <- filter(stack2, !Category %in% all_zeros$Category)
    }
    
    stack2 <- group_by(ungroup(stack2), Category, Year) %.% mutate(Cat_Sum=sum(Emissions_Total))
    stack2 <- ungroup(stack2)
    stack2 <- arrange(stack2, Year, -Cat_Sum)
    
    stack2$Year            <- as.numeric(stack2$Year)
    stack2$Emissions_Total <- as.numeric(stack2$Emissions_Total)
    stack2$Category        <- as.character(stack2$Category)
    stack2$Emissions_Total <- signif(stack2$Emissions_Total, digits=3)
    
    h3 <- Highcharts$new()
    for(category in unique(stack2$Category)){
      h3$series(data=arrange(filter(stack2, Category==category),Year)$Emissions_Total, stacking="normal", name=category, type="column") 
      # To add error bars
      #if(input$time=="Emissions") h3$series(type="errorbar", visible=F, color="grey", 
      #data=lapply(1:nrow(filter(stack2,  Year == years)), function(x) as.vector(unlist(filter(stack2,  Year == years)[x,c("Emissions")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95% UCL: ", "<b>{point.high}</b>", sep="")))  
      #else h3series(type="errorbar", visible=F, color="grey", 
      #data=lapply(1:nrow(filter(stack2, year == years)), function(x) as.vector(unlist(filter(stack2,  year == years)[x,c("Emissions", "UCL_95")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95% UCL: ", "<b>{point.high}</b>", sep="")))  
    }
    if(input$time=="Emissions") h3$colors(c('#2f7ed8', '#8bbc21', '#EAC530', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a','#0d233a','grey'))
    else h3$colors(c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9','#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1','#434348'))
    h3$addParams(dom='stackplot')
    h3$tooltip(followPointer=T, hideDelay=0, animation=T, shared=T, 
               pointFormat=ifelse(TRUE %in% (0 > stack2$Emissions_Total),
                            "<span style=\"color:{series.color}\">{series.name}: </span> <b>{point.y}</b><br/>",
                            "<span style=\"color:{series.color}\">{series.name}: </span> <b>{point.y}</b> ({point.percentage:.1f})%<br/>"))
    h3$xAxis(type="category", categories=if(length(unique(stack2$Year))>1) unique(arrange(stack2,Year)$Year)
             else list(stack2$Year[1]), 
             title=list(style=list(fontSize="13.5px", color="#333333"), margin=13,text="Year"))
    h3$yAxis(min=1.25*min(0, stack2$Emissions_Total), max=1.1*max((group_by(stack2, Year) %.% summarize(sumYear=sum(Emissions_Total)))$sumYear), title=list(style=list(fontSize="13.5px", color="#333333"), marginBottom=5, text="Emissions in Tons"))
    h3$chart(zoomType='', height=460, spacingLeft=6, spacingRight=4, spacingTop=0, spacingBottom=0, marginBottom=46, marginRight=5)
    h3$legend(margin=10, redraw=F, symbolWidth=25, y=ifelse(is.null(input$sector) || input$sector=="All" && input$category!="All", 45, 25), symbolPadding=5, align="center", verticalAlign="top", floating=F, borderWidth=0, padding=10)
    h3$title(margin=25, style=list(fontWeight="bold", color="black"), text=titlez()) 
    if(is.null(input$sector) || input$sector=="All" && input$category!="All"){
      h3$subtitle(y=38, x=-1, style=list(color="darkgrey", fontSize="13.5px", fontWeight="bold"), text=input$category)
    }
    h3$plotOptions(series=list(shadow=F, animation=T, stacking="normal", groupPadding=.13, stickyTracking=F))
    h3$exporting(width=1800, sourceWidth=900, buttons=list(contextButton=list(symbolStrokeWidth=2,text="Print")  ))
  }  
  
  else {
    df <- data.frame(Emissions=rep(NA,11), Sector=seq(from=1, to=11))
    suppressWarnings(h3 <- hPlot(data=df, x="Sector", y ="Emissions", type="column"))
    h3$addParams(dom='stackplot')
    h3$xAxis(title=list(style=list(fontSize="13px"), text=""), type="category", labels=list(enabled=F), categories=df$Sector)
    h3$yAxis(min=0, ceiling=10, title=list(style=list(fontSize="13px"), text=" Emissions in Tons"))
    h3$legend(enabled=F)
    h3$chart(height=460, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0, plotBackgroundColor="#E8E8E8")
    h3$title(style=list(color="darkgrey"), text="No data available for this selection.")
    h3$subtitle(text="Try selecting additional years.")        
  }
  
  suppressWarnings(return(h3))
})
  output$barplot <- renderChart({
    
    if(is.null(isolate(input$gas)) | is.null(dataset2()) ) {
      df <- data.frame(Emissions = rep(NA,11), Year= seq(from=2000, to=2010))
      suppressWarnings(h2 <- hPlot(x="Year", y ="Emissions", type="column", data=df))
      h2$addParams(dom ='barplot')
      h2$xAxis(title=list(text=""), labels=list(enabled=F), tickLength=0, lineWidth=0)
      h2$yAxis(title=list(text =""))
      h2$legend(enabled=F)
      h2$chart(height=450, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0)
      
    }
    else if(nrow(dataset2()) > 0){
      bar2 <- dataset2()[,c("Year","Sector","Category", input$time)]
      bar2<-arrange(bar2,Year)
      if(is.null(input$sector) | input$sector=="All" & length(unique(bar2$Sector))>1) bar2$Category<-bar2$Sector
        #for(years in unique(bar2$Year)) {for(sector in unique(bar2$Sector)) {if(nrow(filter(bar2, Sector==sector, Year == years))<1) bar2<-rbind(bar2, c(years, sector, "category", 0)) }}
      for(years in unique(bar2$Year)){ 
        for(category in unique(bar2$Category)){
          if(nrow(filter(bar2, Category==category, Year == years))<1) bar2<-rbind(bar2, c(years, filter(bar2, Category==category)$Sector[1], category, 0))
      }}
      bar2$Year <- as.numeric(bar2$Year)
      bar2$Emissions <- as.numeric(bar2$Emissions)
      bar2$Category<-as.character(bar2$Category)
      bar2 <- group_by(bar2, Year, Category) %.% summarize(Emissions_Total = sum(Emissions))
      if(length(unique(bar2$Category))>1){
      all_zeros <- group_by(bar2, Category) %.% summarize(All_Years = sum(Emissions_Total))
      all_zeros <- filter(all_zeros, All_Years==0)
      bar2 <- filter(bar2, !Category %in% all_zeros$Category)
      }
      bar2$Emissions_Total <- signif(bar2$Emissions_Total, digits=3)
      
      bar2 <- group_by(bar2, Category) %.% mutate(Cat_Sum=sum(Emissions_Total))
      bar2 <- arrange(bar2, -Cat_Sum)
      bar2$Year <- as.numeric(bar2$Year)
      bar2$Emissions_Total <- as.numeric(bar2$Emissions_Total)
      bar2$Category <- as.character(bar2$Category)
      
      bar2 <- arrange(bar2, Year)
      h2 <- Highcharts$new()
      for(years in unique(bar2$Year)){
          h2$series(data = filter(bar2, Year == years)$Emissions_Total, name=years, type="column") 
        
          # To add error bars
          #if(input$time=="Emissions") h2$series(type="errorbar", visible=F, color="grey", 
          #data =  lapply(1:nrow(filter(bar2,  Year == years)), function(x) as.vector(unlist(filter(bar2,  Year == years)[x,c("Emissions")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95% UCL: ", "<b>{point.high}</b>", sep="")))  
          #else h2$series(type="errorbar", visible=F, color="grey", 
          #data = lapply(1:nrow(filter(bar2,  year == years)), function(x) as.vector(unlist(filter(bar2,  year == years)[x,c("Emissions", "UCL_95")]))), name=paste(years, "95% UCL"), tooltip=list(pointFormat=paste(years, " 95% UCL: ", "<b>{point.high}</b>", sep="")))  
      }
      if(input$time=="Emissions") h2$colors(c('#2f7ed8', '#8bbc21', '#EAC530', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a','#0d233a','grey'))
      else h2$colors(c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9','#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1','#434348'))
      h2$addParams(dom='barplot')
      h2$tooltip(followPointer=T, hideDelay=0, animation=T, shared=F)
      h2$xAxis(categories=if(length(unique(bar2$Category))>1) unique(arrange(bar2,-Cat_Sum)$Category) 
                          else list(unique(arrange(bar2,-Cat_Sum)$Category)), 
               title=list(style=list(fontSize="13.5px", color="#333333"), margin=13,
               text=ifelse(is.null(input$sector) | input$sector=="All", "Sector", "Category")),
               type="category", 
               labels=list(rotation=-58, marginTop=5, enabled=T, align='right'))
      h2$yAxis(min=.999*min(c(0,bar2$Emissions_Total), na.rm=T), max=1.001*max(bar2$Emissions_Total, na.rm=T), title=list(style=list(fontSize="13.5px", color="#333333"), marginBottom=5, text="Emissions in Tons"))
      h2$chart(zoomType='', height=460, spacingLeft=6, spacingRight=4, spacingTop=0, spacingBottom=0, marginBottom=265, marginRight=5)
      h2$legend(margin=10, redraw=F, symbolWidth=25, y=ifelse(is.null(input$sector) || input$sector=="All" && input$category!="All", 45, 25), symbolPadding=5, align="center", verticalAlign="top", floating=F, borderWidth=0, padding=10)
      #bar2[is.na(bar2$Emissions_Total),"Emissions_Total"] <- 0
      h2$title(margin=25, style= list(fontWeight="bold", color="black"), text=titlez()) 
      if(is.null(input$sector) || input$sector=="All" && input$category!="All"){
      h2$subtitle(y=38, x=-1, style=list(color="darkgrey", fontSize="13.5px", fontWeight="bold"), text=input$category)
      }
      h2$plotOptions(series=list(shadow=F,animation=T, groupPadding=.13, stickyTracking=F))
      h2$exporting(width=1800, sourceWidth=900, buttons=list(contextButton=list(symbolStrokeWidth=2,text="Print")  ))
    }     
    else {
          df <- data.frame(Emissions=rep(NA,11), Sector=seq(from=1, to=11))
          suppressWarnings(h2 <- hPlot(data=df, x="Sector", y ="Emissions", type="column"))
          h2$addParams(dom='barplot')
          h2$xAxis(title=list(style=list(fontSize="13px"), text=""), type="category", labels=list(enabled=F), categories=df$Sector)
          h2$yAxis(min=0, ceiling=10, title=list(style=list(fontSize="13px"), text=" Emissions in Tons"))
          h2$legend(enabled=F)
          h2$chart(height=460, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0, plotBackgroundColor="#E8E8E8")
          h2$title(style=list(color="darkgrey"), text="No data available for this selection.")
          h2$subtitle(text="Try selecting additional years.")        
    }
    
    suppressWarnings(return(h2))
  })
  
  output$trends <- renderChart({
    
    if(is.null(isolate(input$gas)) | is.null(dataset2()) ) {
      df <- data.frame(Emissions = rep(NA,12), Year= rep(NA,12))
      suppressWarnings(h1 <- hPlot(x="Year", y = "Emissions", type="line", data = df))
      h1$addParams(dom = 'trends')
      h1$xAxis(title = list(text=""), labels = list(enabled=F), tickLength=0, lineWidth=0)
      h1$yAxis(title = list(text = ""))
      h1$legend(enabled=F)
      h1$chart(height=460, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0)
      
    }
    else if(nrow(dataset2())>0){
      trend2 <- dataset2()[,c("Year","Sector", "Category", input$time)]
      #counts <- group_by(trend2, Sector) %.% summarise(count = length(Sector))
      #if(length(unique(trend2$Sector)) > 7) trend2 <- trend2[trend2$Sector %in% counts[counts$count >1,1], ] 
      #if(length(unique(trend2$Sector)) < 2) trend2 <- dataset2()[,c("Year","Sector", "Category", input$time)]
      #names(trend2)[4] <- "Emissions"
      
      if(is.null(input$sector) | input$sector=="All" & length(unique(trend2$Sector))>1) trend2$Category <- trend2$Sector
      trend2 <- group_by(trend2, Year, Category) %.% summarize(Emissions = sum(Emissions))
      trend2 <- arrange(trend2,Year)
      for(years in unique(trend2$Year)) { 
          for(category in unique(trend2$Category)) {
               if(nrow(filter(trend2, Category==category, Year == years))<1) trend2<-rbind(trend2, c(years, filter(trend2, Category==category)$Sector[1], category, -1.234)) 
      }}
      trend2[trend2==-1.234] <- NA
      trend2$Year <- as.numeric(as.character(trend2$Year))
      trend2$Emissions <- as.numeric(trend2$Emissions)
      if(length(unique(trend2$Category))>1){
      all_zeros <- group_by(trend2, Category) %.% summarize(All_Years = sum(Emissions, na.rm=T))
      all_zeros <- filter(all_zeros, All_Years==0)
      trend2 <- filter(trend2, !Category %in% all_zeros$Category)
      }
      
      trend2 <- group_by(trend2, Category) %.% mutate(Cat_Sum=sum(Emissions))
      trend2 <- arrange(trend2, -Cat_Sum)
      trend2$Year <- as.numeric(trend2$Year)
      trend2$Emissions <- as.numeric(trend2$Emissions)
      trend2$Category<-as.character(trend2$Category)
    
      trend2 <- arrange(trend2,Year)
      trend2$Emissions <- signif(trend2$Emissions, digits=3)
      h1 <- Highcharts$new()
      x  <- 1
      for(category in unique(trend2$Category)) {
           h1$series(data=filter(trend2, Category==category)$Emissions, name=category, type="spline", connectNulls=T, dashStyle=c("line","shortdot","dot")[x%%4+1])
           x <-x+1
      }                         
      h1$addParams(dom='trends')
      if(input$time=="Emissions") h1$colors(c('#2f7ed8', '#8bbc21', '#EAC530', '#1aadce', '#492970', '#f28f43', '#77a1e5', '#c42525', '#a6c96a','#0d233a','grey'))
      else h1$colors(c('#7cb5ec', '#90ed7d', '#f7a35c', '#8085e9','#f15c80', '#e4d354', '#8085e8', '#8d4653', '#91e8e1','#434348'))
      h1$tooltip(followPointer =T, hideDelay = 0, animation=T, shared=F)
      h1$xAxis(categories = if(length(unique(trend2$Year))>1) unique(trend2$Year) else list(unique(trend2$Year)),
               title = list(style=list(fontSize="13.5px", color="#333333"), margin=13, text="Year"),
               type="category")
      h1$yAxis(min=min(trend2$Emissions, na.rm=T), max=1.001*max(trend2$Emissions, na.rm=T), title = list(style=list(fontSize="13.5px", color="#333333"), marginBottom=5, text="Emissions in Tons"))
      h1$chart(height=460, spacingLeft=5, spacingRight=4, spacingTop=0, marginBottom=46, marginRight=5)
      h1$legend(margin=10, redraw=F, symbolWidth=20, symbolPadding=5, x=40, y=ifelse(is.null(input$sector) || input$sector=="All" && input$category!="All", 42, 22), align="center", verticalAlign="top", floating=F, borderWidth=0, paddingBottom=5, width=890)
      h1$title(margin=25, style= list(fontWeight="bold", color="black"), text=titlez()) 
      if(is.null(input$sector) || input$sector=="All" && input$category!="All"){
          h1$subtitle(y=38,x=-1, style= list(color="darkgrey", fontSize="13.5px", fontWeight="bold"), text=input$category)
      }
      h1$plotOptions(series=list(shadow=T,animation=T,  pointPadding=7, lineWidth=1.9, marker=list(radius=4.2), groupPadding=1, stickyTracking=F, pointInterval=1))
      h1$exporting(width=1800, sourceWidth=900, buttons=list(contextButton=list(symbolStrokeWidth=2,text="Print ", marginRight=5)  ))
    }     
    else {
          df <- data.frame(Emissions=rep(NA,21), Year= seq(from=1990, to=2010))
          suppressWarnings(h1 <- hPlot(x="Year", y="Emissions", type="line", data = df))
          h1$addParams(dom='trends')
          h1$xAxis(title=list(style=list(fontSize="13px"), text="Year"), type="category", categories=df$Year, min=1990, max=2010)
          h1$yAxis(min=0, ceiling=10, title=list(style=list(fontSize="13px"), text=paste(isolate(input$gas), " Emissions in Tons")))
          h1$legend(enabled=F)
          h1$chart(height=460, spacingLeft=5, marginBottom=45, marginRight=0, spacingRight=0, plotBackgroundColor="#E8E8E8")
          h1$title(style=list(color="darkgrey"), text="No data available for this selection.")
          h1$subtitle(text="Try selecting additional years.")        
    }
    
    suppressWarnings(return(h1))
    
  })
  
  output$table = renderDataTable({
    if(is.null(input$category)) return(NULL)
    data <- dataset2()
    #options(scipen=999)
    #data$Emissions <- signif(data$Emissions, digits=3)
    data$Emissions <- formatC(as.numeric(data$Emissions), format="g", big.mark=",", digits=3)
    #options("digits"= 3)
    if(is.null(input$allData) | input$allData == F) data[order(data$Year, data$Sector, data$Category),-c(3,4)]
    else data[order(data$Year, data$Sector, data$Category)]
  }, options= list(bLengthChange=T, digits=3, aLengthMenu = c(5, 10, 25, 50), sInfoThousands=",", iDisplayLength = 10, bFilter=T))
  
  
})
