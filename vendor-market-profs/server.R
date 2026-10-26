# server.R
source("AkHiStPoly.R")
source("stMaps.R")
source("vendor-market.R")
source("helpers.R")
ven<-read.table("data/ep-vendor-data.txt",sep=",",header=T,colClasses="character")
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])
xwalk<-read.table("data/xwalk.txt",header=T,sep=",",colClasses="character")
rank<-read.table("data/ep-vendor-rank.txt",sep=",",header=T,colClasses="character")

shinyServer(
  function(input, output) {
  
  output$ui<-renderUI({
    if(is.na(input$metric))
      return()
    
    switch(input$metric,
           "Market Share"=sliderInput("range",
                                      label=h6("Choose a Percentage Range"),
                                      min = 0, max = 100, value=c(0,25)),
           "Ranking"="")
  })  
  output$map<-renderPlot(height=500,{
    metric<-switch(input$metric,
                   "Market Share" = ven[,1:5],
                   "Market Rank" = rank[,1:6]
                   )

    
  percent_map(data = metric,
                year = input$year,
                vendor = input$var,
                system = input$system,
                legend.title = input$var, 
                labs = input$labs[1],
                ctrOff = ctrOff,
                xwalk = xwalk,
                min = input$range[1], 
                max = input$range[2])
  })
   
})
