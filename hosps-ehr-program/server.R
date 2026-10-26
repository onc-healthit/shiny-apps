# server.R

source("AkHiStPoly.R")
source("stMaps.R")
source("helpers.R")
mu<-read.table("data/state-rates.csv",sep=",",header=T,colClasses="character")
mu[,3]<-as.numeric(mu[,3])
mu[,4]<-as.numeric(mu[,4])
mu[,5]<-as.numeric(mu[,5])
mu[,6]<-as.numeric(mu[,6])
mu[,7]<-as.numeric(mu[,7])
mu[,8]<-as.numeric(mu[,8])
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])

shinyServer(
  function(input, output) {
  output$map<-renderPlot({
    metric<-if(input$type=="All Eligible and Critical Access Hospitals" & input$paid=="Demonstrated Meaningful Use of Certified Health IT")
			{mu[,5]}
			else if(input$type=="All Eligible and Critical Access Hospitals" & input$paid=="Demonstrated Meaningful Use and/or Adopted Implemented, or Upgraded any EHR")
			{mu[,3]}
			else if(input$type=="Critical Access and Small Rural Hospitals" & input$paid=="Demonstrated Meaningful Use of Certified Health IT")
			{mu[,6]}
			else if(input$type=="Critical Access and Small Rural Hospitals" & input$paid=="Demonstrated Meaningful Use and/or Adopted Implemented, or Upgraded any EHR")
			{mu[,4]}
    percent_map(data = mu,
				var=metric,
				type=input$type,
				paid=input$paid,
				labs = input$labs[1],
                ctrOff = ctrOff,
                min=0,
                max=100)
  })
   
})
