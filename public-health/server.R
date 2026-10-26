# server.R
source("AkHiStPoly.R")
source("stMaps.R")
source("helpers.R")
phm<- read.table("data/phm-2014.txt",sep=",",header=T,colClasses="character")
phm[,2]<-as.numeric(phm[,2])
phm[,3]<-as.numeric(phm[,3])
phm[,4]<-as.numeric(phm[,4])
phm[,5]<-as.numeric(phm[,5])
phm[,6]<-as.numeric(phm[,6])
phm[,7]<-as.numeric(phm[,7])
phm[,8]<-as.numeric(phm[,8])
phm[,9]<-as.numeric(phm[,9])
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])

shinyServer(
  function(input, output) {
  
  output$map<-renderPlot(height=500,{
    measures<-switch(input$var,
                     "Percent of Hospitals Reporting Without Exclusion on the Electronic Laboratory Reporting Measure" = phm[,2],
                     "Percent of Hospitals Reporting Without Exclusion on the Syndromic Surveillance Measure" = phm[,3],
                     "Percent of Hospitals Reporting Without Exclusion on the Immunization Measure" = phm[,4],
                     "Percent of Hospitals Reporting Without Exclusion on at Least One Measure" = phm[,5],
                     "Percent of Stage 2 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures" = phm[,6],
                     "Percent of Stage 1 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures" = phm[,7]
    )
    
    percent_map(data = phm,
                var = measures, 
                legend.title = input$var, 
                labs = input$labs[1],
                ctrOff = ctrOff,
                min = input$range[1], 
                max = input$range[2])
  })
   
})
