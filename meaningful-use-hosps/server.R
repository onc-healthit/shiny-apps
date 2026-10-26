# server.R
source("AkHiStPoly.R")
source("stMaps.R")
source("helpers.R")
hosp <- read.table("data/hosp-demo-meaningful-use-geo.txt",header=T,sep=",",colClasses="character")
hosp[,5]<-as.numeric(hosp[,5])
hosp[,6]<-as.numeric(hosp[,6])
rates<-read.table("data/hosp-demo-meaningful-use.txt",header=T,sep=",",colClasses="character")
rates[,3]<-as.numeric(rates[,3])
rates[,4]<-as.numeric(rates[,4])
rates[,5]<-as.numeric(rates[,5])
rates[,6]<-as.numeric(rates[,6])
rates[,7]<-as.numeric(rates[,7])
rates[,8]<-as.numeric(rates[,8])
rates[,9]<-as.numeric(rates[,9])
rates[,10]<-as.numeric(rates[,10])
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])

shinyServer(
  function(input, output) {
  output$map<-renderPlot({
    area<-switch(input$state,
                 "National" = "US",
                 "Alabama" = "Alabama",
                 "Alaska" = "Alaska",
                 "Arizona" = "Arizona",
                 "Arkansas" = "Arkansas",
                 "California" = "California",
                 "Colorado" = "Colorado",
                 "Connecticut" = "Connecticut",
                 "Delaware" = "Delaware",
                 "District Of Columbia" = "District Of Columbia",
                 "Florida" = "Florida",
                 "Georgia" = "Georgia",
                 "Hawaii" = "Hawaii",
                 "Idaho" = "Idaho",
                 "Illinois" = "Illinois",
                 "Indiana" = "Indiana",
                 "Iowa" = "Iowa",
                 "Kansas" = "Kansas",
                 "Kentucky" = "Kentucky",
                 "Louisiana" = "Louisiana",
                 "Maine" = "Maine",
                 "Maryland" = "Maryland",
                 "Massachusetts" = "Massachusetts",
                 "Michigan" = "Michigan",
                 "Minnesota" = "Minnesota",
                 "Mississippi" = "Mississippi",
                 "Missouri" = "Missouri",
                 "Montana" = "Montana",
                 "Nebraska" = "Nebraska",
                 "Nevada" = "Nevada",
                 "New Hampshire" = "New Hampshire",
                 "New Jersey" = "New Jersey",
                 "New Mexico" = "New Mexico",
                 "New York" = "New York",
                 "North Carolina" = "North Carolina",
                 "North Dakota" = "North Dakota",
                 "Ohio" = "Ohio",
                 "Oklahoma" = "Oklahoma",
                 "Oregon" = "Oregon",
                 "Pennsylvania" = "Pennsylvania",
                 "Rhode Island" = "Rhode Island",
                 "South Carolina" = "South Carolina",
                 "South Dakota" = "South Dakota",
                 "Tennessee" = "Tennessee",
                 "Texas" = "Texas",
                 "Utah" = "Utah",
                 "Vermont" = "Vermont",
                 "Virginia" = "Virginia",
                 "Washington" = "Washington",
                 "West Virginia" = "West Virginia",
                 "Wisconsin" = "Wisconsin",
                 "Wyoming" = "Wyoming"
    )
     
    percent_map(data = rates,
                data2 = hosp,
                ctrOff = ctrOff,
                labs = input$labs[1],
                state = area,
                lUrban=input$lUrban[1],
                sUrban=input$sUrban[1],
                lRural=input$lRural[1],
                sRural=input$sRural[1],
                cah=input$cah[1],
                min = input$range[1], 
                max = input$range[2]
                )
  })
   
})
