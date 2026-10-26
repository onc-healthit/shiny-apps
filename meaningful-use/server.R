# server.R
source("AkHiPoly.R")
source("ctyMaps.R")
source("helpers.R")
ep <- read.table("data/eps-data.txt",header=T,sep=",",colClasses="character")
ep[,3]<-as.numeric(ep[,3])
ep[,4]<-as.numeric(ep[,4])
ep[,5]<-as.numeric(ep[,5])

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
     
    
    
    percent_map(data = ep,
                state = area,
                color = "Blues",
                #legend.title = legend 
                min = input$range[1], 
                max = input$range[2]
                )
  })
   
})
