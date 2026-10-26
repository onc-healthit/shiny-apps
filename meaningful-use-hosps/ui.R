# ui.R
require(shiny)
require(RColorBrewer)
shinyUI(fixedPage(theme="readable.css",
        title="Hospitals Paid by Medicare for Meaningful Use",
        wellPanel(
          tags$head(
            tags$style(type="text/css", ".well {width: 100%;margin-top:10px;margin-bottom:5px;padding-top:5px;padding-bottom:5px;}")
            ),
          fixedRow(
            column(3,h6("View state reports")),
            column(4,h6("Add hospital data points to map")),
            column(3,h6("Choose a percentage range"))),
          fixedRow(       
            column(3,selectInput("state",
                        label="State",
                        choices=c("National",
                                  "Alabama",
                                  "Alaska",
                                  "Arizona",
                                  "Arkansas",
                                  "California",
                                  "Colorado",
                                  "Connecticut",
                                  "Delaware",
                                  "District Of Columbia",
                                  "Florida",
                                  "Georgia",
                                  "Hawaii",
                                  "Idaho",
                                  "Illinois",
                                  "Indiana",
                                  "Iowa",
                                  "Kansas",
                                  "Kentucky",
                                  "Louisiana",
                                  "Maine",
                                  "Maryland",
                                  "Massachusetts",
                                  "Michigan",
                                  "Minnesota",
                                  "Mississippi",
                                  "Missouri",
                                  "Montana",
                                  "Nebraska",
                                  "Nevada",
                                  "New Hampshire",
                                  "New Jersey",
                                  "New Mexico",
                                  "New York",
                                  "North Carolina",
                                  "North Dakota",
                                  "Ohio",
                                  "Oklahoma",
                                  "Oregon",
                                  "Pennsylvania",
                                  "Rhode Island",
                                  "South Carolina",
                                  "South Dakota",
                                  "Tennessee",
                                  "Texas",
                                  "Utah",
                                  "Vermont",
                                  "Virginia",
                                  "Washington",
                                  "West Virginia",
                                  "Wisconsin",
                                  "Wyoming"),
                        selected = "National")),
            
          column(2,
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("lUrban",label="Urban (>50 beds)",value=FALSE)),
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("lRural",label="Rural (>100 beds)",value=FALSE)),
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("cah",label="Critical Access ",value=FALSE))),
          
          column(2,
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("sUrban",label="Small Urban (<50 beds)",value=FALSE)),
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("sRural",label="Small Rural (<100 beds)",value=FALSE))),
                 
          column(3,
                 sliderInput("range",label=NA,min = 0, max = 100, value=c(95,100))),
          column(3,
                 tags$div(style="font-size:12px;font-family:Raleway;",checkboxInput("labs",label="Click to display % labels (national only)",value=FALSE))
                 ))
          ),
        
          fixedRow(
            plotOutput("map",width="100%",height="550px")
                  )
))
