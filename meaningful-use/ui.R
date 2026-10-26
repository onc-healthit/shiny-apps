# ui.R

#require(shiny)
library(RColorBrewer)
shinyUI(fixedPage(theme="readable.css",
        title="Office-based Physicians Paid by Medicare for Meaningful Use",
        wellPanel(
          tags$head(
            tags$style(type="text/css", ".well {width: 100%;margin-top:10px;margin-bottom:5px;padding-top:5px;padding-bottom:5px;}")
            ),
          fixedRow(
            column(2),
            column(4,
            selectInput("state",
                        label=h6("View Individual State Reports"),
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
                        selected = "National")
            ),
            column(3,sliderInput("range",
                                 label=h6("Choose a Percentage Range"),
                                 min = 0, max = 100, value=c(0,100)))
            
            )),
        
          fixedRow(
            plotOutput("map",width="100%",height="550px")
                  )
))
