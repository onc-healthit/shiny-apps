# ui.R

#require(shiny)
library(RColorBrewer)
shinyUI(fixedPage(theme="readable.css",
        title="Hospital Health IT Adoption and Use, 2020",
        wellPanel(
          tags$head(
            tags$style(type="text/css", ".selectize-input.items.full.has-options.has-items {width: 100%;height:35px; }"),
            tags$style(type="text/css", ".well {width: 95%;margin-top:10px;margin-bottom:5px;padding-top:5px;padding-bottom:5px;}")
          ),
          fixedRow(
                    column(3,tags$div(style="font-size:14px;font-family:Raleway;",radioButtons("input_type",
                            label=h6("Choose a Measure Area"),
                            choices=c("EHR Adoption","Interoperability","Patient Access","Rx"),
                            selected="EHR Adoption"))),
                          
                    column(7,uiOutput("ui")), 
					
                    column(3,sliderInput("range",
                               label=h6("Choose a percentage range"),
                               min = 0, max = 100, value=c(0,100))),
                    
                    column(3,tags$div(style="font-size:14px;font-family:Raleway;",checkboxInput("labs",
                                           label="Click to display % labels",value=FALSE)))
          )),
                               
          fixedRow(
           plotOutput("map",width="100%",height="100%")
)))