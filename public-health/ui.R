# ui.R
require(shiny)
#install.packages("RColorBrewer",repos='http://cran.rstudio.com/')
#require(RColorBrewer)
shinyUI(fixedPage(theme="readable.css",
        title="Hospital Public Health Measure Selection, 2014",
        wellPanel(
          tags$head(
            tags$style(type="text/css", ".selectize-input.items.full.has-options.has-items {width: 675px;height:40px; }"),
            tags$style(type="text/css", ".well {width: 1125px;margin-top:10px;margin-bottom:5px;padding-top:5px;padding-bottom:5px;margin-left:0px;margin-right:0px;padding-left:0px;padding-right:0px;}")
          ),
          fixedRow(
                          
                    column(7,selectInput("var",
                                         label=h6("Choose a Measure"),
                                         choices=c("Percent of Hospitals Reporting Without Exclusion on the Electronic Laboratory Reporting Measure",
                                                   "Percent of Hospitals Reporting Without Exclusion on the Syndromic Surveillance Measure",
                                                   "Percent of Hospitals Reporting Without Exclusion on the Immunization Measure",
                                                   "Percent of Hospitals Reporting Without Exclusion on at Least One Measure",
                                                   "Percent of Stage 2 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures",
                                                   "Percent of Stage 1 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures"),
                                         selected="Percent of Stage 2 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures")),
          
                    column(2,sliderInput("range",
                               label=h6("Choose a percentage range"),
                               min = 0, max = 100, value=c(0,100))),
                    
                    column(2,tags$div(style="font-size:14px;font-family:Raleway;",checkboxInput("labs",
                                           label="Click to display % labels",value=FALSE)))
          )),
                               
          fixedRow(
           plotOutput("map",width="100%",height="500px")
)))