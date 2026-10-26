# ui.R
require(shiny)
#require(RColorBrewer)
shinyUI(fixedPage(theme="readable.css",
        title="MU Paid",
        wellPanel(
          tags$head(
            tags$style(type="text/css", ".selectize-input.items.full.has-options.has-items {width: 675px;height:35px; }"),
            tags$style(type="text/css", ".well {width: 1100px;margin-top:10px;margin-bottom:5px;padding-top:5px;padding-bottom:5px;}")
          ),
        fixedRow(
        column(4,tags$div(style="font-size:12px;font-family:Raleway;",
                          radioButtons("type",
                          label=h6("Hospital Type"),
                          choices=c("All Eligible and Critical Access Hospitals","Critical Access and Small Rural Hospitals"),
                          selected="All Eligible and Critical Access Hospitals"))),
		column(4,tags$div(style="font-size:12px;font-family:Raleway;",
                          radioButtons("paid",
                          label=h6("Program Participation"),
                          choices=c("Demonstrated Meaningful Use of Certified Health IT","Demonstrated Meaningful Use and/or Adopted Implemented, or Upgraded any EHR"),
                          selected="Demonstrated Meaningful Use of Certified Health IT"))),			  
          
          column(2,
          tags$div(style="font-size:14px;font-family:Raleway;",checkboxInput("labs",label="Click to display % labels",value=FALSE))
          ))
        ),
        fixedRow(
          plotOutput("map",width="95%",height="500px"))
))