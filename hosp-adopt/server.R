# server.R
source("AkHiStPoly.R")
source("stMaps.R")
source("helpers.R")
aha<-read.csv("data/aha.csv",colClasses="character") ## update with name of newest dataset
aha<-aha[aha$period=="2020",] ## update with correct period
aha<-aha[order(aha[,1],decreasing=F),]
aha[,1]<-NULL
geo<-read.table("data/state-geo-data.txt",sep=",",header=T,colClasses="character")
#aha$fips<-geo$Fips[match(aha$region_code,geo$Code)]
aha$longitude<-geo$longitude[match(aha$region_code,geo$Code)]
aha$latitude<-geo$latitude[match(aha$region_code,geo$Code)]
#aha$fips<-as.numeric(aha$fips)
#aha<-aha[order(aha$fips,decreasing=F),]
## adoption cehrt
aha$pct_hospitals_cehrt<-round(as.numeric(aha$pct_hospitals_cehrt)*100,0) ## cert
aha$pct_hospitals_cehrt_2015<-round(as.numeric(aha$pct_hospitals_cehrt_2015)*100,0) ## 2015 cert
## interoperability
aha$pct_hospitals_find_clinical_info<-round(as.numeric(aha$pct_hospitals_find_clinical_info)*100,0) ## find
aha$pct_hospitals_send_clinical_info<-round(as.numeric(aha$pct_hospitals_send_clinical_info)*100,0) ## send
aha$pct_hospitals_receive_clinical_info<-round(as.numeric(aha$pct_hospitals_receive_clinical_info)*100,0) ## receive
aha$pct_hospitals_integrate_scr<-round(as.numeric(aha$pct_hospitals_integrate_scr)*100,0) ## integrate
aha$pct_hospitals_send_receive_find_integrate<-round(as.numeric(aha$pct_hospitals_send_receive_find_integrate)*100,0) ## srfi
## access
aha$pct_hospitals_api<-round(as.numeric(aha$pct_hospitals_api)*100,0) ## api
aha$pct_hospitals_patients_view<-round(as.numeric(aha$pct_hospitals_patients_view)*100,0) ## view
aha$pct_hospitals_patients_download<-round(as.numeric(aha$pct_hospitals_patients_download)*100,0) ## download
aha$pct_hospitals_patients_transmit<-round(as.numeric(aha$pct_hospitals_patients_transmit)*100,0) ## transmit
aha$pct_hospitals_patients_vdt<-round(as.numeric(aha$pct_hospitals_patients_vdt)*100,0) ## vdt
## rx
aha$pct_hospitals_epcs<-round(as.numeric(aha$pct_hospitals_epcs)*100,0) ## epcs
aha$longitude<-as.numeric(aha$longitude)
aha$latitude<-as.numeric(aha$latitude)
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])

shinyServer(
  function(input, output) {
    
  output$ui <- renderUI({
  if (is.null(input$input_type))
      return()
      
  switch(input$input_type,
             "EHR Adoption" = selectInput("var",
                              label=h6("Choose an EHR Adoption Measure"),
                              choices=c("% of all Hospitals that have Adopted a Certified EHR",
                                        "% of all Hospitals that have Adopted a 2015 Edition Certified EHR"
                                        ),
                              selected="% of all Hospitals that have Adopted a 2015 Edition Certified EHR"),
             "Interoperability" = selectInput("var",
                                              label=h6("Choose an Interoperability Measure"),
                                              choices=c("% of Hospitals that Electronically Find Patient Health Information from Outside Providers",
                                                        "% of Hospitals that Electronically Send Patient Summary of Care Records to Outside Providers",
                                                        "% of Hospitals that Electronically Receive Patient Summary of Care Records from Outside Providers",
                                                        "% of Hospitals that Electronically Integrate Patient Summary of Care Records from Outside Providers",
                                                        "% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers"
                                                        ),
                                              selected="% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers"),
             "Patient Access"=selectInput("var",
                                  label=h6("Choose a Patient Access Measure"),
                                  choices=c("% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)",
                                            "% of Hospitals with Capability for Patients to View their Health Information Electronically",
                                            "% of Hospitals with Capability for Patients to Download their Health Information Electronically",
                                            "% of Hospitals with Capability for Patients to Transmit their Health Information Electronically",
                                            "% of Hospitals with Capability for Patients to View, Download, and Transmit their Health Information Electronically"),
                                  selected="% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)"),
              "Rx"=selectInput("var",
                               label=h6("Choose a Rx Measure"),
                               choices=c("% of Hospitals that Can Electronically Prescribe Controlled Substances"),
                               selected="% of Hospitals that Can Electronically Prescribe Controlled Substances")
      )
    })
  
  output$map<-renderPlot(height=500,{
    if (is.null(input$var))
      return()
    measures<-switch(input$var,
      "% of all Hospitals that have Adopted a Certified EHR" = 3,
      "% of all Hospitals that have Adopted a 2015 Edition Certified EHR" = 4,
      "% of Hospitals that Electronically Find Patient Health Information from Outside Providers" = 5,
      "% of Hospitals that Electronically Send Patient Summary of Care Records to Outside Providers" = 6,
      "% of Hospitals that Electronically Receive Patient Summary of Care Records from Outside Providers" = 7,
      "% of Hospitals that Electronically Integrate Patient Summary of Care Records from Outside Providers" = 8,
      "% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers" =9,
      "% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)" = 14,
      "% of Hospitals with Capability for Patients to View their Health Information Electronically" = 18,
      "% of Hospitals with Capability for Patients to Download their Health Information Electronically" = 19,
      "% of Hospitals with Capability for Patients to Transmit their Health Information Electronically" = 20,
      "% of Hospitals with Capability for Patients to View, Download, and Transmit their Health Information Electronically" = 21,
      "% of Hospitals that Can Electronically Prescribe Controlled Substances" = 17
    )
    
    color<-switch(input$var,
                  "% of all Hospitals that have Adopted a Certified EHR" = "Greens",
                  "% of all Hospitals that have Adopted a 2015 Edition Certified EHR" = "Greens",
                  "% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)" = "Purples",
                  "% of Hospitals with Capability for Patients to View their Health Information Electronically" = "Purples",
                  "% of Hospitals with Capability for Patients to Download their Health Information Electronically" = "Purples",
                  "% of Hospitals with Capability for Patients to Transmit their Health Information Electronically" = "Purples",
                  "% of Hospitals with Capability for Patients to View, Download, and Transmit their Health Information Electronically" = "Purples",
                  "% of Hospitals that Electronically Find Patient Health Information from Outside Providers" = "Blues",
                  "% of Hospitals that Electronically Send Patient Summary of Care Records to Outside Providers" = "Blues",
                  "% of Hospitals that Electronically Receive Patient Summary of Care Records from Outside Providers" = "Blues",
                  "% of Hospitals that Electronically Integrate Patient Summary of Care Records from Outside Providers" = "Blues",
                  "% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers" = "Blues",
                  "% of Hospitals that Can Electronically Prescribe Controlled Substances" = "Reds"
    )
    
    legend<-switch(input$var,
                   "% of all Hospitals that have Adopted a Certified EHR" = "% of all Hospitals that have Adopted a Certified EHR",
                   "% of all Hospitals that have Adopted a 2015 Edition Certified EHR" = "% of all Hospitals that have Adopted a 2015 Edition Certified EHR",
                   "% of Hospitals with Capability for Patients to View their Health Information Electronically" = "% of Hospitals with Capability for Patients to View their Health Information Electronically",
                   "% of Hospitals with Capability for Patients to Download their Health Information Electronically" = "% of Hospitals with Capability for Patients to Download their Health Information Electronically",
                   "% of Hospitals with Capability for Patients to Transmit their Health Information Electronically" = "% of Hospitals with Capability for Patients to Transmit their Health Information Electronically",
                   "% of Hospitals with Capability for Patients to View, Download, and Transmit their Health Information Electronically" = "% of Hospitals with Capability for Patients to View, Download, and Transmit their Health Information Electronically",
                   "% of Hospitals that Electronically Find Patient Health Information from Outside Providers" = "% of Hospitals that Electronically Find Patient Health Information from Outside Providers",
                   "% of Hospitals that Electronically Send Patient Summary of Care Records to Outside Providers" = "% of Hospitals that Electronically Send Patient Summary of Care Records to Outside Providers",
                   "% of Hospitals that Electronically Receive Patient Summary of Care Records from Outside Providers" = "% of Hospitals that Electronically Receive Patient Summary of Care Records from Outside Providers",
                   "% of Hospitals that Electronically Integrate Patient Summary of Care Records from Outside Providers" = "% of Hospitals that Electronically Integrate Patient Summary of Care Records from Outside Providers",
                   "% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers" = "% of Hospitals that Electronically Send, Receive, Find and Integrate Patient Information from Outside Providers",
                   "% of Hospitals that Can Electronically Prescribe Controlled Substances" = "% of Hospitals that Can Access State Prescription Drug Monitoring Program (PDMP) within EHR",
                   "% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)" = "% of Hospitals with Capability for Patients to Access their Health Information using an Application Programming Interface (API)"
    )
    
    percent_map(data = aha,
                var = measures, 
                color = color, 
                legend.title = legend,
                labs = input$labs[1],
                ctrOff = ctrOff,
                min = input$range[1], 
                max = input$range[2])
  })
   
})
