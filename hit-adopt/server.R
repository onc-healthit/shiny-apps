# server.R

source("AkHiStPoly.R")
source("stMaps.R")
source("helpers.R")
#nehrs<- read.table("data/NAMCS-2013.txt",header=T,sep=",",colClasses="character")
nehrs<-read.csv("data/nehrs.csv",colClasses="character")
nehrs<-nehrs[nehrs$period=="2019",]
nehrs<-nehrs[order(nehrs[,1],decreasing=F),]
nehrs[,1]<-NULL
geo<-read.table("data/state-geo-data.txt",sep=",",header=T,colClasses="character")
nehrs$longitude<-geo$longitude[match(nehrs$region_code,geo$Code)]
nehrs$latitude<-geo$latitude[match(nehrs$region_code,geo$Code)]
## ehr adoption
nehrs$pct_phys_any_ehr<-as.numeric(nehrs$pct_phys_any_ehr) ## any ehr / 3
nehrs$pct_phys_cert_ehr<-as.numeric(nehrs$pct_phys_cert_ehr) ## certified ehr / 4
## hie and interop
nehrs$pct_phys_send_receive_any_patient_info<-as.numeric(nehrs$pct_phys_send_receive_any_patient_info) ## send or receive any / 8
nehrs$pct_phys_find_clin_info<-as.numeric(nehrs$pct_phys_find_clin_info) ## find any / 12
nehrs$pct_phys_send_any_clin_info<-as.numeric(nehrs$pct_phys_send_any_clin_info) ## send any / 13
nehrs$pct_phys_send_summary_care_record<-as.numeric(nehrs$pct_phys_send_summary_care_record) ## send socr / 14
nehrs$pct_phys_receive_any_clin_info<-as.numeric(nehrs$pct_phys_receive_any_clin_info) ## receive any / 15
nehrs$pct_phys_receive_summary_care_record<-as.numeric(nehrs$pct_phys_receive_summary_care_record) ## receive socr / 16
nehrs$pct_phys_integrate_any_clin_info<-as.numeric(nehrs$pct_phys_integrate_any_clin_info) ## integrate any / 17
nehrs$pct_phys_integrate_summary_care_record<-as.numeric(nehrs$pct_phys_integrate_summary_care_record) ## integrate socr / 18
## rx
nehrs$pct_phys_erx<-as.numeric(nehrs$pct_phys_erx) ## erx / 25
nehrs$pct_phys_pdmp<-as.numeric(nehrs$pct_phys_pdmp) ## pdmp / 24
nehrs$pct_phys_epcs<-as.numeric(nehrs$pct_phys_epcs) ## epcs / 23
## pe
nehrs$pct_phys_vdt<-as.numeric(nehrs$pct_phys_vdt) ## vd or t / 10
nehrs$pct_phys_vd_and_t<-as.numeric(nehrs$pct_phys_vd_and_t) ## vd&t / 11
nehrs$pct_phys_patient_secure_message<-as.numeric(nehrs$pct_phys_patient_secure_message) ## secure messaging / 9
nehrs$longitude<-as.numeric(nehrs$longitude)
nehrs$latitude<-as.numeric(nehrs$latitude)
ctrOff<-read.table("data/state-centers-offset.txt",header=T,sep=",",colClasses="character")
ctrOff[,2]<-as.numeric(ctrOff[,2])
ctrOff[,3]<-as.numeric(ctrOff[,3])
##
shinyServer(
  function(input, output) {
    
    output$ui <- renderUI({
      if (is.na(input$input_type))
        return()
      
      switch(input$input_type,
             "EHR Adoption" = selectInput("var",
                                          label=h6("Choose an EHR Adoption Measure"),
                                          choices=c("% of all Physicians that have Adopted Any EHR",
													                          "% of all Physicians that have Adopted Certified EHRs"),
                                          selected="% of all Physicians that have Adopted Certified EHRs"),
             "Interoperability"= selectInput("var",
                                                        label=h6("Choose an Interoperability Measure"),
                                                        choices=c("% of Physicians that Electronically Send or Receive Patient Health Information with Any Other Providers",
														"% of Physicians with Capability to Search, Find, or Query Patient Health Information from Outside Sources",
														"% of Physicians that Send Any Patient Clinical Information to Outside Providers",
														"% of Physicians that Send Patient Summary of Care Records to Outside Providers",
														"% of Physicians that Receive Any Patient Clinical Information from Outside Providers",
														"% of Physicians that Receive Patient Summary of Care Records from Outside Providers",
														"% of Physicians with Capability to Integrate into an EHR Any Patient Clinical Information from Outside Providers",
														"% of Physicians with Capability to Integrate into an EHR Summary of Care Records from Outside Providers"),
                                                        selected="% of Physicians that Electronically Send or Receive Patient Health Information with Any Other Providers"),
             "Patient Access"=selectInput("var",
                                              label=h6("Choose a Patient Access Measure"),
                                              choices=c("% of Physicians with Capability to Exchange Secure Messages with Patients",
                                                        "% of Physicians with Capability to Allow their Patients to View, Download or Transmit their Health Information Electronically",
                                                        "% of Physicians with Capability to Allow their Patients to View, Download, and Transmit their Health Information Electronically"),
                                              selected="% of Physicians with Capability to Exchange Secure Messages with Patients"),
             "Rx"=selectInput("var",
                                              label=h6("Choose a Rx Measure"),
                                              choices=c("% of Physicians who Prescribe Controlled Substances and do so Electronically",
                                                        "% of Physicians who Electronically Prescribe",
                                                        "% of Physicians who Check State Prescription Drug Monitoring Program"),
                                              selected="% of Physicians who Prescribe Controlled Substances and do so Electronically")
      )
    })
    
    output$map<-renderPlot(height=500,{
      measures<-switch(input$var,
                    "% of all Physicians that have Adopted Any EHR"=3,
					"% of all Physicians that have Adopted Certified EHRs"=4,
					"% of Physicians that Electronically Send or Receive Patient Health Information with Any Other Providers"=8,
					"% of Physicians with Capability to Search, Find, or Query Patient Health Information from Outside Sources"=12,
					"% of Physicians that Send Any Patient Clinical Information to Outside Providers"=13,
					"% of Physicians that Send Patient Summary of Care Records to Outside Providers"=14,
					"% of Physicians that Receive Any Patient Clinical Information from Outside Providers"=15,
					"% of Physicians that Receive Patient Summary of Care Records from Outside Providers"=16,
					"% of Physicians with Capability to Integrate into an EHR Any Patient Clinical Information from Outside Providers"=17,
					"% of Physicians with Capability to Integrate into an EHR Summary of Care Records from Outside Providers"=18,
					"% of Physicians with Capability to Exchange Secure Messages with Patients"=9,
					"% of Physicians who Prescribe Controlled Substances and do so Electronically"=23,
					"% of Physicians who Electronically Prescribe"=25,
					"% of Physicians who Check State Prescription Drug Monitoring Program"=24,
					"% of Physicians with Capability to Allow their Patients to View, Download or Transmit their Health Information Electronically"=10,
					"% of Physicians with Capability to Allow their Patients to View, Download, and Transmit their Health Information Electronically"=11
      )
      
      color<-switch(input$var,
                    "% of all Physicians that have Adopted Any EHR"="Greens",
					"% of all Physicians that have Adopted Certified EHRs"="Greens",
					"% of Physicians that Electronically Send or Receive Patient Health Information with Any Other Providers"="Blues",
					"% of Physicians with Capability to Search, Find, or Query Patient Health Information from Outside Sources"="Blues",
					"% of Physicians that Send Any Patient Clinical Information to Outside Providers"="Blues",
					"% of Physicians that Send Patient Summary of Care Records to Outside Providers"="Blues",
					"% of Physicians that Receive Any Patient Clinical Information from Outside Providers"="Blues",
					"% of Physicians that Receive Patient Summary of Care Records from Outside Providers"="Blues",
					"% of Physicians with Capability to Integrate into an EHR Any Patient Clinical Information from Outside Providers"="Blues",
					"% of Physicians with Capability to Integrate into an EHR Summary of Care Records from Outside Providers"="Blues",
					"% of Physicians with Capability to Exchange Secure Messages with Patients"="Purples",
					"% of Physicians with Capability to Allow their Patients to View, Download or Transmit their Health Information Electronically"="Purples",
					"% of Physicians with Capability to Allow their Patients to View, Download, and Transmit their Health Information Electronically"="Purples",
					"% of Physicians who Prescribe Controlled Substances and do so Electronically"="Reds",
					"% of Physicians who Electronically Prescribe"="Reds",
					"% of Physicians who Check State Prescription Drug Monitoring Program"="Reds"
					
      )
      
      percent_map(data = nehrs,
                  var = measures, 
                  color = color, 
                  legend.title = input$var, 
                  labs = input$labs[1],
                  ctrOff = ctrOff,
                  min = input$range[1], 
                  max = input$range[2])
    })
    
  })
