percent_map <- function(data,var,legend.title,labs=FALSE,ctrOff,min = 0,max = 100) {
  #options(stringsAsFactors=FALSE)
  
  if (min !=0 & max !=100) {
    vec<-c(0,0.1,min-1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(1/2))),0),round((min+((max-min)*(3/4))),0),max,100)
  }
  else if (min !=0) {
    vec<-c(0,0.1,min-1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(1/2))),0),round((min+((max-min)*(3/4))),0),max)
  }
  else if (max !=100) {
    vec<-c(min,0.1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(1/2))),0),round((min+((max-min)*(3/4))),0),max,100)
  }
  else {
    vec<-c(min,0.1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(1/2))),0),round((min+((max-min)*(3/4))),0),max)
  }
  
  if(length(vec)==6){
    shades<-c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")
  }
  else if(length(vec)==7 & min !=0){
    shades<-c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  }
  else if(length(vec)==7 & max !=100){
    shades<-c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  }
  else if(length(vec)==8){
    shades<-c("#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }
  
  var[var==0]<-0.05
  percents<-as.numeric(cut(var,vec))
  #percents<-percents+1
  #percents[is.na(percents)]<-1
  
  state<-percents[match(state.fips$state,data[,1])]
  fills <- shades[state]
  
  var[var==0.05]<-0
  
  # refresh map
  layout(matrix(c(4,2,2,3,3,4,1,1,1,1,4,1,1,1,1), 5), c(1,3), c(1,3))
  par(mar=c(0,0,0,0))
  par(oma=c(0,0,0,0))
  
  if(labs==TRUE){
  
  map("state",col="white",fill=F,
     lty = 1, lwd = 1, mar=c(0,0,0,0),
     myborder = 0)
  
  # plot choropleth map
  par(mar=c(0,0,0,0))
  
  stData(state,shades)
  
  # overlay state borders
  map("state", col = "gray", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, 
    myborder = 0,mar=c(0,0,0,0),resolution=0)
  
  # code for labels
  ctrCont<-data[data[,1] !="District Of Columbia",]
  ctrCont<-ctrCont[ctrCont[,1] !="Maryland",]
  ctrCont<-ctrCont[ctrCont[,1] !="Delaware",]
  ctrCont<-ctrCont[ctrCont[,1] !="New Jersey",]
  ctrCont<-ctrCont[ctrCont[,1] !="Connecticut",]
  ctrCont<-ctrCont[ctrCont[,1] !="Rhode Island",]
  ctrCont<-ctrCont[ctrCont[,1] !="Massachusetts",]
  ctrCont<-ctrCont[ctrCont[,1] !="Vermont",]
  ctrCont<-ctrCont[ctrCont[,1] !="New Hampshire",]
  segs<-rbind(
    data[data[,1]=="District Of Columbia",],
    data[data[,1]=="Maryland",],
    data[data[,1]=="Delaware",],
    data[data[,1]=="New Jersey",],
    data[data[,1]=="Connecticut",],
    data[data[,1]=="Rhode Island",],
    data[data[,1]=="Massachusetts",],
    data[data[,1]=="Vermont",],
    data[data[,1]=="New Hampshire",])
  segs<-segs[order(segs[,1],decreasing=F),]
  stLabs<-function(long,lat,lab,col,font,cex,min,max){
    if(is.na(lab)){
      label<-text(long,lat,"Not Applicable",col="dimgrey",cex=cex,font=font)
    }
    else if(min==0 & max==100){
      if (lab<=25){
      label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }
    else if(min !=0){
      if(lab<min){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if (lab<=(min+((max-min)*(1/4)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }  
    else if (max!=100){
      if(lab<=(min+((max-min)*(1/4)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if(lab<=(min+((max-min)*(1/2)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }  
    else if (min !=0 & max !=100){
      if(lab<min){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if(lab<=(min+((max-min)*(1/4)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }    
    else {
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
    }  
    return(label)
  }
  stLabs1<-function(long,lat,lab,col,font,cex,pos,offset){
    if(is.na(lab)){
      label<-text(long,lat,"Not Applicable",col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    else {
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    return(label)
  }
  dataVar<-data.frame(var)
  mapply(stLabs,long=ctrCont[,8],lat=ctrCont[,9],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
  mapply(stLabs1,long=segs[1,8],lat=segs[1,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
  mapply(stLabs1,long=segs[2,8],lat=segs[2,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
  mapply(stLabs1,long=segs[3,8],lat=segs[3,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
  mapply(stLabs1,long=segs[4,8],lat=segs[4,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
  mapply(stLabs1,long=segs[5,8],lat=segs[5,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
  mapply(stLabs1,long=segs[6,8],lat=segs[6,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
  mapply(stLabs1,long=segs[7,8],lat=segs[7,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
  mapply(stLabs1,long=segs[8,8],lat=segs[8,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
  mapply(stLabs1,long=segs[9,8],lat=segs[9,9],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT
  ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
  ptlines<-function(long1,lat1,long2,lat2){
    ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="dimgrey")
    return(ptlines)
  }
  mapply(ptlines,long1=segs[,8],lat1=segs[,9],long2=ctrOff[,3],lat2=ctrOff[,2])
  
  }
  
  # code for no labels
  else {
    map("state",col="white",fill=F,
        lty = 1, lwd = 1, mar=c(0,0,0,0),
        myborder = 0)
    
    # plot choropleth map
    par(mar=c(0,0,0,0))
    
    stData(state,shades)
    
    # overlay state borders
    map("state", col = "gray", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, 
        myborder = 0,mar=c(0,0,0,0),resolution=0)
    
  }
  
  # alaska plot
  par(mar=c(8,0,0,0))
  
  if(labs==TRUE){
  plot(st_geometry(ak),
        col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],
        border="lightgray",asp=2.5)
  dataVar<-data.frame(var)
  if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])){
    text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,"N/A",col="dimgrey",cex=1.5,font=2)
  }
  else if(min==0 & max==100){
    if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=25){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }
  else if(min !=0){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<min){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }  
  else if (max!=100){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/2)))){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }  
  else if (min !=0 & max !=100){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<min){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }    
  else {
      text(data[data[,1]=="Alaska",]$longitude,data[data[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
  }
  }
  else{
  plot(st_geometry(ak),
       col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],
       border="lightgray",asp=2.5)
  }
  # hawaii plot
  par(mar=c(6,4,0,0))
  if(labs==TRUE){
  plot(st_geometry(hi),
       col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],
       border="lightgray",asp=1.5)
  dataVar<-data.frame(var)
  text(data[data[,1]=="Hawaii",]$longitude,data[data[,1]=="Hawaii",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="Hawaii",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
  }
  else {
  plot(st_geometry(hi),
        col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],
        border="lightgray",asp=1.5)
  }
  
  # false plot/legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  # add a legend
  inc <- (max - min) / 4
  
    if (min !=0 & max !=100) {
        legend.txt<-c(
          "0%",
          paste("Less than",paste(min, "%",sep=""),sep=" "),
          paste(min,"-",round((min + inc),0), "%"),
          paste(round((min + inc),0)+1,"-",round((min + 2 * inc),0), "%"),
          paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
          paste(round((min+3*inc),0)+1,"-",max,"%"),
          paste("More than",paste(max, "%",sep=""),sep=" ")
        )
    }
    else if(min !=0){
        legend.txt<-c(
          "0%",
          paste("Less than",paste(min, "%",sep=""),sep=" "),
          paste(min,"-",round((min + inc),0), "%"),
          paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
          paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
          paste(round((min+3*inc),0)+1,"-",max,"%")
        )
      }
    else if (max !=100){
        legend.txt<-c( 
          "0%",
          paste(min+1,"-",round((min + inc),0), "%"),
          paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
          paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
          paste(round((min+3*inc),0)+1,"-",max,"%"),
          paste("More than",paste(max, "%",sep=""),sep=" ")
        ) 
    }
    else {
        legend.txt<-c( 
          "0%",
          paste(min+1,"-",round((min + inc),0), "%"),
          paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
          paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
          paste(round((min+3*inc),0)+1,"-",max,"%")
        )
    }
    
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0.25))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=2,horiz=T,
         title = paste(legend.title,paste("National Average =",paste(var[as.numeric(rownames(data[data[,1]=="National",]))],"%",sep=""),sep=" "),sep=" | "),
         cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0.25))

  if(legend.title=="Percent of Hospitals Reporting Without Exclusion on the Immunization Measure"){
    mtext("Source: Medicare EHR Incentive Program Data through December, 2014",outer=T,side=1,line=-4.25,adj=0,cex=0.90,col="dimgrey")
    mtext("Note: Includes eligible hospitals reporting to the Medicare EHR Incentive Program for Fiscal Year 2014;",outer=T,side=1,line=-2.75,adj=0,cex=0.90,col="dimgrey")
    mtext("denominator includes only eligible hospitals that administer vaccinations that are reportable to the local public health agency. (N=3,855)",outer=T,side=1,line=-1.25,adj=0,cex=0.90,col="dimgrey")  
  }
  else if(legend.title=="Percent of Hospitals Reporting Without Exclusion on the Syndromic Surveillance Measure"){
    mtext("Source: Medicare EHR Incentive Program Data through December, 2014",outer=T,side=1,line=-4.5,adj=0,cex=0.90,col="dimgrey")
    mtext("Note: Includes eligible hospitals reporting to the Medicare EHR Incentive Program for Fiscal Year 2014;",outer=T,side=1,line=-3,adj=0,cex=0.90,col="dimgrey")
    mtext("denominator includes only eligible hospitals that offer urgent care or emergency services. (N=3,906)",outer=T,side=1,line=-1.5,adj=0,cex=0.90,col="dimgrey")  
  }
  else if(legend.title=="Percent of Stage 2 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures"){
    mtext("Source: Medicare EHR Incentive Program Data through December, 2014",outer=T,side=1,line=-4.5,adj=0,cex=0.90,col="dimgrey")
    mtext("Note: Includes eligible hospitals reporting to the Medicare EHR Incentive Program for Fiscal Year 2014;",outer=T,side=1,line=-3,adj=0,cex=0.90,col="dimgrey")
    mtext("denominator includes only eligible hospitals that attested to Stage 2 in 2014. (N=1,811)",outer=T,side=1,line=-1.5,adj=0,cex=0.90,col="dimgrey")  
  }
  else if(legend.title=="Percent of Stage 1 Hospitals Reporting Without Exclusion on all Applicable Public Health Measures"){
    mtext("Source: Medicare EHR Incentive Program Data through December, 2014",outer=T,side=1,line=-4.5,adj=0,cex=0.90,col="dimgrey")
    mtext("Note: Includes eligible hospitals reporting to the Medicare EHR Incentive Program for Fiscal Year 2014;",outer=T,side=1,line=-3,adj=0,cex=0.90,col="dimgrey")
    mtext("denominator includes only eligible hospitals that attested to Stage 1 in 2014. (N=2,158)",outer=T,side=1,line=-1.5,adj=0,cex=0.90,col="dimgrey")  
  }
  else{
    mtext("Source: Medicare EHR Incentive Program Data through December, 2014",outer=T,side=1,line=-3,adj=0,cex=0.90,col="dimgrey")
    mtext("Note: Includes eligible hospitals reporting to the Medicare EHR Incentive Program for Fiscal Year 2014. (N=3,969)",outer=T,side=1,line=-1.5,adj=0,cex=0.90,col="dimgrey")
  }
  
}