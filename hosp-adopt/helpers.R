percent_map <- function(data,var, color, legend.title, labs=FALSE, ctrOff, min = 0, max = 100) {
  
  nat<-data[data[,1]=="US",][,var]
  data<-data[data[,1] !="US",]
  rownames(data)<-c(1:nrow(data))
  var<-data[,var]
  
  if (min !=0 & max !=100) {
    vec<-c(0,min-1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(2/4))),0),round((min+((max-min)*(3/4))),0),max,100)
  }
  else if (min !=0) {
    vec<-c(0,min-1,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(2/4))),0),round((min+((max-min)*(3/4))),0),max)
  }
  else if (max !=100) {
    vec<-c(min,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(2/4))),0),round((min+((max-min)*(3/4))),0),max,100)
  }
  else {
    vec<-c(0,round((min+((max-min)*(1/4))),0),round((min+((max-min)*(2/4))),0),round((min+((max-min)*(3/4))),0),100)
  }
  
  percents<-as.numeric(cut(var,vec))
  if(length(percents[is.na(percents)])>0){
    percents[is.na(percents)]<-0
    percents<-percents+1
    shades<-c("#f0f0f0",brewer.pal(length(vec)-1,color))
  }
  else{
    shades<-brewer.pal(length(vec)-1,color)
  }
  
  ##shades<-shades[1:length(shades)]
  
  # constrain gradient to percents that occur between min and max
  # var <- pmax(var, min)
  # var <- pmin(var, max)
  # percents <- as.integer(cut(var, 100, 
  #   include.lowest = TRUE, ordered = TRUE))
  
  state<-percents[match(state.fips$abb,data[,1])]
  fills <- shades[state]
  #fills[is.na(fills)]<-"gray"
  
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
    
    # rural note
    if(is.na(sum(var))){
      #mtext("*State estimate is not reliable",outer=T,line=-10.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("have no rural providers, or",outer=T,line=-9,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("too few rural providers were",outer=T,line=-7.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("captured in the survey to",outer=T,line=-6,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("make a confident estimate",outer=T,line=-4.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
    }
    else {
    }
    
    # code for labels
    ctrCont<-data[data[,1] !="DC",]
    ctrCont<-ctrCont[ctrCont[,1] !="MD",]
    ctrCont<-ctrCont[ctrCont[,1] !="DE",]
    ctrCont<-ctrCont[ctrCont[,1] !="NJ",]
    ctrCont<-ctrCont[ctrCont[,1] !="CT",]
    ctrCont<-ctrCont[ctrCont[,1] !="RI",]
    ctrCont<-ctrCont[ctrCont[,1] !="MA",]
    ctrCont<-ctrCont[ctrCont[,1] !="VT",]
    ctrCont<-ctrCont[ctrCont[,1] !="NH",]
    segs<-rbind(
      data[data[,1]=="DC",],
      data[data[,1]=="MD",],
      data[data[,1]=="DE",],
      data[data[,1]=="NJ",],
      data[data[,1]=="CT",],
      data[data[,1]=="RI",],
      data[data[,1]=="MA",],
      data[data[,1]=="VT",],
      data[data[,1]=="NH",])
    segs<-segs[order(segs[,1],decreasing=F),]
    stLabs<-function(long,lat,lab,col,font,cex,min,max){
      if(is.na(lab)){
        label<-text(long,lat,"NR",col="dimgrey",cex=cex,font=font)
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
        label<-text(long,lat,"NR",col=col,cex=cex,font=font,pos=pos,offset=offset)
      }
      else {
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
      }
      return(label)
    }
    dataVar<-data.frame(var)
    mapply(stLabs,long=ctrCont[,42],lat=ctrCont[,43],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
    mapply(stLabs1,long=segs[1,42],lat=segs[1,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
    mapply(stLabs1,long=segs[2,42],lat=segs[2,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
    mapply(stLabs1,long=segs[3,42],lat=segs[3,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
    mapply(stLabs1,long=segs[4,42],lat=segs[4,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
    mapply(stLabs1,long=segs[5,42],lat=segs[5,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
    mapply(stLabs1,long=segs[6,42],lat=segs[6,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
    mapply(stLabs1,long=segs[7,42],lat=segs[7,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
    mapply(stLabs1,long=segs[8,42],lat=segs[8,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
    mapply(stLabs1,long=segs[9,42],lat=segs[9,43],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT
    ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
    ptlines<-function(long1,lat1,long2,lat2){
      ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="dimgrey")
      return(ptlines)
    }
    mapply(ptlines,long1=segs[,42],lat1=segs[,43],long2=ctrOff[,3],lat2=ctrOff[,2])
    
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
    
    # rural note
    if(is.na(sum(var))){
      #mtext("State estimate is not reliable",outer=T,line=-10.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("have no rural providers, or",outer=T,line=-9,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("too few rural providers were",outer=T,line=-7.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("captured in the survey to",outer=T,line=-6,side=1,adj=1,font=2,cex=1,col="dimgrey")
      #mtext("make a confident estimate",outer=T,line=-4.5,side=1,adj=1,font=2,cex=1,col="dimgrey")
    }
    else {
    }
    
  }
  
  # alaska plot
  par(mar=c(8,0,0,0))
  
  if(labs==TRUE){
    plot(st_geometry(ak),
         col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],
         border="lightgray",asp=2.5)
    dataVar<-data.frame(var)
    if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])){
      text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,"NR",col="dimgrey",cex=1.5,font=2)
    }
    else if(min==0 & max==100){
      if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<=25){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else{
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }
    else if(min !=0){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<min){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else{
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }  
    else if (max!=100){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/2)))){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else{
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }  
    else if (min !=0 & max !=100){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<min){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else{
        text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }    
    else {
      text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
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
    text(data[data[,1]=="HI",]$longitude,data[data[,1]=="HI",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="HI",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
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
    if(length(var[is.na(var)])>0){
      legend.txt<-c(
        "Not reliable",
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min + inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      )
    }
    else{
      legend.txt<-c(
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min + inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      )
    }
  }
  else if(min !=0){
    if(length(var[is.na(var)])>0){
      legend.txt<-c(
        "Not reliable",
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%")
      )
    }
    else{
      legend.txt<-c(
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%")
      )
    }
    
  }
  else if (max !=100){
    if(length(var[is.na(var)])>0){
      legend.txt<-c( 
        "Not reliable",
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      ) 
    }
    else{
      legend.txt<-c(
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      ) 
    }
    
    
  }
  else {
    if(length(var[is.na(var)])>0){
      legend.txt<-c( 
        "Not reliable",
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%")
      )
    }
    else{
      legend.txt<-c(
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",max,"%")
      )
    }
    
  }
  
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0.25))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=2,horiz=T,
         title = paste(legend.title,paste("National Avg =",paste(nat,"%",sep=""),sep=" "),sep=" | "),
         cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0.25))
  
  mtext("Source: 2020 American Hospital Association Survey (AHA)",outer=T,side=1,line=-2,adj=0,cex=0.90,col="dimgrey")
  
  #mtext(paste("National Average =",paste(var[as.numeric(rownames(data[data[,1]=="National",]))],"%",sep=""),sep=" "),outer=T,side=3,line=-7,adj=0,cex=1,font=2,col="dimgrey")  
  
  
  
  #box(bty="]",col="darkgray")
  #mtext(paste("% of Physicians Practicing, by County, in a Group of",legend.title,"Physicians",sep=" "),outer=T,line=-1.5,side=3,adj=0,font=2,cex=1.5,col="dimgrey")
}