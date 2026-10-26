percent_map <- function(data,data2,ctrOff,labs=FALSE,state="US",lUrban=FALSE,sUrban=FALSE,lRural=FALSE,sRural=FALSE,cah=FALSE,min=95,max=100) {
  ## for bar chart
  stPaid<-data.frame(
    state=data[order(data[,3],decreasing=F),][,1],
    paid=data[order(data[,3],decreasing=F),][,3],
    abb=data[order(data[,3],decreasing=F),][,2]
    )
  piePaid<-data.frame(
    state=data$state,
    abb=data$abb,
    paid=data$medicare_paid,
    urban=data$urban,
    sUrban=data$small_urban,
    rural=data$rural,
    sRural=data$small_rural,
    cah=data$cah
    )
  national<-0.95
  ##
  if (state !="US"){
    data<-data[data$state==state,]
    data2<-data2[data2$state==state,]
  }
  else{
  }
  var<-round((100*data[,3]),0)
  if (min==0 & max==100){
    vec<-c(0,20,40,60,80,100) #7
    shades<-c("#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  }
  else if(min==0 & max !=100){
    vec<-c(0,max*(1/5),max*(2/5),max*(3/5),max*(4/5),max,100) #8
    shades<-c("#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }
  else if(min !=0 & max==100){
    vec<-c(0,min-1,(min+((100-min)*(1/5))),(min+((100-min)*(2/5))),(min+((100-min)*(3/5))),(min+((100-min)*(4/5))),100) #8
    shades<-c("#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }
  else if(min !=0 & max !=100){
    vec<-c(0,min-1,(min+((100-min)*(1/5))),(min+((100-min)*(2/5))),(min+((100-min)*(3/5))),(min+((100-min)*(4/5))),max,100) #9
    shades<-c("#c7e9b4","#78c679","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }

  percents<-as.numeric(cut(var,vec))
  states<-percents[match(state.fips$abb,data[,2])]
  fills <- shades[states]
  
  ## for map legends
  inc <- (max - min) / 5
  if (min !=0 & max !=100) {
      legend.txt<-c(
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",round((min + 4 * inc),0),"%"),
        paste(round((min+4*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      )
  }
  else if(min !=0){
      legend.txt<-c(
        paste("Less than",paste(min, "%",sep=""),sep=" "),
        paste(min,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",round((min + 4 * inc),0),"%"),
        paste(round((min+4*inc),0)+1,"-",max,"%")
      )
  }
  else if (max !=100){
      legend.txt<-c(
        paste(1,"-",round((0 + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",round((min + 4 * inc),0),"%"),
        paste(round((min+4*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      )
  }
  else {
      legend.txt<-c("1-20%","21-40%","41-60%","61-80%","81-100%")
  }
  
  if (state=="US"){
  
  # refresh map
  layout(matrix(c(4,2,2,3,3,4,1,1,1,1,4,1,1,1,1), 5), c(1,3), c(1,3))
  par(mar=c(0,0,0,0))
  par(oma=c(0,1,0,0))
  if(labs==TRUE){
    map("state",col="white",fill=F,
        lty = 1, lwd = 1, mar=c(0,0,0,0),
        myborder = 0)
    
    # plot choropleth map
    par(mar=c(0,0,0,0))
    stData(states,shades)
    
    # overlay state borders
    map("state", col = "#bdbdbd", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, 
        myborder = 0,mar=c(0,0,0,0),resolution=0)
    
    # code for labels
    ctrCont<-data[data[,2] !="DC",]
    ctrCont<-ctrCont[ctrCont[,2] !="MD",]
    ctrCont<-ctrCont[ctrCont[,2] !="DE",]
    ctrCont<-ctrCont[ctrCont[,2] !="NJ",]
    ctrCont<-ctrCont[ctrCont[,2] !="CT",]
    ctrCont<-ctrCont[ctrCont[,2] !="RI",]
    ctrCont<-ctrCont[ctrCont[,2] !="MA",]
    ctrCont<-ctrCont[ctrCont[,2] !="VT",]
    ctrCont<-ctrCont[ctrCont[,2] !="NH",]
    segs<-rbind(
      data[data[,2]=="DC",],
      data[data[,2]=="MD",],
      data[data[,2]=="DE",],
      data[data[,2]=="NJ",],
      data[data[,2]=="CT",],
      data[data[,2]=="RI",],
      data[data[,2]=="MA",],
      data[data[,2]=="VT",],
      data[data[,2]=="NH",])
    segs<-segs[order(segs[,2],decreasing=F),]
    stLabs<-function(long,lat,lab,col,font,cex,min,max){
      if(is.na(lab)){
        label<-text(long,lat,"N/A",col="#525252",cex=cex,font=font)
      }
      else if(min==0 & max==100){
        if (lab<=25){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else{
          label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
        }
      }
      else if(min !=0){
        if(lab<min){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else if (lab<=(min+((max-min)*(1/4)))){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else{
          label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
        }
      }  
      else if (max!=100){
        if(lab<=(min+((max-min)*(1/4)))){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else if(lab<=(min+((max-min)*(1/2)))){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else{
          label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
        }
      }  
      else if (min !=0 & max !=100){
        if(lab<min){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
        }
        else if(lab<=(min+((max-min)*(1/4)))){
          label<-text(long,lat,paste(lab,"%",sep=""),col="#525252",cex=cex,font=font)
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
        label<-text(long,lat,"N/A",col=col,cex=cex,font=font,pos=pos,offset=offset)
      }
      else {
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
      }
      return(label)
    }
    dataVar<-data.frame(var)
    mapply(stLabs,long=ctrCont[,9],lat=ctrCont[,10],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
    mapply(stLabs1,long=segs[1,9],lat=segs[1,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #CT
    mapply(stLabs1,long=segs[2,9],lat=segs[2,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #DC
    mapply(stLabs1,long=segs[3,9],lat=segs[3,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #DE
    mapply(stLabs1,long=segs[4,9],lat=segs[4,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #MA
    mapply(stLabs1,long=segs[5,9],lat=segs[5,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #MD
    mapply(stLabs1,long=segs[6,9],lat=segs[6,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="#525252",cex=1.5,font=2,pos=3,offset=0.50) #NH
    mapply(stLabs1,long=segs[7,9],lat=segs[7,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #NJ
    mapply(stLabs1,long=segs[8,9],lat=segs[8,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="#525252",cex=1.5,font=2,pos=4,offset=0.50) #RI
    mapply(stLabs1,long=segs[9,9],lat=segs[9,10],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="#525252",cex=1.5,font=2,pos=3,offset=0.50) #VT
    ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
    ptlines<-function(long1,lat1,long2,lat2){
      ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="#525252")
      return(ptlines)
    }
    mapply(ptlines,long1=segs[,9],lat1=segs[,10],long2=ctrOff[,3],lat2=ctrOff[,2])
    
  }
  else{
  map("state",col="white",fill=F,
     lty = 1, lwd = 1, mar=c(0,0,0,0),
     myborder = 0)
  
  # plot choropleth map
  par(mar=c(0,0,0,0))
  stData(states,shades)
  
  # overlay state borders
  map("state", col = "#bdbdbd", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, 
    myborder = 0,mar=c(0,0,0,0),resolution=0)
  }
  # hospital points
  if(lUrban==TRUE){
    points(data2[data2$type=="UH",]$longitude,data2[data2$type=="UH",]$latitude,col="firebrick4",pch=19,cex=1.5)
  }
  if(sUrban==TRUE){
    points(data2[data2$type=="SUH",]$longitude,data2[data2$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=1.5)
  }
  if(lRural==TRUE){
    points(data2[data2$type=="RH",]$longitude,data2[data2$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=1.5)
  }
  if(sRural==TRUE){
    points(data2[data2$type=="SRH",]$longitude,data2[data2$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=1.5)
  }
  if(cah==TRUE){
    points(data2[data2$type=="CAH",]$longitude,data2[data2$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=1.5)
  }
  
  # alaska plot
  par(mar=c(0,0,0,0))
  if(labs==TRUE){
    plot(st_geometry(ak),
         col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[nrow(state.fips[state.fips$state=="Alaska",])])])],
         border="#bdbdbd",asp=2.5)
    dataVar<-data.frame(var)
    if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])){
      text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],"N/A",col="#525252",cex=1.5,font=2)
    }
    else if(min==0 & max==100){
      if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<=25){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else{
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }
    else if(min !=0){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<min){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else{
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }  
    else if (max!=100){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/2)))){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else{
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }  
    else if (min !=0 & max !=100){
      if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<min){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else if((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/4)))){
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
      }
      else{
        text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
      }
    }    
    else {
      text(data[data[,2]=="AK",][,9],data[data[,2]=="AK",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }
  else{
  plot(st_geometry(ak),
       col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[nrow(state.fips[state.fips$state=="Alaska",])])])],
       border="#bdbdbd",asp=2)
  }
  # hospital points
  if(lUrban==TRUE){
  points(data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="UH",]$longitude,data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="UH",]$latitude,col="firebrick4",pch=19,cex=1.5)
  }
  if(sUrban==TRUE){
  points(data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="SUH",]$longitude,data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=1.5)
  }
  if(lRural==TRUE){
  points(data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="RH",]$longitude,data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=1.5)
  }
  if(sRural==TRUE){
  points(data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="SRH",]$longitude,data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=1.5)
  }
  if(cah==TRUE){
  points(data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="CAH",]$longitude,data2[data2$state=="Alaska",][data2[data2$state=="Alaska",]$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=1.5)
  }
  
  # hawaii plot
  par(mar=c(0,0,0,0))
  if(labs==TRUE){
    plot(st_geometry(hi),
         col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[nrow(state.fips[state.fips$state=="Hawaii",])])])],
         border="#bdbdbd",asp=1.5)
    dataVar<-data.frame(var)
    text(data[data[,2]=="HI",][,9],data[data[,2]=="HI",][,10],paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,2]=="HI",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="#525252",cex=1.5,font=2)
  }
  else{
  plot(st_geometry(hi),
       col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[nrow(state.fips[state.fips$state=="Hawaii",])])])],
       border="#bdbdbd",asp=1.5)
  }
  # hospital points
  if(lUrban==TRUE){
  points(data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="UH",]$longitude,data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="UH",]$latitude,col="firebrick4",pch=19,cex=1.5)
  }
  if(sUrban==TRUE){
  points(data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="SUH",]$longitude,data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=1.5)
  }
  if(lRural==TRUE){
  points(data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="RH",]$longitude,data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=1.5)
  }
  if(sRural==TRUE){
  points(data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="SRH",]$longitude,data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=1.5)
  }
  if(cah==TRUE){
  points(data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="CAH",]$longitude,data2[data2$state=="Hawaii",][data2[data2$state=="Hawaii",]$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=1.5)
  }
  
  # legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="#525252",box.col=NA,bty="n",inset=c(0,0))
  legend("bottomleft",
         legend = c("Urban (>50 beds)","Small Urban (<50 beds)","Rural (>100 beds)","Small Rural (<100 beds)","Critical Access"), xpd=NA,
         col = c("firebrick4","firebrick1","goldenrod4","goldenrod1","#fb9a99"),text.font=2,horiz=T,
         title = "",pch=c(19,19,15,15,17),
         cex=1.5,pt.cex=2,text.col="#525252",box.col=NA,bty="n",inset=c(0,-0.30))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=2,horiz=T,
         title = "Percent of Medicare Eligible Hospitals that have Demonstrated Meaningful Use | December 2015",
         cex=2.5,text.col="#253494",box.col=NA,bty="n",inset=c(0,0.15))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=3,horiz=T,
         title = "95% of all U.S. hospitals have demonstrated meaningful use",
         cex=2,text.col="#252525",box.col=NA,bty="n",inset=c(0,-0.05))	 
  
  mtext("Source: CMS EHR Incentive Program data, 2015 and CMS Provider of Services data, December 2015",outer=T,side=1,line=-4,adj=0,cex=0.90,col="#525252")
  mtext("Note: Hospitals eligible to participate in Medicare EHR Incentive Program only; children's hospitals excluded",outer=T,side=1,line=-2,adj=0,cex=0.90,col="#525252")
  }
  
  else{
    
    layout(matrix(c(9,8,8,8,3,9,8,8,8,4,9,8,8,8,5,9,8,8,8,6,9,8,8,8,7,9,8,8,8,10,9,8,8,8,10,1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1,1,1,1,2), nrow=5,ncol=11),c(1,1),c(1,1.5))
    
    if(state !="Alaska" & state !="Hawaii"){
    par(mar=c(0,0,0,0))
    par(oma=c(5,1,0,1))
    map("state",region=c(state),col="white",fill=F,
        lty = 1, lwd = 1, mar=c(0,0,0,0),
        myborder = 0)
    
    # plot choropleth map
    par(mar=c(0,0,0,0))
    stData(states,shades)
    
    # overlay state borders
    map("state", region=c(state),col = "gray", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, 
        myborder = 0,mar=c(0,0,0,0),resolution=0)
    
    # hospital points
    if(lUrban==TRUE){
      points(data2[data2$type=="UH",]$longitude,data2[data2$type=="UH",]$latitude,col="firebrick4",pch=19,cex=2.5)
    }
    if(sUrban==TRUE){
      points(data2[data2$type=="SUH",]$longitude,data2[data2$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=2.5)
    }
    if(lRural==TRUE){
      points(data2[data2$type=="RH",]$longitude,data2[data2$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=2.5)
    }
    if(sRural==TRUE){
      points(data2[data2$type=="SRH",]$longitude,data2[data2$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=2.5)
    }
    if(cah==TRUE){
      points(data2[data2$type=="CAH",]$longitude,data2[data2$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=2.5)
    }
    
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
    legend("bottom",
           legend = c("Urban (>50 beds)","Small Urban (<50 beds)","Rural (>100 beds)","Small Rural (<100 beds)","Critical Access"), xpd=NA,
           col = c("firebrick4","firebrick1","goldenrod4","goldenrod1","#fb9a99"),text.font=2,horiz=F,ncol=2,
           title = "",pch=c(19,19,15,15,17),
           cex=1.5,pt.cex=2,text.col="#525252",box.col=NA,bty="n",inset=c(0,-0.10))
    }
  else if (state=="Alaska"){
    par(mar=c(0,0,0,0))
    plot(st_geometry(ak),
         col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Alaska",])[nrow(state.fips[state.fips$state=="Alaska",])])])],
         border="gray",asp=2.5)
    
    # hospital points
    if(lUrban==TRUE){
      points(data2[data2$type=="UH",]$longitude,data2[data2$type=="UH",]$latitude,col="firebrick4",pch=19,cex=2.5)
    }
    if(sUrban==TRUE){
      points(data2[data2$type=="SUH",]$longitude,data2[data2$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=2.5)
    }
    if(lRural==TRUE){
      points(data2[data2$type=="RH",]$longitude,data2[data2$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=2.5)
    }
    if(sRural==TRUE){
      points(data2[data2$type=="SRH",]$longitude,data2[data2$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=2.5)
    }
    if(cah==TRUE){
      points(data2[data2$type=="CAH",]$longitude,data2[data2$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=2.5)
    }
    
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
    legend("bottom",
           legend = c("Urban (>50 beds)","Small Urban (<50 beds)","Rural (>100 beds)","Small Rural (<100 beds)","Critical Access"), xpd=NA,
           col = c("firebrick4","firebrick1","goldenrod4","goldenrod1","#fb9a99"),text.font=2,horiz=F,ncol=2,
           title = "",pch=c(19,19,15,15,17),
           cex=1.5,pt.cex=2,text.col="#525252",box.col=NA,bty="n",inset=c(0,-0.05))
  }
  else {
    par(mar=c(0,0,0,0))
    plot(st_geometry(hi),
         col=shades[as.numeric(states[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[1]):as.numeric(rownames(state.fips[state.fips$state=="Hawaii",])[nrow(state.fips[state.fips$state=="Hawaii",])])])],
         border="gray",asp=1.5)
    
    # hospital points
    if(lUrban==TRUE){
      points(data2[data2$type=="UH",]$longitude,data2[data2$type=="UH",]$latitude,col="firebrick4",pch=19,cex=2.5)
    }
    if(sUrban==TRUE){
      points(data2[data2$type=="SUH",]$longitude,data2[data2$type=="SUH",]$latitude,col="firebrick1",pch=19,cex=2.5)
    }
    if(lRural==TRUE){
      points(data2[data2$type=="RH",]$longitude,data2[data2$type=="RH",]$latitude,col="goldenrod4",pch=15,cex=2.5)
    }
    if(sRural==TRUE){
      points(data2[data2$type=="SRH",]$longitude,data2[data2$type=="SRH",]$latitude,col="goldenrod1",pch=15,cex=2.5)
    }
    if(cah==TRUE){
      points(data2[data2$type=="CAH",]$longitude,data2[data2$type=="CAH",]$latitude,col="#fb9a99",pch=17,cex=2.5)
    }
    
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
    legend("bottom",
           legend = c("Urban (>50 beds)","Small Urban (<50 beds)","Rural (>100 beds)","Small Rural (<100 beds)","Critical Access"), xpd=NA,
           col = c("firebrick4","firebrick1","goldenrod4","goldenrod1","#fb9a99"),text.font=2,horiz=F,ncol=2,
           title = "",pch=c(19,19,15,15,17),
           cex=1.5,pt.cex=2,text.col="#525252",box.col=NA,bty="n",inset=c(0,-0.05))
  }
  ## for pie graphs
fills<-function(num,state){
  if(data[data[,1]==state,][,num]>=(mean(piePaid[,num])+0.025)){
    fills<-"limegreen"
  }
  else if (data[data[,1]==state,][,num]<=(mean(piePaid[,num])-0.025)){
    fills<-"firebrick3"
  }
  else if (data[data[,1]==state,][,num]==0){
    fills<-"gray"
  }
  else{
    fills<-"yellow3"
  }
  return(fills)
}
  par(mar=c(1,0,0,0))
  ## urban
  pie(c(data[data[,1]==state,][,4],1-data[data[,1]==state,][,4]),
      labels=NA,
      col=c(fills(num=4,state=state),"gray"),
      main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*data[data[,1]==state,][,4]),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Urban",line=-0.5,cex.sub=1.5,font.sub=2)
  ## small urban
  pie(c(data[data[,1]==state,][,5],1-data[data[,1]==state,][,5]),
    labels=NA,
    col=c(fills(num=5,state=state),"gray"),
    main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*data[data[,1]==state,][,5]),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Small Urban",line=-0.5,cex.sub=1.5,font.sub=2)
  ## rural
  pie(c(data[data[,1]==state,][,6],1-data[data[,1]==state,][,6]),
    labels=NA,
    col=c(fills(num=6,state=state),"gray"),
    main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*data[data[,1]==state,][,6]),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Rural",line=-0.5,cex.sub=1.5,font.sub=2)
  ## small rural
  pie(c(data[data[,1]==state,][,7],1-data[data[,1]==state,][,7]),
    labels=NA,
    col=c(fills(num=7,state=state),"gray"),
    main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*data[data[,1]==state,][,7]),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Small Rural",line=-0.5,cex.sub=1.5,font.sub=2)
  ## cah
  pie(c(data[data[,1]==state,][,8],1-data[data[,1]==state,][,8]),
    labels=NA,
    col=c(fills(num=8,state=state),"gray"),
    main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*data[data[,1]==state,][,8]),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Critical Access",line=-0.5,cex.sub=1.5,font.sub=2)
  par(mar=c(2,3.25,0,0))
  fills<-c()
  for(i in 1:length(stPaid[,1])){
    if(stPaid[,1][i]==state){
      if(stPaid[,2][i]>=(national+0.025)){
        fills<-c(fills,"limegreen")
      }
      else if (stPaid[,2][i]<=(national-0.025)){
        fills<-c(fills,"firebrick3") 
      }
      else{
        fills<-c(fills,"yellow3")
      }
    }
    else {
      fills<-c(fills,"lightgray")
    }
  }
  barplot(stPaid[,2],horiz=F,ylim=c(0,1.1),border=NA,yaxt="n",col=fills)
  axis(side=2,line=-1,las=2,at=c(0,.25,.5,.75,1),labels=c(0,"25%","50%","75%","100%"),col=NA,col.ticks="gray",font=2,cex.axis=1.5)
  mids<-c(0.7,
          1.9,
          3.1,
          4.3,
          5.5,
          6.7,
          7.9,
          9.1,
          10.3,
          11.5,
          12.7,
          13.9,
          15.1,
          16.3,
          17.5,
          18.7,
          19.9,
          21.1,
          22.3,
          23.5,
          24.7,
          25.9,
          27.1,
          28.3,
          29.5,
          30.7,
          31.9,
          33.1,
          34.3,
          35.5,
          36.7,
          37.9,
          39.1,
          40.3,
          41.5,
          42.7,
          43.9,
          45.1,
          46.3,
          47.5,
          48.7,
          49.9,
          51.1,
          52.3,
          53.5,
          54.7,
          55.9,
          57.1,
          58.3,
          59.5,
          60.7)
  axis(side=1,line=-0.5,las=2,at=mids,labels=stPaid[,3],col=NA,col.ticks=NA,font=2,cex.axis=1.35)
  text(30.7,1.025,paste(paste(round((100*data[data[,1]==state,][,3]),0),"%",sep=""),"of Medicare eligible",state,"hospitals",sep=" "),col=fills[as.numeric(rownames(stPaid[stPaid[,1]==state,]))],cex=2,font=2,adj=0.5)
  ## plot area 6
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  mtext(paste("Percent of",state,"Hospitals that have Demonstrated Meaningful Use | December 2015",sep=" "),outer=T,line=-2,side=3,adj=0,cex=1.25,font=2,col="#253494")
  mtext("Source: CMS EHR Incentive Program data, 2015 and CMS Provider of Services data, December 2015",outer=T,line=-4,side=3,adj=0,font=1,cex=0.9,col="#525252")
  mtext("Note: Hospitals eligible to participate in Medicare EHR Incentive Program only; children's hospitals excluded",outer=T,line=-6,side=3,adj=0,font=1,cex=0.9,col="#525252")
  # legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  legend.txt<-c("<2.5 pct points below US Avg",
                "(+/-) 2.5 pct points of US Avg",
                ">2.5 pct points above US Avg")
  legend("top",
         legend = legend.txt, xpd=NA,
         fill = c("firebrick3","yellow3","limegreen"),text.font=1,horiz=F,
         title="",
         cex=1.5,text.col="#525252",box.col=NA,bty="n",border=NA)
  }
}