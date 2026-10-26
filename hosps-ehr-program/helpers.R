percent_map <- function(data,var,type,paid,labs=FALSE,ctrOff,min,max) {
  nat<-var[52]
  
    vec<-c(0,1,summary(var)[2][[1]]-0.001,nat,nat+((100-nat)/2),100)
    shades<-c("#969696","#d94801","#67a9cf","#02818a","#014636")

  var[var==0]<-0.001
  percents<-as.numeric(cut(var,vec))
  state<-percents[match(state.fips$abb,data[,1])]
  fills <- shades[state]
  var[var==0.001]<-0
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
    lty = 1, lwd = 0.5, 
    myborder = 0,mar=c(0,0,0,0),resolution=0)
  
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
    if(lab==0){
		label<-text(long,lat,"N/A",col="dimgrey",cex=cex,font=font)
    }
	else if(lab>=30 & lab<=79){
		label<-text(long,lat,paste(lab,"%",sep=""),col="white",cex=cex,font=font)
	}
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col="white",cex=cex,font=font)
      }
    return(label)
  }
  stLabs1<-function(long,lat,lab,col,font,cex,pos,offset){
    if(lab==0){
      label<-text(long,lat,"N/A",col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    else {
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    return(label)
  }
  dataVar<-data.frame(var)
  mapply(stLabs,long=ctrCont[,8],lat=ctrCont[,7],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
  mapply(stLabs1,long=segs[1,8],lat=segs[1,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
  mapply(stLabs1,long=segs[2,8],lat=segs[2,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
  mapply(stLabs1,long=segs[3,8],lat=segs[3,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
  mapply(stLabs1,long=segs[4,8],lat=segs[4,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
  mapply(stLabs1,long=segs[5,8],lat=segs[5,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
  mapply(stLabs1,long=segs[6,8],lat=segs[6,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
  mapply(stLabs1,long=segs[7,8],lat=segs[7,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
  mapply(stLabs1,long=segs[8,8],lat=segs[8,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
  mapply(stLabs1,long=segs[9,8],lat=segs[9,7],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT
  ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
  ptlines<-function(long1,lat1,long2,lat2){
    ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="dimgrey")
    return(ptlines)
  }
  mapply(ptlines,long1=segs[,8],lat1=segs[,7],long2=ctrOff[,3],lat2=ctrOff[,2])
  
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
  if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])){
      text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,"N/A",col="dimgrey",cex=1.5,font=2)
  }
  else if ((dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])<80 & (dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))])>29){
      text(data[data[,1]=="AK",]$longitude,data[data[,1]=="AK",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(data[data[,1]=="AK",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
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
  
  legend.txt<-c("N/A","< National Avg","National Avg",paste0(nat+((100-nat)/2),"%"),"100%")
  #c("N/A","0-9%","10-19%","20-29%","30-39%","40-49%","50-59%","60-69%","70-79%","80-89%","90-99%","100%") 
  
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.05))
  legend("bottomleft",
           legend = "", xpd=NA,text.font=2,horiz=T,
           title = paste("Percent of",type,"that have",paid,"| 2016",sep=" "),
           cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.15))
  #legend("bottomleft",
  #       legend = "", xpd=NA,text.font=2,horiz=T,
  #       title = type,
  #       cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=2,horiz=T,
         title = paste(paste(var[52],"%",sep=""), "of", type,"have",paid,sep=" "),
         cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.05))
  mtext("Source: CMS EHR Incentive Program data, 2016 and CMS Provider of Services File, March 2017",outer=T,line=-1.5,side=1,adj=0,font=1,cex=0.8,col="dimgrey")
  #mtext("Notes: (1) State-level denominators estimated from CMS Provider of Services File. These include only general acute #care,",outer=T,line=-3,side=1,adj=0,font=1,cex=0.8,col="dimgrey")
  #mtext("critical access, and children's hospitals. (2) State counts include only active hospitals eligible for the EHR Incentive #Programs.",outer=T,line=-1.5,side=1,adj=0,font=1,cex=0.8,col="dimgrey")
}