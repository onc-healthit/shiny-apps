percent_map <- function(data,year,vendor,system,legend.title,labs=FALSE,ctrOff,xwalk,min = 0,max = 25) {
  options(stringsAsFactors=FALSE)
  # set values for boolean variables: combo & standalone
  if(system=="Display all Office-based Providers with this Vendor"){
    combo<-FALSE
    standalone<-FALSE
  }
  else if(system=="Display Office-based Providers with Only this Vendor"){
    standalone<-TRUE
    combo<-FALSE
  }
  else if(system=="Display Office-based Providers with this Vendor Combined with Others"){
    combo<-TRUE
    standalone<-FALSE
  }
  # generate data frame for map // code for market share map
  if(ncol(data)<6){
  func<-function(data=data,year=year,vendor=vendor,standalone=standalone,combo=combo){
    out<-tryCatch(
  venMkt(data=data,year=year,vendor=vendor,standalone=standalone,combo=combo),
  error = function(e){
    print("No market information available")
  }
    )
  return(out)
  }
  newDat<-func(data=data,year=year,vendor=vendor,standalone=standalone,combo=combo)
  if(length(newDat)==1){
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    rect(-1,0.75,1,1,col="firebrick",border=NA)
    text(-0.97,0.875,"No market information available for selected vendor in selected year. Please make another selection.",font=2,cex=1.25,col="white",adj=0)
  }
  else{
  var<-as.numeric(newDat[,4])
  newDat<-data.frame(newDat[,1:4],longitude=xwalk$longitude[match(newDat[,1],xwalk[,1])],latitude=xwalk$latitude[match(newDat[,1],xwalk[,1])])
  newDat[,5]<-as.character(newDat[,5])
  newDat[,6]<-as.character(newDat[,6])
  newDat[,5]<-as.numeric(newDat[,5])
  newDat[,6]<-as.numeric(newDat[,6])
  # assign 0 a new value
  var[var==0]<-0.001
  # determine range for choropleth
  vec<-tryCatch({
  if (min==0 & max !=100) {
    vec<-c(min,0.002,round((min+((max-min)*(1/5))),0),round((min+((max-min)*(2/5))),0),round((min+((max-min)*(3/5))),0),round((min+((max-min)*(4/5))),0),max,100)
    #shades<-c("lightgray",brewer.pal(7,"Blues")[2:7])
  }
  else if (min !=0 & max !=100) {
	vec<-c(0,0.002,min-1,round((min+((max-min)*(1/5))),0),round((min+((max-min)*(2/5))),0),round((min+((max-min)*(3/5))),0),round((min+((max-min)*(4/5))),0),max,100)
	#shades<-c("lightgray",brewer.pal(8,"Blues")[2:8])
  }
  else if (min !=0 & max==100) {
    vec<-c(0,0.002,min-1,round((min+((max-min)*(1/5))),0),round((min+((max-min)*(2/5))),0),round((min+((max-min)*(3/5))),0),round((min+((max-min)*(4/5))),0),max)
    #shades<-c("lightgray",brewer.pal(7,"Blues")[2:7])
  }
  else {
    vec<-c(0,0.002,20,40,60,80,100)
    #shades<-c("lightgray",brewer.pal(6,"Blues")[2:6])
  }
  },
  error = function(e){
    vec<-c("Loading...")
  }
  )
  if(length(vec)==1){
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    text(-1,1,"Loading...",font=2,cex=1.5,col="firebrick",adj=0)
  }
  else{
    if (length(vec)==8) {
      shades<-c("lightgray",brewer.pal(7,"Blues")[2:7])
    }
    else if (length(vec)==9) {
      shades<-c("lightgray",brewer.pal(8,"Blues")[2:8])
    }
    else {
      shades<-c("lightgray",brewer.pal(6,"Blues")[2:6])
    }
  # calculate tranches for % values
  percents<-as.numeric(cut(var,vec))
  # match to state
  state<-percents[match(state.fips$state,newDat[,1])]
  fills <- shades[state]
  # re-assign 0 a 0 value
  var[var==0.001]<-0
  # refresh map
  layout(matrix(c(4,2,2,3,3,4,1,1,1,1,4,1,1,1,1), 5), c(1,3), c(1,3))
  par(mar=c(0,0,0,0))
  par(oma=c(0,0,0,0))
  # run code for map w/ labels
  if(labs==TRUE){
  # generate continental US map
  map("state",col="white",fill=F,
     lty = 1, lwd = 1, mar=c(0,0,0,0),
     myborder = 0)
  # plot choropleth map
  par(mar=c(0,0,0,0))
  stData(state,shades)
  # overlay state borders
  map("state", col = "gray88", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, 
    myborder = 0,mar=c(0,0,0,0),resolution=0)
  # code for labels
  ctrCont<-newDat[newDat[,1] !="District Of Columbia",]
  ctrCont<-ctrCont[ctrCont[,1] !="Maryland",]
  ctrCont<-ctrCont[ctrCont[,1] !="Delaware",]
  ctrCont<-ctrCont[ctrCont[,1] !="New Jersey",]
  ctrCont<-ctrCont[ctrCont[,1] !="Connecticut",]
  ctrCont<-ctrCont[ctrCont[,1] !="Rhode Island",]
  ctrCont<-ctrCont[ctrCont[,1] !="Massachusetts",]
  ctrCont<-ctrCont[ctrCont[,1] !="Vermont",]
  ctrCont<-ctrCont[ctrCont[,1] !="New Hampshire",]
  segs<-rbind(
    newDat[newDat[,1]=="District Of Columbia",],
    newDat[newDat[,1]=="Maryland",],
    newDat[newDat[,1]=="Delaware",],
    newDat[newDat[,1]=="New Jersey",],
    newDat[newDat[,1]=="Connecticut",],
    newDat[newDat[,1]=="Rhode Island",],
    newDat[newDat[,1]=="Massachusetts",],
    newDat[newDat[,1]=="Vermont",],
    newDat[newDat[,1]=="New Hampshire",])
  segs<-segs[order(segs[,1],decreasing=F),]
  stLabs<-function(long,lat,lab,col,font,cex,min,max){
    if(is.na(lab)){
      label<-text(long,lat,"N/A",col="dimgrey",cex=cex,font=font)
    }
    else if(min==0 & max==100){
      if (lab<=20){
      label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }
    else if(min !=0 & max==100){
      if(lab<min){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if (lab<=(min+((max-min)*(1/5)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }  
    else if (min==0 & max!=100){
      if(lab<=(min+((max-min)*(1/5)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if(lab>=max){
        label<-text(long,lat,paste(lab,"%",sep=""),col="white",cex=cex,font=font)
      }
      else{
        label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
      }
    }  
    else if (min !=0 & max !=100){
      if(lab<min){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if(lab<=(min+((max-min)*(1/5)))){
        label<-text(long,lat,paste(lab,"%",sep=""),col="dimgrey",cex=cex,font=font)
      }
      else if(lab<=(min+((max-min)*(2/5)))){
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
      label<-text(long,lat,"N/A",col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    else {
      label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
    }
    return(label)
  }
  dataVar<-data.frame(as.numeric(var))
  dataVar[dataVar>1]<-round(dataVar[dataVar>1],0)
  dataVar[dataVar<0.10 & dataVar !=0]<-"<0.1"
  mapply(stLabs,long=ctrCont[,5],lat=ctrCont[,6],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
  mapply(stLabs1,long=segs[1,5],lat=segs[1,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
  mapply(stLabs1,long=segs[2,5],lat=segs[2,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
  mapply(stLabs1,long=segs[3,5],lat=segs[3,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
  mapply(stLabs1,long=segs[4,5],lat=segs[4,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
  mapply(stLabs1,long=segs[5,5],lat=segs[5,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
  mapply(stLabs1,long=segs[6,5],lat=segs[6,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
  mapply(stLabs1,long=segs[7,5],lat=segs[7,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
  mapply(stLabs1,long=segs[8,5],lat=segs[8,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
  mapply(stLabs1,long=segs[9,5],lat=segs[9,6],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT
  ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
  ptlines<-function(long1,lat1,long2,lat2){
    ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="dimgrey")
    return(ptlines)
  }
  mapply(ptlines,long1=segs[,5],lat1=segs[,6],long2=ctrOff[,3],lat2=ctrOff[,2])
  
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
    map("state", col = "gray88", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, 
        myborder = 0,mar=c(0,0,0,0),resolution=0) 
  }
  # alaska plot
  par(mar=c(8,0,0,0))
  # code for labels
  if(labs==TRUE){
  plot(st_geometry(ak),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],border="lightgray",asp=2.5)
  dataVar<-data.frame(var)
  dataVar[dataVar>1]<-round(dataVar[dataVar>1],0)
  dataVar[dataVar<0.10 & dataVar !=0]<-"<0.1"
  if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])){
    text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,"N/A",col="dimgrey",cex=1.5,font=2)
  }
  else if(min==0 & max==100){
    if ((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=20){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }
  else if(min !=0 & max==100){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<min){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if ((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/5)))){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }  
  else if (min==0 & max!=100){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/5)))){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(2/5)))){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }  
  else if (min !=0 & max !=100){
    if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<min){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(1/5)))){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<=(min+((max-min)*(2/5)))){
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
    }
    else{
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
    }
  }    
  else {
      text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="white",cex=1.5,font=2)
  }
  }
  # no labels
  else{
  plot(st_geometry(ak),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],border="lightgray",asp=2.5)
  }
  # hawaii plot
  par(mar=c(6,4,0,0))
  # code for labels
  if(labs==TRUE){
  plot(st_geometry(hi),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],border="lightgray",asp=1.5)
  dataVar<-data.frame(var)
  dataVar[dataVar>1]<-round(dataVar[dataVar>1],0)
  dataVar[dataVar<0.10 & dataVar !=0]<-"<0.1"
  text(newDat[newDat[,1]=="Hawaii",]$longitude,newDat[newDat[,1]=="Hawaii",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Hawaii",]))),as.numeric(rownames(dataVar)))],"%",sep=""),col="dimgrey",cex=1.5,font=2)
  }
  # no labels
  else {
  plot(st_geometry(hi),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],border="lightgray",asp=1.5)
  }
  # false plot/legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  # add a legend
  inc <- (max - min) / 5
    if (min !=0 & max !=100) {
      legend.txt<-c(
        "0%",
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
          "0%",
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
        "0%",
        paste(min+1,"-",round((min + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",round((min + 4 * inc),0),"%"),
        paste(round((min+4*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      ) 
    }
  else {
        legend.txt<-c("0%","1-20%","21-40%","41-60%","61-80%","81-100%")
    }
  # calculation for national average in title
  natAvg<-var[as.numeric(rownames(newDat[newDat[,1]=="National",]))]
  # character variable for averages less than 1
  if(natAvg<1){
    natAvg<-"<1"
  }
  else{}
  if(standalone==TRUE & combo==FALSE){
    legend("bottomleft",
           legend = legend.txt, xpd=NA,
           fill = shades,text.font=2,horiz=T,
           title = "",
           cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
    legend("bottomleft",
           legend = "", xpd=NA,text.font=2,horiz=T,
           title = "Percent of Office-based Providers Participating in Medicare EHR Incentive Program Using only this Vendor's Product",
           cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
    legend("bottomleft",
           legend = "", xpd=NA,text.font=2,horiz=T,
           title = vendor,
           cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
    #legend("bottomleft",
    #       legend = "", xpd=NA,text.font=2,horiz=T,
    #       title = paste(year,"National Market Share =",paste(natAvg,"%",sep=""),sep=" "),
    #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
  }
  else if (combo==TRUE & standalone==FALSE){
    legend("bottomleft",
           legend = legend.txt, xpd=NA,
           fill = shades,text.font=2,horiz=T,
           title = "",
           cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
    legend("bottomleft",
           legend = "", xpd=NA,text.font=2,horiz=T,
           title = "Percent of Office-based Providers Participating in Medicare EHR Incentive Program using Vendor's Product in addition to Other Product(s)",
           cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
    legend("bottomleft",
           legend = "", xpd=NA,text.font=2,horiz=T,
           title = vendor,
           cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
    #legend("bottomleft",
    #       legend = "", xpd=NA,text.font=2,horiz=T,
    #       title = paste(year,"National Market Share =",paste(natAvg,"%",sep=""),sep=" "),
    #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
  }
  else{
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
  legend("bottomleft",
         legend = "", xpd=NA,text.font=2,horiz=T,
         title = "Percent of Office-based Providers Participating in Medicare EHR Incentive Program using Vendor's Product",
         cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
  legend("bottomleft",
          legend = "", xpd=NA,text.font=2,horiz=T,
          title = vendor,
          cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
  #legend("bottomleft",
  #       legend = "", xpd=NA,text.font=2,horiz=T,
  #       title = paste(year,"National Market Share =",paste(natAvg,"%",sep=""),sep=" "),
  #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
  }
  mtext("Source: EHR Products Used for Meaningful Use Attestation Public Use File, December 2015",outer=T,side=1,line=-4,adj=0,cex=0.90,font=3,col="dimgrey")
  mtext("Note: See 'http://healthit.gov/datadashboard/data.php#vendors' for more information",outer=T,side=1,line=-2,adj=0,cex=0.90,font=3,col="dimgrey")
  }
  }
  }
  # code for generating map for market rank
  else{
      if(standalone==TRUE & combo==FALSE){
        newDat<-data[data[,5]=="standalone",]
      }
      else if(standalone==FALSE & combo==TRUE){
        newDat<-data[data[,5]=="combo",]
      }
      else{
        newDat<-data[data[,5]=="any",]
      }
      #newDat<-newDat[newDat[,1] !="National",][newDat[newDat[,1] !="National",][,6]==year,][newDat[newDat[,1] !="National",][newDat[newDat[,1] !="National",][,6]==year,][,2]==vendor,]
      newDat<-newDat[newDat[,6]==year,][newDat[newDat[,6]==year,][,2]==vendor,]
      if(nrow(newDat)==0){
        par(mar=c(0,0,0,0))
        plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
        rect(-1,0.75,1,1,col="firebrick",border=NA)
        text(-0.97,0.875,"No market information available for selected vendor in selected year. Please make another selection.",font=2,cex=1.25,col="white",adj=0)
      }
      else{
      natDat<-newDat[newDat[,6]==year,][newDat[newDat[,6]==year,][,2]==vendor,]
      newDat<-newDat[newDat[,1] !="National",]
      rownames(newDat)<-c(1:nrow(newDat))
      var<-as.numeric(newDat[,4])
      newDat<-data.frame(newDat[,1:6],longitude=xwalk$longitude[match(newDat[,1],xwalk[,1])],latitude=xwalk$latitude[match(newDat[,1],xwalk[,1])])
      newDat[,7]<-as.character(newDat[,7])
      newDat[,8]<-as.character(newDat[,8])
      newDat[,7]<-as.numeric(newDat[,7])
      newDat[,8]<-as.numeric(newDat[,8])
      # assign 0 value new value for colors
      var[var==0]<-0.001
      vec<-c(0,0.002,1.5,2.5,5,10,100)
      shades<-c("lightgray","#018571","#80cdc1","#ffffbf","#dfc27d","#a6611a")
      percents<-as.numeric(cut(var,vec))
      state<-percents[match(state.fips$state,newDat[,1])]
      fills <- shades[state]
      # re-assign 0 value a 0
      var[var==0.001]<-0  
      # refresh map
      layout(matrix(c(4,2,2,3,3,4,1,1,1,1,4,1,1,1,1), 5), c(1,3), c(1,3))
      par(mar=c(0,0,0,0))
      par(oma=c(0,0,0,0))
      # code for labels
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
        ctrCont<-newDat[newDat[,1] !="District Of Columbia",]
        ctrCont<-ctrCont[ctrCont[,1] !="Maryland",]
        ctrCont<-ctrCont[ctrCont[,1] !="Delaware",]
        ctrCont<-ctrCont[ctrCont[,1] !="New Jersey",]
        ctrCont<-ctrCont[ctrCont[,1] !="Connecticut",]
        ctrCont<-ctrCont[ctrCont[,1] !="Rhode Island",]
        ctrCont<-ctrCont[ctrCont[,1] !="Massachusetts",]
        ctrCont<-ctrCont[ctrCont[,1] !="Vermont",]
        ctrCont<-ctrCont[ctrCont[,1] !="New Hampshire",]
        segs<-rbind(
          newDat[newDat[,1]=="District Of Columbia",],
          newDat[newDat[,1]=="Maryland",],
          newDat[newDat[,1]=="Delaware",],
          newDat[newDat[,1]=="New Jersey",],
          newDat[newDat[,1]=="Connecticut",],
          newDat[newDat[,1]=="Rhode Island",],
          newDat[newDat[,1]=="Massachusetts",],
          newDat[newDat[,1]=="Vermont",],
          newDat[newDat[,1]=="New Hampshire",])
        segs<-segs[order(segs[,1],decreasing=F),]
        stLabs<-function(long,lat,lab,col,font,cex,min,max){
          if(is.na(lab)){
            label<-text(long,lat,"N/A",col="dimgrey",cex=cex,font=font)
          }
          else if(lab==0){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
          else if(lab==1){
              label<-text(long,lat,lab,col="white",cex=cex,font=font)
          }  
          else if(lab==2){
              label<-text(long,lat,lab,col="white",cex=cex,font=font)
          }
		  else if(lab==3){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		 else if(lab==4){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		 else if(lab==5){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		 else if(lab==6){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		 else if(lab==7){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		 else if(lab==8){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		  else if(lab==9){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }
		  else if(lab==10){
              label<-text(long,lat,lab,col="dimgrey",cex=cex,font=font)
          }

          else {
            label<-text(long,lat,lab,col="white",cex=cex,font=font)
          }  
          return(label)
        }
        stLabs1<-function(long,lat,lab,col,font,cex,pos,offset){
          if(is.na(lab)){
            label<-text(long,lat,"N/A",col=col,cex=cex,font=font,pos=pos,offset=offset)
          }
          else {
            label<-text(long,lat,lab,col=col,cex=cex,font=font,pos=pos,offset=offset)
          }
          return(label)
        }
        dataVar<-data.frame(as.numeric(newDat[,4]))
        mapply(stLabs,long=ctrCont[,7],lat=ctrCont[,8],lab=dataVar[,1][match(as.numeric(rownames(ctrCont)),as.numeric(rownames(dataVar)))],col="white",cex=1.5,font=2,min=min,max=max)
        #if(year !="2011"){
        mapply(stLabs1,long=segs[1,7],lat=segs[1,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
        mapply(stLabs1,long=segs[2,7],lat=segs[2,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
        mapply(stLabs1,long=segs[3,7],lat=segs[3,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
        mapply(stLabs1,long=segs[4,7],lat=segs[4,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
        mapply(stLabs1,long=segs[5,7],lat=segs[5,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
        mapply(stLabs1,long=segs[6,7],lat=segs[6,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
        mapply(stLabs1,long=segs[7,7],lat=segs[7,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
        mapply(stLabs1,long=segs[8,7],lat=segs[8,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
        mapply(stLabs1,long=segs[9,7],lat=segs[9,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][9],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT
        #}
        #else{
        #  mapply(stLabs1,long=segs[1,7],lat=segs[1,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][1],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #CT
          #mapply(stLabs1,long=segs[2,7],lat=segs[2,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DC
        #  mapply(stLabs1,long=segs[2,7],lat=segs[2,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][2],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #DE
        #  mapply(stLabs1,long=segs[3,7],lat=segs[3,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][3],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MA
        #  mapply(stLabs1,long=segs[4,7],lat=segs[4,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][4],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #MD
        #  mapply(stLabs1,long=segs[5,7],lat=segs[5,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][5],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #NH
        #  mapply(stLabs1,long=segs[6,7],lat=segs[6,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][6],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #NJ
        #  mapply(stLabs1,long=segs[7,7],lat=segs[7,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][7],col="dimgrey",cex=1.5,font=2,pos=4,offset=0.50) #RI
        #  mapply(stLabs1,long=segs[8,7],lat=segs[8,8],lab=dataVar[,1][match(as.numeric(rownames(segs)),as.numeric(rownames(dataVar)))][8],col="dimgrey",cex=1.5,font=2,pos=3,offset=0.50) #VT 
        #}
        ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
        #if(year=="2011"){
        #  ctrOff<-ctrOff[ctrOff[,1] !="District Of Columbia",]
        #}
        #else{}
        ptlines<-function(long1,lat1,long2,lat2){
          ptlines<-segments(long1,lat1,long2,lat2,lwd=1,col="dimgrey")
          return(ptlines)
        }
        mapply(ptlines,long1=segs[,7],lat1=segs[,8],long2=ctrOff[,3],lat2=ctrOff[,2])
        
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
      # code for labels
      if(labs==TRUE){
        plot(st_geometry(ak),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],border="lightgray",asp=2.5)
        dataVar<-data.frame(var)
        if(is.na(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])){
          text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,"N/A",col="dimgrey",cex=1.5,font=2)
        }
        else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])==0){
          text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="dimgrey",cex=1.5,font=2)
        }
        else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])==1){
            text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="white",cex=1.5,font=2)
        }
        else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])==2){
            text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="white",cex=1.5,font=2)
        }
        else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])>2 & (dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<6){
            text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="dimgrey",cex=1.5,font=2)
        }
        else if((dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])>5 & (dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))])<11){
          text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="dimgrey",cex=1.5,font=2)
        }   
        else {
          text(newDat[newDat[,1]=="Alaska",]$longitude,newDat[newDat[,1]=="Alaska",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Alaska",]))),as.numeric(rownames(dataVar)))],sep=""),col="dimgrey",cex=1.5,font=2)
        }
      }
      else{
        plot(st_geometry(ak),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Alaska",]))]],border="lightgray",asp=2.5)
      }
      # hawaii plot
      par(mar=c(6,4,0,0))
      # code for labels
      if(labs==TRUE){
        plot(st_geometry(hi),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],border="lightgray",asp=1.5)
        dataVar<-data.frame(var)
        text(newDat[newDat[,1]=="Hawaii",]$longitude,newDat[newDat[,1]=="Hawaii",]$latitude,paste(dataVar[,1][match(as.numeric(rownames(data.frame(newDat[newDat[,1]=="Hawaii",]))),as.numeric(rownames(dataVar)))],sep=""),col="dimgrey",cex=1.5,font=2)
      }
      else {
        plot(st_geometry(hi),col=shades[state[as.numeric(rownames(state.fips[state.fips$state=="Hawaii",]))]],border="lightgray",asp=1.5)
      }
      
      # false plot/legend
      par(mar=c(0,0,0,0))
      plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
      # add a legend
      legend.txt<-c("None","1","2","3-5","6-10","11+")
      if(standalone==TRUE & combo==FALSE){
        legend("bottomleft",
               legend = legend.txt, xpd=NA,
               fill = shades,text.font=2,horiz=T,
               title = "",
               cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = paste("Market Rank among all EHR Vendors used by Office-based Providers (using only One Vendor) Participating in the Medicare EHR Incentive Program",sep=" "),
               cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = vendor,
               cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
        #legend("bottomleft",
        #       legend = "", xpd=NA,text.font=2,horiz=T,
        #       title = paste(year,"National Market Rank =",paste(natDat[natDat[,1]=="National",][,4],sep=""),sep=" "),
        #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
      }
      else if (combo==TRUE & standalone==FALSE){
        legend("bottomleft",
               legend = legend.txt, xpd=NA,
               fill = shades,text.font=2,horiz=T,
               title = "",
               cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = paste("Market Rank among all EHR Vendors used by Office-based Providers (using more than One Vendor) Participating in the Medicare EHR Incentive Program",sep=" "),
               cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = vendor,
               cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
        #legend("bottomleft",
        #       legend = "", xpd=NA,text.font=2,horiz=T,
        #       title = paste(year,"National Market Rank =",paste(natDat[natDat[,1]=="National",][,4],sep=""),sep=" "),
        #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
      }
      else{
        legend("bottomleft",
               legend = legend.txt, xpd=NA,
               fill = shades,text.font=2,horiz=T,
               title = "",
               cex=1.5,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,0))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = paste("Market Rank among all EHR Vendors used by Office-based Providers Participating in the Medicare EHR Incentive Program",sep=" "),
               cex=1.75,text.col="black",box.col=NA,bty="n",inset=c(0,0.05))
        legend("bottomleft",
               legend = "", xpd=NA,text.font=2,horiz=T,
               title = vendor,
               cex=2,text.col="black",box.col=NA,bty="n",inset=c(0,0.25))
        #legend("bottomleft",
        #       legend = "", xpd=NA,text.font=2,horiz=T,
        #       title = paste(year,"National Market Rank =",paste(natDat[natDat[,1]=="National",][,4],sep=""),sep=" "),
        #       cex=1.75,text.col="dimgrey",box.col=NA,bty="n",inset=c(0,-0.20))
      }
      mtext("Source: EHR Products Used for Meaningful Use Attestation Public Use File, December 2015",outer=T,side=1,line=-4,adj=0,cex=0.90,font=3,col="dimgrey")
      mtext("Note: See 'http://healthit.gov/datadashboard/data.php#vendors' for more information",outer=T,side=1,line=-2,adj=0,cex=0.90,font=3,col="dimgrey")
      }
}
}