ctrCont<-ctr[ctr[,1] !="DC",]
ctrCont<-ctrCont[ctrCont[,1] !="MD",]
ctrCont<-ctrCont[ctrCont[,1] !="DE",]
ctrCont<-ctrCont[ctrCont[,1] !="NJ",]
ctrCont<-ctrCont[ctrCont[,1] !="CT",]
ctrCont<-ctrCont[ctrCont[,1] !="RI",]
ctrCont<-ctrCont[ctrCont[,1] !="MA",]
ctrCont<-ctrCont[ctrCont[,1] !="VT",]
ctrCont<-ctrCont[ctrCont[,1] !="NH",]
segs<-rbind(
  ctr[ctr[,1]=="DC",],
  ctr[ctr[,1]=="MD",],
  ctr[ctr[,1]=="DE",],
  ctr[ctr[,1]=="NJ",],
  ctr[ctr[,1]=="CT",],
  ctr[ctr[,1]=="RI",],
  ctr[ctr[,1]=="MA",],
  ctr[ctr[,1]=="VT",],
  ctr[ctr[,1]=="NH",])
segs<-segs[order(segs[,1],decreasing=F),]
stLabs<-function(long,lat,lab,col,font,cex){
  label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font)
  return(label)
}
stLabs1<-function(long,lat,lab,col,font,cex,pos,offset){
  label<-text(long,lat,paste(lab,"%",sep=""),col=col,cex=cex,font=font,pos=pos,offset=offset)
  return(label)
}
mapply(stLabs,long=ctrCont[,3],lat=ctrCont[,2],lab=ctrCont[,1],col="black",cex=0.65,font=2)
mapply(stLabs1,long=segs[1,3],lat=segs[1,2],lab=segs[1,1],col="black",cex=0.65,font=2,pos=4,offset=0.20) #CT
mapply(stLabs1,long=segs[2,3],lat=segs[2,2],lab=segs[2,1],col="black",cex=0.65,font=2,pos=4,offset=0.15) #DC
mapply(stLabs1,long=segs[3,3],lat=segs[3,2],lab=segs[3,1],col="black",cex=0.65,font=2,pos=4,offset=0.15) #DE
mapply(stLabs1,long=segs[4,3],lat=segs[4,2],lab=segs[4,1],col="black",cex=0.65,font=2,pos=3,offset=0.15) #MA
mapply(stLabs1,long=segs[5,3],lat=segs[5,2],lab=segs[5,1],col="black",cex=0.65,font=2,pos=4,offset=0.15) #MD
mapply(stLabs1,long=segs[6,3],lat=segs[6,2],lab=segs[6,1],col="black",cex=0.65,font=2,pos=3,offset=0.25) #NH
mapply(stLabs1,long=segs[7,3],lat=segs[7,2],lab=segs[7,1],col="black",cex=0.65,font=2,pos=1,offset=0.10) #NJ
mapply(stLabs1,long=segs[8,3],lat=segs[8,2],lab=segs[8,1],col="black",cex=0.65,font=2,pos=1,offset=0.10) #RI
mapply(stLabs1,long=segs[9,3],lat=segs[9,2],lab=segs[9,1],col="black",cex=0.65,font=2,pos=3,offset=0.25) #VT
ctrOff<-ctrOff[order(ctrOff[,1],decreasing=F),]
ptlines<-function(long1,lat1,long2,lat2){
ptlines<-segments(long1,lat1,long2,lat2)
return(ptlines)
}
mapply(ptlines,long1=segs[,3],lat1=segs[,2],long2=ctrOff[,3],lat2=ctrOff[,2])


data<-read.table("U:/ONC/R/hit-adopt/data/NAMCS-2013.txt",sep=",",header=T)
lat<-ctr[,2][match(data[,1],ctr[,1])]
long<-ctr[,3][match(data[,1],ctr[,1])]
data1<-data.frame(data,longitude=long,latitude=lat)

labels<-switch(input$labs,
               "Click to display % labels" == "Y"
              )

percent_map(data = nehrs,
            var = measures, 
            color = color, 
            legend.title = legend, 
            labs = labels,
            labelD = ctr,
            labelD1 = ctrOff,
            min = input$range[1], 
            max = input$range[2])

rowN<-function(data){
  n<-gsub("[^0-9]","",data)
  n<-as.numeric(n)
  return(n)
}
which(names(iris)=="Petal.Length")

if(var=nehrs[,6]){
  mtext("Note: states without data either have no rural providers,",outer=T,line=-3.5,side=1,adj=0,font=2,cex=0.65,col="dimgrey")
  mtext("or too few rural providers were captured in the survey to make a confident estimate",outer=T,line=-2.5,side=1,adj=0,font=2,cex=0.65,col="dimgrey")
}
else {
  }