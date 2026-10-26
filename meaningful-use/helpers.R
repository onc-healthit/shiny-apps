percent_map <- function(data, state="US", color,min=0,max=100) {
  ## for pie chart
  metSplit<-split(data,data[,6])
  urban<-sapply(metSplit,function(x)sum(x[,c("medPaid")]))[[2]]*(1/(sapply(metSplit,function(x)sum(x[,c("skaDocs")]))[[2]]))
  rural<-(sapply(metSplit,function(x)sum(x[,c("medPaid")]))[[1]])*(1/(sapply(metSplit,function(x)sum(x[,c("skaDocs")]))[[1]]))
  ## for bar chart
  #stPaid<-split(medPaid,medPaid$state)
  stPaid1<-data.frame(
  states=names(split(data,data[,1])),
  paid=((data.frame(sapply(split(data,data[,1]),function(x)sum(x[,c("medPaid")])))[,1])*(1/(
    data.frame(sapply(split(data,data[,1]),function(x)sum(x[,c("skaDocs")])))[,1]))),
  abb=data[,7][match(names(split(data,data[,1])),data[,1])])
  stPaid1<-stPaid1[order(stPaid1[,2],decreasing=F),]
  rownames(stPaid1)<-1:51
  national<-sum(data[,4])*(1/sum(data[,5]))

  if (state !="US"){
    data<-data[data$state==state,]
  }
  else{
  }
  var<-data[,3]
  var[var==0]<-0.5
  var[is.na(var)]<-0.1
  if (min==0 & max==100){
    vec<-c(0,0.49,0.51,20,40,60,80,100) #7
    #shades<-c("#bdbdbd","gray68","#A52A2A","#EE7600","#9ACD32","#698B22","#008B8B")
    shades<-c("gray68","#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#2c7fb8","#253494")
  }
  else if(min==0 & max !=100){
    vec<-c(0,0.49,0.51,max*(1/5),max*(2/5),max*(3/5),max*(4/5),max,100) #8
    shades<-c("gray68","#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }
  else if(min !=0 & max==100){
    vec<-c(0,0.49,0.51,min-1,(min+((100-min)*(1/5))),(min+((100-min)*(2/5))),(min+((100-min)*(3/5))),(min+((100-min)*(4/5))),100) #8
    shades<-c("gray68","#ffffcc","#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }
  else if(min !=0 & max !=100){
    vec<-c(0,0.49,0.51,min-1,(min+((100-min)*(1/5))),(min+((100-min)*(2/5))),(min+((100-min)*(3/5))),(min+((100-min)*(4/5))),max,100) #9
    shades<-c("gray68","#ffffcc","#c7e9b4","#78c679","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#0c2c84")
  }

  percents<-as.numeric(cut(var,vec))
  cty<-percents[match(county.fips$fips,data[,2])]
  fills <- shades[cty]
  
  ## for map legends
  inc <- (max - min) / 5
  if (min !=0 & max !=100) {
      legend.txt<-c(
        "No Physicians",
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
        "No Physicians",
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
        "No Physicians",
        "0%",
        paste(1,"-",round((0 + inc),0), "%"),
        paste(round((min+inc),0)+1,"-",round((min + 2 * inc),0), "%"),
        paste(round((min+2*inc),0)+1,"-",round((min + 3 * inc),0),"%"),
        paste(round((min+3*inc),0)+1,"-",round((min + 4 * inc),0),"%"),
        paste(round((min+4*inc),0)+1,"-",max,"%"),
        paste("More than",paste(max, "%",sep=""),sep=" ")
      )
  }
  else {
      legend.txt<-c("No Physicians","0%","1-20%","21-40%","41-60%","61-80%","81-100%")
  }
  
  if (state=="US"){

  # refresh map
  layout(matrix(c(4,2,2,3,3,4,1,1,1,1,4,1,1,1,1), 5), c(1,3), c(1,3))
  par(mar=c(0,0,0,0))
  par(oma=c(0,1,0,0))
  map("state",col="white",fill=F,
     lty = 1, lwd = 1, mar=c(0,0,0,0),
     myborder = 0)
  
  # plot choropleth map
  par(mar=c(0,0,0,0))
  ctyData (cty,shades)
  #cmap("county", fill = TRUE, col = fills, 
    # resolution = 0, lty = 0, 
    # myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "#bdbdbd", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, 
    myborder = 0,mar=c(0,0,0,0),resolution=0)

  # alaska plot
  par(mar=c(0,0,0,0))
  plot(st_geometry(ak),
       col=shades[as.numeric(cty[as.numeric(rownames(county.fips[county.fips$state=="Alaska",])[1]):as.numeric(rownames(county.fips[county.fips$state=="Alaska",])[nrow(county.fips[county.fips$state=="Alaska",])])])],
       border="#bdbdbd",asp=2.5)
  #box(bty="]",col="darkgray")
  
  # hawaii plot
  par(mar=c(0,4,0,0))
  plot(st_geometry(hi),
       col=shades[as.numeric(cty[as.numeric(rownames(county.fips[county.fips$state=="Hawaii",])[1]):as.numeric(rownames(county.fips[county.fips$state=="Hawaii",])[nrow(county.fips[county.fips$state=="Hawaii",])])])],
       border="#bdbdbd",asp=1.5)
  
  # legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  legend("bottomleft",
         legend = legend.txt, xpd=NA,
         fill = shades,text.font=2,horiz=T,
         title = "",
         cex=1.5,text.col="#525252",box.col=NA,bty="n",inset=c(0,0.25))
  #legend("bottomleft",
  #       legend = "", xpd=NA,text.font=2,horiz=T,
  #       title = paste0("Percent of all Office-based Physicians that have Demonstrated\n","Meaningful Use through Medicare | December 2015"),
  #       cex=2.25,text.col="#253494",box.col=NA,bty="n",inset=c(0,0.25))
  usr <- par("usr")
  
  text(
    x = usr[1] + 0.01 * diff(usr[1:2]),
    y = usr[4] - 0.05 * diff(usr[3:4]),
    labels = paste0(
      "Percent of all Office-based Physicians that have Demonstrated\n",
      "Meaningful Use through Medicare | December 2015"
    ),
    adj = c(0, 1),
    font = 2,
    cex = 2,
    col = "#253494"
  )
  
  mtext("Source: CMS EHR Incentive Program data, 2015 and SK&A Office-based Provider Database, 2013",outer=T,side=1,line=-4,adj=0,cex=0.90,col="#525252")
  mtext("Note: Ambulatory medical doctors (MD) and doctors of osteopathy (DO) only",outer=T,side=1,line=-2,adj=0,cex=0.90,col="#525252")
  }
  
  else{
    
    layout(matrix(c(6,5,5,5,3,6,5,5,5,4,6,5,5,5,7,6,5,5,5,7,6,5,5,5,7,6,5,5,5,7,1,1,1,1,2,1,1,1,1,2,1,1,1,1,2,1,1,1,1,2), nrow=5,ncol=10),c(1,1),c(1,1.5))
    
    if(state !="Alaska" & state !="Hawaii"){
    par(mar=c(0,0,0,0))
    par(oma=c(0,1,0,1))
    map("state",region=c(state),col="white",fill=F,
        lty = 1, lwd = 1, mar=c(0,0,0,0),
        myborder = 0)
    
    # plot choropleth map
    par(mar=c(0,0,0,0))
    ctyData (cty,shades)
    
    # overlay state borders
    map("county", region=c(state),col = "gray", fill = FALSE, add = TRUE,
        lty = 1, lwd = 1, 
        myborder = 0,mar=c(0,0,0,0),resolution=0)
    
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
    }
  else if (state=="Alaska"){
    par(mar=c(0,0,0,0))
	par(oma=c(0,1,0,1))
    plot(st_geometry(ak),
         col=shades[as.numeric(cty[as.numeric(rownames(county.fips[county.fips$state=="Alaska",])[1]):as.numeric(rownames(county.fips[county.fips$state=="Alaska",])[nrow(county.fips[county.fips$state=="Alaska",])])])],
         border="gray",asp=2.5)
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
  }
  else {
    par(mar=c(0,0,0,0))
	par(oma=c(0,1,0,1))
    plot(st_geometry(hi),
         col=shades[as.numeric(cty[as.numeric(rownames(county.fips[county.fips$state=="Hawaii",])[1]):as.numeric(rownames(county.fips[county.fips$state=="Hawaii",])[nrow(county.fips[county.fips$state=="Hawaii",])])])],
         border="gray",asp=1.5)
    # legend
    par(mar=c(0,0,0,0))
    plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
    legend("top",
           legend=legend.txt,xpd=NA,
           fill=shades,text.font=2,horiz=F,
           title="",ncol=3,
           cex=1.5,text.col="#525252",box.col=NA,bty="n",border="#525252")
  }
  ## for pie graphs
  urbanSt<-(sum(data[data[,6]=="urban",][data[data[,6]=="urban",]$state==state,][,4]))*(1/(sum(data[data[,6]=="urban",][data[data[,6]=="urban",]$state==state,][,5])))
  urbanSt[urbanSt>1]<-1
  ruralSt<-(sum(data[data[,6]=="rural",][data[data[,6]=="rural",]$state==state,][,4]))*(1/(sum(data[data[,6]=="rural",][data[data[,6]=="rural",]$state==state,][,5])))
  ruralSt[ruralSt>1]<-1
  urbanSt[is.na(urbanSt)]<-0
  ruralSt[is.na(ruralSt)]<-0
  if(urbanSt>=(urban+0.025)){
    fills.urban<-"forestgreen"
  }
  else if (urbanSt<=(urban-0.025)){
    fills.urban<-"firebrick3"
  }
  else if (urbanSt==0){
    fills.urban<-"gray"
  }
  else{
    fills.urban<-"yellow3"
  }
  if(ruralSt>=(rural+0.025)){
    fills.rural<-"forestgreen"
  }
  else if (ruralSt<=(rural-0.025)){
    fills.rural<-"firebrick3"
  }
  else if (ruralSt==0){
    fills.rural<-"gray"
  }
  else{
    fills.rural<-"yellow3"
  }
  par(mar=c(1,0,0,0))
  pie(c(urbanSt,1-urbanSt),
      labels=NA,
      col=c(fills.urban,"gray"),
      main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*urbanSt),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Urban",line=-0.5,cex.sub=1.75,font.sub=2)
  pie(c(ruralSt,1-ruralSt),
      labels=NA,
      col=c(fills.rural,"gray"),
      main=NA,lty=0,font=2,cex.main=2)
  text(0,0,paste(round((100*ruralSt),0),"%",sep=""),col="white",font=2,cex=2.5)
  title(sub="Rural",line=-0.5,cex.sub=1.75,font.sub=2)
  par(mar=c(2,3.25,0,0))
  fills<-c()
  for(i in 1:length(stPaid1[,1])){
    if(stPaid1[,1][i]==state){
      if(stPaid1[,2][i]>=(national+0.025)){
        fills<-c(fills,"forestgreen")
      }
      else if (stPaid1[,2][i]<=(national-0.025)){
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
  barplot(stPaid1[,2],horiz=F,ylim=c(0,1),border=NA,yaxt="n",col=fills)
  axis(side=2,line=-1,las=2,at=c(0,.25,.5,.75,1),labels=c(0,"25%","50%","75%","100%"),col=NA,col.ticks="gray",font=2,cex.axis=1.5)
  mids<-c(0.7,1.9,3.1,4.3,5.5,6.7,7.9,9.1,10.3,11.5,12.7,13.9,15.1,16.3,17.5,18.7,19.9,21.1,22.3,23.5,24.7,25.9,27.1,28.3,29.5,30.7,31.9,33.1,34.3,35.5,36.7,37.9,39.1,40.3,41.5,42.7,43.9,45.1,46.3,47.5,48.7,49.9,51.1,52.3,53.5,54.7,55.9,57.1,58.3,59.5,60.7)
  axis(side=1,line=-0.5,las=2,at=mids,labels=stPaid1[,3],col=NA,col.ticks=NA,font=2,cex.axis=1.35)
  text(30.7,0.95,paste(paste(round((100*stPaid1[stPaid1[,1]==state,][,2]),0),"%",sep=""),"of",state,"office-based physicians",sep=" "),col=fills[as.numeric(rownames(stPaid1[stPaid1[,1]==state,]))],cex=2,font=2,adj=0.5)
  text(30.7,0.90, "have demonstrated meaningful use through Medicare",col=fills[as.numeric(rownames(stPaid1[stPaid1[,1]==state,]))],cex=2,font=2,adj=0.5)
  ## plot area 6
  #plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  par(mar = c(0, 0, 0, 0))
  plot.new()
  mtext(paste("Percent of",state,"Office-based Physicians that have Demonstrated Meaningful Use",sep=" "),outer=T,line=-2,side=3,adj=0,font=2,cex=1,col='#253494')
  mtext("Participation in Medicare EHR Incentive Program only through December 2015",outer=T,line=-4,side=3,adj=0,font=1,cex=0.9,col='#525252')
  mtext("Source: CMS EHR Incentive Program data, 2015 and SK&A Office-based Provider Database, 2013",outer=T,line=-6,side=3,adj=0,font=1,cex=0.85,col="#525252")
  mtext("Note: Ambulatory medical doctors (MD) and doctors of osteopathy (DO) only",outer=T,line=-8,side=3,adj=0,font=1,cex=0.85,col="#525252")
  # legend
  par(mar=c(0,0,0,0))
  plot(0,0,type="n",yaxt="n",xaxt="n",bty="n")
  legend.txt<-c("<2.5 pct points below US Avg",
                "(+/-) 2.5 pct points of US Avg",
                ">2.5 pct points above US Avg")
  legend("top",
         legend = legend.txt, xpd=NA,
         fill = c("firebrick3","yellow3","forestgreen"),text.font=1,horiz=F,
         title="",
         cex=1.5,text.col="#525252",box.col=NA,bty="n",border=NA)
  }
}