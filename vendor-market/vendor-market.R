## creates data frame with a variable combining all vendors for a unique provider in each program year
venFunc<-function(data,year){
  if(year=="2011"){
    num<-7}
  else if(year=="2012"){
    num<-8}
  else if(year=="2013"){
    num<-9}
  else if(year=="2014"){
    num<-10}
  else if(year=="2015"){
    num<-11}	
  yrDF<-data[data[,num] !=0,]
  if(year=="2011"){
    yrDF<-yrDF[yrDF$prgm_yr=="2011",]
  }
  if(year=="2012"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,8]==1,][yrDF[yrDF[,8]==1,]$prgm_yr=="2012",],
      yrDF[yrDF[,8]==2,][yrDF[yrDF[,8]==2,]$prgm_yr=="2011",]
    ))
  }
  if(year=="2013"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,9]==1,][yrDF[yrDF[,9]==1,]$prgm_yr=="2013",],
      yrDF[yrDF[,9]==2,][yrDF[yrDF[,9]==2,]$prgm_yr=="2012",],
      yrDF[yrDF[,9]==3,][yrDF[yrDF[,9]==3,]$prgm_yr=="2011",]
    ))
  }
  if(year=="2014"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,10]==1,][yrDF[yrDF[,10]==1,]$prgm_yr=="2014",],
      yrDF[yrDF[,10]==2,][yrDF[yrDF[,10]==2,]$prgm_yr=="2013",],
      yrDF[yrDF[,10]==3,][yrDF[yrDF[,10]==3,]$prgm_yr=="2012",],
      yrDF[yrDF[,10]==4,][yrDF[yrDF[,10]==4,]$prgm_yr=="2011",]
    ))
  }
  if(year=="2015"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,11]==1,][yrDF[yrDF[,11]==1,]$prgm_yr=="2015",],
      yrDF[yrDF[,11]==2,][yrDF[yrDF[,11]==2,]$prgm_yr=="2014",],
      yrDF[yrDF[,11]==3,][yrDF[yrDF[,11]==3,]$prgm_yr=="2013",],
      yrDF[yrDF[,11]==4,][yrDF[yrDF[,11]==4,]$prgm_yr=="2012",],
	  yrDF[yrDF[,11]==5,][yrDF[yrDF[,11]==5,]$prgm_yr=="2011",]
    ))
  }
  x<-dcast(yrDF,id~vendor,value.var="pymt_yr")
  x1<-split(x,x[,1])
  x2<-lapply(x1,function(x) x[ , ! apply( x , 2 , function(x) all(is.na(x)) ) ])
  venPaste<-function(x){
    y<-matrix(colnames(x[2:ncol(x)]),ncol=length(colnames(x[2:ncol(x)])))
    if(ncol(y)==1){
      ven<-y[1]
    }
    else if(ncol(y)==2){
      ven<-paste(y[1],y[2],sep="/&")
    }
    else if(ncol(y)==3){
      ven<-paste(y[1],y[2],y[3],sep="/&")
    }
    else if(ncol(y)==4){
      ven<-paste(y[1],y[2],y[3],y[4],sep="/&")
    }
    else if(ncol(y)==5){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],sep="/&")
    }
    else if(ncol(y)==6){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],sep="/&")
    }
    else if(ncol(y)==7){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],sep="/&")
    }
    else if(ncol(y)==8){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],sep="/&")
    }
    else if(ncol(y)==9){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],sep="/&")
    }
    else if(ncol(y)==10){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],y[10],sep="/&")
    }
    return(ven)
  }
  x3<-lapply(x2,function(x) venPaste(x))
  ven<-matrix(unlist(x3))
  df<-data.frame(x$id,ven)
  df<-data.frame(df,combo=grepl("/&",df$ven,fixed=TRUE),state=yrDF$state[match(df[,1],yrDF[,1])])
  return(df)
}
############
func<-function(data,year,vendor,standalone,combo){
  out<-tryCatch(
    venMkt(data=data,year=year,vendor=vendor,standalone=standalone,combo=combo),
    error = function(e){
      data.frame(state="",yrTot="",venTot="",prop="",ven="")
    }
  )
  return(out)
}
############
venMkt<-function(data,year,vendor,standalone=FALSE,combo=FALSE){
  data<-data[data[,2]==year,]
  idTots<-data.frame(table(data[,1]))
  idTots<-data.frame(idTots[,1],data$state[match(idTots[,1],data[,1])])
  stTots<-data.frame(table(idTots[,2]))
  if(standalone==TRUE & combo==FALSE){
    df<-data[data$combo==FALSE,]
    venTots<-data.frame(table(df[df$ven==vendor,]$state))
    yrTots<-data.frame(stTots,(venTots[,2][match(stTots[,1],venTots[,1])]),round((100*(venTots[,2][match(stTots[,1],venTots[,1])]*(1/stTots[,2]))),2),rep(vendor,nrow(stTots)))
    colnames(yrTots)<-c("state","yrTot","venTot","prop","ven")
    yrTots[,2]<-as.numeric(yrTots[,2])
    yrTots[,3]<-as.numeric(yrTots[,3])
    yrTots<-data.frame(
      rbind(
        yrTots,data.frame(state="National",yrTot=sum(yrTots[,2],na.rm=T),venTot=sum(yrTots[,3],na.rm=T),prop=(round((100*(sum(yrTots[,3],na.rm=T)*(1/sum(yrTots[,2],na.rm=T)))),1)),ven=vendor)))
    yrTots[is.na(yrTots)]<-0
  }
  else if (standalone==FALSE & combo==TRUE){
    df<-data[data$combo==TRUE,]
	if(vendor=="Infor"){
	df<-data.frame(df,venY=grepl(vendor,df$ven,fixed=TRUE),venZ=grepl("Information",df$ven,fixed=T),venW=grepl("Informatics",df$ven,fixed=T))
	venTots<-data.frame(table(df[df$venY==TRUE & df$venZ==FALSE & df$venW==FALSE,]$state))
	}
	else{
    df<-data.frame(df,venY=grepl(vendor,df$ven,fixed=TRUE))
    venTots<-data.frame(table(df[df$venY==TRUE,]$state))
	}
    yrTots<-data.frame(stTots,(venTots[,2][match(stTots[,1],venTots[,1])]),round((100*(venTots[,2][match(stTots[,1],venTots[,1])]*(1/stTots[,2]))),2),rep(vendor,nrow(stTots)))
    colnames(yrTots)<-c("state","yrTot","venTot","prop","ven")
    yrTots[,2]<-as.numeric(yrTots[,2])
    yrTots[,3]<-as.numeric(yrTots[,3])
    yrTots<-data.frame(
      rbind(
        yrTots,data.frame(state="National",yrTot=sum(yrTots[,2],na.rm=T),venTot=sum(yrTots[,3],na.rm=T),prop=(round((100*(sum(yrTots[,3],na.rm=T)*(1/sum(yrTots[,2],na.rm=T)))),1)),ven=vendor)))
    yrTots[is.na(yrTots)]<-0
  }
  else if (standalone==FALSE & combo==FALSE){
    if(vendor=="Infor"){
	df<-data.frame(data,venY=grepl(vendor,data$ven,fixed=TRUE),venZ=grepl("Information",data$ven,fixed=T),venW=grepl("Informatics",data$ven,fixed=T))
	venTots<-data.frame(table(df[df$venY==TRUE & df$venZ==FALSE & df$venW==FALSE,]$state))
	}
	else{
    df<-data.frame(data,venY=grepl(vendor,data$ven,fixed=TRUE))
    venTots<-data.frame(table(df[df$venY==TRUE,]$state))
	}
    yrTots<-data.frame(stTots,(venTots[,2][match(stTots[,1],venTots[,1])]),round((100*(venTots[,2][match(stTots[,1],venTots[,1])]*(1/stTots[,2]))),2),rep(vendor,nrow(stTots)))
    colnames(yrTots)<-c("state","yrTot","venTot","prop","ven")
    yrTots[,2]<-as.numeric(yrTots[,2])
    yrTots[,3]<-as.numeric(yrTots[,3])
    yrTots<-data.frame(
      rbind(
        yrTots,data.frame(state="National",yrTot=sum(yrTots[,2],na.rm=T),venTot=sum(yrTots[,3],na.rm=T),prop=(round((100*(sum(yrTots[,3],na.rm=T)*(1/sum(yrTots[,2],na.rm=T)))),1)),ven=vendor)))
    yrTots[is.na(yrTots)]<-0
  }
  return(yrTots)
}
############
venRank<-function(st,size){
  x<-data.frame(table(st[[size]][,4]))
  x[,1]<-as.character(x[,1])
  x[,1]<-as.numeric(x[,1])
  x<-x[order(-x[,1]),]
  x1<-data.frame(prop=x[,1],order=c(1:nrow(x)))
  if(nrow(x1[x1[,1]==0,]) !=0){
    x1[x1[,1]==0,][,2]<-0
    df<-data.frame(state=st[[size]]$state,ven=st[[size]]$ven,prop=st[[size]]$prop,rank=x1[,2][match(st[[size]][,4],x1[,1])])
  }
  else{
    df<-data.frame(state=st[[size]]$state,ven=st[[size]]$ven,prop=st[[size]]$prop,rank=x1[,2][match(st[[size]][,4],x1[,1])])
  }
  return(df)
}
############
rankData<-function(file,data,year,standalone,combo){
  venCt<-data.frame(table(file$vendor))
  venCt[,1]<-as.character(venCt[,1])
  venDF<-ldply(lapply(venCt[,1], function(x) func(data,year,x,standalone,combo)),data.frame)
  venDF<-venDF[venDF[,1] !="",]
  venDF$prop<-as.numeric(venDF$prop)
  venDF<-venDF[order(venDF$state,-venDF$prop),]
  st<-split(venDF,venDF$state)
  size<-c(1:length(st))
  rank<-ldply(lapply(size,function(x) venRank(st,x)),data.frame)
  if(standalone==TRUE){
    rank<-data.frame(rank,type=rep("standalone",nrow(rank)))
  }
  else if (combo==TRUE){
    rank<-data.frame(rank,type=rep("combo",nrow(rank)))
  }
  else{
    rank<-data.frame(rank,type=rep("any",nrow(rank)))
  }
  rank<-data.frame(rank,year=rep(year,nrow(rank)))
  return(rank)
}
############
venCount<-function(data,year,vendor){
  data<-data[data[,2]==year,]
  strcount <- function(x, pattern, split){
    unlist(lapply(
      strsplit(x, split),
      function(z) na.omit(length(grep(pattern, z)))
    ))
  }
  venDF<-data.frame(data,venY=grepl(vendor,data[,3],fixed=TRUE))
  venDF<-venDF[venDF$venY==TRUE,]
  x<-data.frame(table(venDF[,3]))[order(data.frame(table(venDF[,3]))[,2],decreasing=T),]
  x[,1]<-as.character(x[,1])
  y<-strcount(x[,1],"/&"," ")
  y1<-y+1
  x1<-data.frame(rep(vendor,nrow(x)),gsub("/&","//",x[,1]),y1,x[,2])
  colnames(x1)<-c("vendor","vendor comboes","vendor count","frequency")
  return(x1)
}
## creates data frame with a variable combining all vendors for a unique provider in each program year
venFuncProfs<-function(data,year){
  if(year=="2011"){
    num<-7}
  else if(year=="2012"){
    num<-8}
  else if(year=="2013"){
    num<-9}
  else if(year=="2014"){
    num<-10}
  yrDF<-data[data[,num] !=0,]
  if(year=="2011"){
    yrDF<-yrDF[yrDF$prgm_yr=="2011",]
  }
  if(year=="2012"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,8]==1,][yrDF[yrDF[,8]==1,]$prgm_yr=="2012",],
      yrDF[yrDF[,8]==2,][yrDF[yrDF[,8]==2,]$prgm_yr=="2011",]
    ))
  }
  if(year=="2013"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,9]==1,][yrDF[yrDF[,9]==1,]$prgm_yr=="2013",],
      yrDF[yrDF[,9]==2,][yrDF[yrDF[,9]==2,]$prgm_yr=="2012",],
      yrDF[yrDF[,9]==3,][yrDF[yrDF[,9]==3,]$prgm_yr=="2011",]
    ))
  }
  if(year=="2014"){
    yrDF<-data.frame(rbind(
      yrDF[yrDF[,10]==1,][yrDF[yrDF[,10]==1,]$prgm_yr=="2014",],
      yrDF[yrDF[,10]==2,][yrDF[yrDF[,10]==2,]$prgm_yr=="2013",],
      yrDF[yrDF[,10]==3,][yrDF[yrDF[,10]==3,]$prgm_yr=="2012",],
      yrDF[yrDF[,10]==4,][yrDF[yrDF[,10]==4,]$prgm_yr=="2011",]
    ))
  }
  uI<-data.frame(table(yrDF$id))
  uI[,1]<-as.character(uI[,1])
  yrDF$numVen<-uI[,2][match(yrDF$id,uI[,1])]
  yrDF1<-yrDF[yrDF$numVen==1,]
  yrDF2<-yrDF[yrDF$numVen !=1,]  
  x<-dcast(yrDF2,id~vendor,value.var="pymt_yr")
  x1<-split(x,x[,1])
  x2<-lapply(x1,function(x) x[ , ! apply( x , 2 , function(x) all(is.na(x)) ) ])
  venPaste<-function(x){
    y<-matrix(colnames(x[2:ncol(x)]),ncol=length(colnames(x[2:ncol(x)])))
    if(ncol(y)==1){
      ven<-y[1]
    }
    else if(ncol(y)==2){
      ven<-paste(y[1],y[2],sep="/&")
    }
    else if(ncol(y)==3){
      ven<-paste(y[1],y[2],y[3],sep="/&")
    }
    else if(ncol(y)==4){
      ven<-paste(y[1],y[2],y[3],y[4],sep="/&")
    }
    else if(ncol(y)==5){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],sep="/&")
    }
    else if(ncol(y)==6){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],sep="/&")
    }
    else if(ncol(y)==7){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],sep="/&")
    }
    else if(ncol(y)==8){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],sep="/&")
    }
    else if(ncol(y)==9){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],sep="/&")
    }
    else if(ncol(y)==10){
      ven<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],y[10],sep="/&")
    }
    return(ven)
  }
  x3<-lapply(x2,function(x) venPaste(x))
  ven<-matrix(unlist(x3))
  df1<-data.frame(id=x$id,ven)
  df1<-data.frame(df1,combo=grepl("/&",df1$ven,fixed=TRUE),state=yrDF$state[match(df1[,1],yrDF[,1])])
  df2<-data.frame(id=yrDF1$id,ven=yrDF1$vendor,combo=rep("FALSE",nrow(yrDF1)),state=yrDF1$state)
  df<-rbind(df1,df2)
  return(df)
}