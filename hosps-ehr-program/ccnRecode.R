ccnRecode<-function(data,num){
  att_ccn<-c()
  for(i in 1:length(data[,num])){
    if(nchar(data[,num][i])==5){
      att_ccn<-c(att_ccn,paste("0",data[,num][i],sep=""))
    }
    else{
      att_ccn<-c(att_ccn,data[,num][i])
    }
  }
  return(att_ccn)
}