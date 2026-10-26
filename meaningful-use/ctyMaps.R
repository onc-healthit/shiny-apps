library(maps)
county.fips<-read.table("data/countyData.txt",colClasses="character",header=T,sep=",")
# ctyMap<-function(data,state){
# map("county",region=c(state),
#    col=colors[as.numeric(data[as.numeric(rownames(county.fips[county.fips$state==state,])[1]):as.numeric(rownames(county.fips[county.fips$state==state,])[nrow(county.fips[county.fips$state==state,])])])],
#    fill=T,add=T,lty=0)
# }
# states<-data.frame(table(county.fips$state))
# stList<-data.frame(paste(
#  "ctyMap(",
#  "met",
#  ",",
#  "'",
#  states[,1],
#  "'",
#  ")",
#  sep=""
#  ))
# write.table(stList,"stList.txt",row.names=F,sep=",")
ctyData<-function(data,colors) {
  ctyMap<-function(data,state){
    map("county",region=c(state),
        col=colors[as.numeric(data[as.numeric(rownames(county.fips[county.fips$state==state,])[1]):as.numeric(rownames(county.fips[county.fips$state==state,])[nrow(county.fips[county.fips$state==state,])])])],
        fill=T,add=T,lty=0,resolution=0,mar=c(0,0,0,0))
  }
ctyMap(data,'Alabama')
ctyMap(data,'Arizona')
ctyMap(data,'Arkansas')
ctyMap(data,'California')
ctyMap(data,'Colorado')
ctyMap(data,'Connecticut')
ctyMap(data,'Delaware')
ctyMap(data,'District Of Columbia')
ctyMap(data,'Florida')
ctyMap(data,'Georgia')
ctyMap(data,'Idaho')
ctyMap(data,'Illinois')
ctyMap(data,'Indiana')
ctyMap(data,'Iowa')
ctyMap(data,'Kansas')
ctyMap(data,'Kentucky')
ctyMap(data,'Louisiana')
ctyMap(data,'Maine')
ctyMap(data,'Maryland')
ctyMap(data,'Massachusetts')
ctyMap(data,'Michigan')
ctyMap(data,'Minnesota')
ctyMap(data,'Mississippi')
ctyMap(data,'Missouri')
ctyMap(data,'Montana')
ctyMap(data,'Nebraska')
ctyMap(data,'Nevada')
ctyMap(data,'New Hampshire')
ctyMap(data,'New Jersey')
ctyMap(data,'New Mexico')
ctyMap(data,'New York')
ctyMap(data,'North Carolina')
ctyMap(data,'North Dakota')
ctyMap(data,'Ohio')
ctyMap(data,'Oklahoma')
ctyMap(data,'Oregon')
ctyMap(data,'Pennsylvania')
ctyMap(data,'Rhode Island')
ctyMap(data,'South Carolina')
ctyMap(data,'South Dakota')
ctyMap(data,'Tennessee')
ctyMap(data,'Texas')
ctyMap(data,'Utah')
ctyMap(data,'Vermont')
ctyMap(data,'Virginia')
ctyMap(data,'Washington')
ctyMap(data,'West Virginia')
ctyMap(data,'Wisconsin')
ctyMap(data,'Wyoming')
}

