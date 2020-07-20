get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

#Calculate zenith per station via
zenith<-function(t.hour,lat,lon){
  #fix for sequence
  t.hour<-seq(from=t.hour,to=t.hour-lubridate::minutes(55),length.out = 5)  
  solar_angles <- sunPosition(time = t.hour,lat = lat,long = lon)
  if(length(which(solar_angles$altitude<0))>0){return(NA)}
  elevation_angles <- solar_angles$altitude[which(solar_angles$altitude>0)] #altitude is in degrees above the horizon
  zenith_angle<-mean(90-elevation_angles)
  return(zenith_angle)
}

get_df_zenith<-function(meta_file="data/KNMI_meta.txt",data_file="data/uurdata_Q_Ng.csv"){
  meta<-fread(meta_file)
  meta$STN<-as.character(meta$STN)
  
  df<-fread(data_file)
  names(df)<-c("Date","STN","CC","CCmax","Q","VAR")
  df<-subset(df,select=c("Date","STN","CC","Q"))
  df<-df[complete.cases(df)]
  
  
  df<-df[which(df$CC==0 | df$CC==1),]
  df$Date<-as.POSIXct(as.character(df$Date),format="%Y%m%d_%H0000_000000")
  df$STN<-gsub("_H","",df$STN)
  
  
  # df$Q<-df$Q/8.64 #Convertion J/cm2/sec to W/m2
  
  df.com<-merge(meta,df,by="STN")
  names(df.com)<-c("STN","lon","lat","alt","name","Date","CC","Q")
  #apply the function to the 
  df.com<-df.com[complete.cases(df.com),]
  #test with a loop before mapply for debugging 
  df.com$zenith<-NA
  for(i in 1:nrow(df.com)){
    df.com$zenith[i]<-try(zenith(df.com$Date[i], df.com$lat[i],df.com$lon[i])) #do something with try
  }
  #faster way 
  # df.com$zenith<-mapply(zenith, df.com$Date, df.com$lat,df.com$lon)
  return(df.com)
}