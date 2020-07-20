#level 1.5 product is available for the recent period (no level 2 data)
#check https://aeronet.gsfc.nasa.gov for more information
library(data.table)
library(ggplot2)
library(lubridate)

read_AOD_440nm<-function(aeronet_file){
  df<-fread(aeronet_file)
  df$`Date(dd:mm:yyyy)`<-gsub(":","-",df$`Date(dd:mm:yyyy)`)
  df$time<-paste(df$`Date(dd:mm:yyyy)`,df$`Time(hh:mm:ss)`)
  df$time<-as.POSIXct(df$time,format="%d-%m-%Y %H:%M:%S")
  df<-subset(df,select=c(time,AOD_440nm))
  df[which(df$AOD_440nm==-999),]<-NA
  df<-df[complete.cases(df),]
  names(df)<-c("time","AOD")
  return(df)
}

read_AOD_500nm<-function(aeronet_file){
  df<-fread(aeronet_file)
  df$`Date(dd:mm:yyyy)`<-gsub(":","-",df$`Date(dd:mm:yyyy)`)
  df$time<-paste(df$`Date(dd:mm:yyyy)`,df$`Time(hh:mm:ss)`)
  df$time<-as.POSIXct(df$time,format="%d-%m-%Y %H:%M:%S")
  df<-subset(df,select=c(time,AOD_500nm))
  df[which(df$AOD_500nm==-999),]<-NA
  df<-df[complete.cases(df),]
  names(df)<-c("time","AOD")
  return(df)
}

#5 minute data
aeronet_cabauw_5min<-"D:/data/AERONET/20150101_20200613_Cabauw.lev15"
df<-read_AOD_500nm(aeronet_file = aeronet_cabauw_5min)
df$yday<-yday(df$time)
ggplot(df,aes(time,AOD))+geom_point()+ggtitle("AOD 500nm Cabauw level 1.5")

df.covid19<-df[which(df$time>as.POSIXct("2020-03-15")),]

mean(df.covid19$AOD)
sd(df.covid19$AOD)

# df.normal<-df[which(df$time<as.POSIXct("2020-03-15")),]
# df.normal<-df.normal[df.normal$yday %in% unique(df.covid19$yday),]
# mean(df.normal$AOD)
# sd(df.normal$AOD)

#Daily averages
aeronet_cabauw_daily<-"D:/data/AERONET/daily_averages/20150101_20200613_Cabauw.lev15"
df<-read_AOD_500nm(aeronet_file = aeronet_cabauw_daily)
ggplot(df,aes(time,AOD_500nm))+geom_point()+ggtitle("AOD 500nm Cabauw level 1.5")


df.covid19<-df[which(df$time>as.POSIXct("2020-03-15")),]
mean(df.covid19$AOD)
sd(df.covid19$AOD)
