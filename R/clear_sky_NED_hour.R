library(data.table)
library(ggplot2)
library(viridis)
library(metR)

source("R/qq_quality_control_functions.R")
source("R/clear_sky_functions.R")
t.start<-as.Date("2020-05-01")
t.stop<-as.Date("2020-06-15")
get_new_zenith=FALSE
subset_direction=TRUE
zero_only=TRUE
wind_dir="north"

if(get_new_zenith==TRUE){
df.com<-get_df_zenith(data_file="data/uurdata_Q_Ng.csv")
df.com<-df.com[complete.cases(df.com),]
saveRDS(df.com,file = "/data/df_hourly.rds")
write.table(df.com,file="data/AWS_Q_CC_hour_2015_2020.csv",col.names=TRUE,row.names=TRUE,sep=",")
}

df.com<-readRDS("/data/df_hourly.rds")

if(subset_direction==TRUE){
  df.wind<-fread("data/uurdata_wind_2015_2020.csv")
  names(df.wind)<-c("Date","STN","dir","var")
  df.wind<-subset(df.wind,select = c("Date","STN","dir"))
  df.wind<-df.wind[complete.cases(df.wind),]
  
  if(wind_dir=="north"){
    df.wind1<-df.wind[which(df.wind$dir>320),]
    df.wind2<-df.wind[which(df.wind$dir<40),]
    df.wind<-rbind(df.wind1,df.wind2)
  }
  if(wind_dir=="east"){
  df.wind<-df.wind[which(df.wind$dir>60 & df.wind$dir<110),]
  }
  if(wind_dir=="south"){
    df.wind<-df.wind[which(df.wind$dir>140 & df.wind$dir<220),]
  }
  if(wind_dir=="west"){
    df.wind<-df.wind[which(df.wind$dir>240 & df.wind$dir<300),]
  }
  df.wind$Date<-as.POSIXct(as.character(df.wind$Date),format="%Y%m%d_%H0000_000000")
  df.wind$STN<-gsub("_H","",df.wind$STN)
  df.com<-merge(df.com,df.wind,by=c("STN","Date"))
}

if(zero_only==TRUE){
df.com<-df.com[df.com$CC %in% 0,]
}
df.com$hoek<-90-df.com$zenith
df.com$zenith<-cos(df.com$zenith*pi/180)#in rad
# df.com<-df.com[which(df.com$zenith>0.15),] #used by Reinout
df.com$color<-"black"
df.com$color[which(df.com$Date>t.start & df.com$Date<t.stop)]<-"red"
df.com$yday<-yday(df.com$Date)

nday<-data.frame(1:366)
names(nday)<-"yday"
scon<-lapply(nday,FUN=solar_constant)
Sa<-data.frame(scon$yday$Sa)
d<-data.frame(scon$yday$d)
df.scon<-data.frame(cbind(Sa,d,nday))
names(df.scon)<-c("Sa","d","yday")
df.com<-dplyr::full_join(df.com,df.scon,by="yday")

QQ<-fread("data/uurdata_Q_2015_2020.csv")
names(QQ)<-c("Date","STN","Q","VAR")
QQ<-subset(QQ,select=c("Date","STN","Q"))
QQ<-QQ[complete.cases(QQ)]
QQ$Date<-as.POSIXct(as.character(QQ$Date),format="%Y%m%d_%H0000_000000")
QQ$STN<-gsub("_H","",QQ$STN)
df.com<-merge(df.com,QQ,by=c("Date","STN"))

#Overview of the data
df_normal<-df.com[which(df.com$color=="black"),]
df_covid<-df.com[which(df.com$color=="red"),]

#subset for the same time period in the year
df_normal<-df_normal[df_normal$yday %in% unique(df_covid$yday),]

#Divide by sunearth distance constant to 'normalize' Q
df_normal$Q<-df_normal$Q.y/df_normal$d
df_covid$Q<-df_covid$Q.y/df_covid$d

#Fit a exponent through the data
#simplified model
S0=1360
#Bears law does not converge with nls using a as a non-constant (a should become Sa (or Stoa*f))
QZ_model <- nls(Q ~ (S0*zenith)*exp(-b/zenith), data = df_normal,start = list(b = 1))
QZ_model2 <- nls(Q ~ (S0*zenith)*exp(-b/zenith), data = df_covid,start = list(b = 1))

#S4 van Reinout
QZ_log_model <- nls(zenith*log(Q/zenith) ~ a+log(S0)*zenith, data = df_normal,start = list(a=0))
QZ_log_model2 <- nls(zenith*log(Q/zenith) ~ a+log(S0)*zenith, data = df_covid,start = list(a=0))

#S5 van Reinout
QZ_model <- nls(Q.y ~ exp((a+b*zenith)/zenith+log(zenith)), data = df_normal,start = list(a = 0, b = 5))
QZ_model2 <- nls(Q.y ~ exp((a+b*zenith)/zenith+log(zenith)), data = df_covid,start = list(a = 0,b=5))
#
df_normal$pred <- predict(QZ_model) # estimating the model and obtaining the fitted values from the model
df_covid$pred <- predict(QZ_model2) # estimating the model and obtaining the fitted values from the model

# b=0.9
# df_normal$pred<-(1360*df_normal$zenith)*exp(-b/df_normal$zenith)
# df_covid$pred<-(1360*df_covid$zenith)*exp(-b/df_covid$zenith)

ggplot(df_normal,aes(zenith,Q))+ #colour=df_normal$density
  #ggtitle("Heldere  van 20 automatische weer stations")+
  theme_bw()+
  #theme(legend.position = "none")+
  geom_point(colour="grey")+
  scale_color_viridis() +
  #scale_x_reverse()+
  xlab("cos(Z)") +
  geom_point(data=df_covid,aes(zenith,Q),color="red")+ #,color="red"
  # scale_color_manual(name="dagen",values=viridis(n=length(unique(df_covid$Date))),
  #                    labels=unique(df_covid$Date))+
  geom_smooth(aes(zenith,pred),col="black",lwd=1)+
  geom_smooth(data=df_covid,aes(zenith,pred),col="yellow",lwd=1)+
  scale_y_continuous(limits=c(0,1000),expand=c(0,0))
  # scale_x_continuous(limits=c(0,360))+
  # coord_polar(theta = "x") 
#geom_line(aes(zenith,lwr),col="blue",lwd=0.2)+
#geom_line(aes(zenith,upr),col="blue",lwd=0.2)+
#geom_line(data=df_covid,aes(zenith,lwr),col="red",lwd=0.2)+
#geom_line(data=df_covid,aes(zenith,upr),col="red",lwd=0.2)

#maximum difference between the 2 fits
cosZ<-seq(min(df_normal$zenith),max(df_normal$zenith),by=0.05)
cosZ<-data.frame(cosZ)
names(cosZ)<-"zenith"
normal<-predict(QZ_model,newdata = cosZ)
covid<-predict(QZ_model2,newdata = cosZ)
df<-data.frame(normal,covid)
df$dif<-(1-df$normal/df$covid)*100

#Density plot
df_normal$density <- get_density(df_normal$zenith, df_normal$Q, n = 50)
# df_covid$density <- get_density(df_covid$zenith, df_covid$Q, n = 50)

ggplot(df_normal,aes(zenith,Q,colour=df_normal$density))+
  ggtitle("20 AWS stations")+
  theme_bw()+
  theme(legend.position = "none")+
  geom_point()+
  scale_color_viridis() +
  scale_x_reverse()+
  geom_smooth()+
  geom_point(data=df_covid,aes(zenith,Q),color="red")
# RMSE(df_normal$pred,df_normal$Q)

# conf<-data.frame(predict(QZ_model,interval = "confidence", level = 0.95))
# df_normal$lwr<-conf$lwr
# df_normal$upr<-conf$upr
#free parameters 



# conf<-data.frame(predict(QZ_model2,newdata = df_covid,interval = "confidence", level = 0.95))
# df_covid$lwr<-conf$lwr
# df_covid$upr<-conf$upr
