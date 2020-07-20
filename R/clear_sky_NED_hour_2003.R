library(data.table)
library(ggplot2)
library(lubridate)
library(viridis)
library(metR)

source("R/qq_quality_control_functions.R")
source("R/clear_sky_functions.R")
get_new_zenith=FALSE

if(get_new_zenith==TRUE){
df.com<-get_df_zenith()
df.com<-df.com[complete.cases(df.com),]
saveRDS(df.com,file = "/data/df_hourly_2003.rds")
write.table(df.com,file="data/AWS_Q_CC_hour_1998_2003.csv",col.names=TRUE,row.names=TRUE,sep=",")
}

df.com<-readRDS("/data/df_hourly_2003.rds")
df.com$zenith<-cos(df.com$zenith*pi/180)#in rad
df.com$color<-"black"
df.com$color[which(df.com$Date>as.Date("2003-04-01") & df.com$Date<as.Date("2003-05-01"))]<-"red"

#Overview of the data
df_normal<-df.com[which(df.com$color=="black"),]
df_covid<-df.com[which(df.com$color=="red"),]

#Fit a exponent through the data
QZ_model <- nls(Q ~ (a*zenith)*exp(b/zenith), data = df_normal,start = list(a = 1350, b = -1))

df_normal$pred <- predict(QZ_model) # estimating the model and obtaining the fitted values from the model
RMSE(df_normal$pred,df_normal$Q)

conf<-data.frame(predict(QZ_model,interval = "confidence", level = 0.95))
df_normal$lwr<-conf$lwr
df_normal$upr<-conf$upr

#free parameters 
QZ_model2 <- nls(Q ~ (a*zenith)*exp(b/zenith), data = df_covid,start = list(a = 1350, b = -1))
df_covid$pred <- predict(QZ_model2) # estimating the model and obtaining the fitted values from the model


conf<-data.frame(predict(QZ_model2,newdata = df_covid,interval = "confidence", level = 0.95))
df_covid$lwr<-conf$lwr
df_covid$upr<-conf$upr

ydiff<-QZ_model$coefficients[1]-QZ_model2$coefficients[1]

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
  geom_line(aes(zenith,pred),col="black",lwd=1)+
  geom_line(data=df_covid,aes(zenith,pred),col="yellow",lwd=1)
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
