library(data.table)
library(ggplot2)
library(viridis)
library(metR)

source("R/qq_quality_control_functions.R")
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

meta<-fread("data/KNMI_meta.txt")
df<-fread("data/KNMI_20200614.txt",skip=60)
names(df)<-c("STN","Date","Q","CC")
df<-df[complete.cases(df)]
df$Date<-as.Date(as.character(df$Date),format="%Y%m%d")

df<-df[which(df$CC==0 | df$CC==1),]
df$Q<-df$Q/8.64 #Convertion J/cm2/sec to W/m2

df.com<-merge(meta,df,by="STN")
names(df.com)<-c("STN","lon","lat","alt","name","Date","Q","CC")

#Overview of the data
ggplot(df,aes(Date,Q))+geom_point()+
  theme_bw()+ggtitle("Clear sky measurements from the AWSs")+
  xlab("")+
  ylab(expression(W/m^2))

#De Bilt
test<-df.com[which(df.com$STN==260),]

get_zenith<-function(df){
zenith_mean<-mapply(global_radiation_quality_check,
                    timedate = df$Date,
                    MoreArgs = list(lat = unique(df$lat),
                                    lon = unique(df$lon)),
                    SIMPLIFY = FALSE)
zenith_mean<-data.frame(matrix(unlist(zenith_mean),
                  nrow=length(zenith_mean),
                  byrow=TRUE))
names(zenith_mean)<-c("zenith","Qmax","Qmin","Qmax_zen","Qrare_zen")

zenith_mean<-data.frame(zenith_mean,"Date"=df$Date)

df.Bilt<-merge(df,zenith_mean,by="Date")
df.Bilt$color<-"black"
df.Bilt$color[which(df.Bilt$Date>as.Date("2020-03-15"))]<-"red"
return(df.Bilt)
}

df.com<-subset(df.com,select = c(STN,lon,lat,Date,Q,CC))
df<-by(df.com,df.com$STN,get_zenith)
df<-do.call("rbind",df)

df_normal<-df[which(df$color=="black"),]
df_covid<-df[which(df$color=="red"),]

#Test if the two time series are significantly different with a chow test
library(gap)
chow.test(y1=df_normal$Q,x1=df_normal$zenith,
          y2=df_covid$Q,x2=df_covid$zenith)
#

df_normal$density <- get_density(df_normal$zenith, df_normal$Q, n = 50)
df_covid$density <- get_density(df_covid$zenith, df_covid$Q, n = 50)

ggplot(df_normal,aes(zenith,Q,colour=df_normal$density))+
  ggtitle("20 AWS stations")+
  theme_bw()+
  theme(legend.position = "none")+
  geom_point()+
  scale_color_viridis() +
  scale_x_reverse()+
  geom_smooth()+
  geom_point(data=df_covid,aes(zenith,Q),color="red")

#Fit a polynome through the data
RMSE<-function(pred,obs){sqrt(mean((pred - obs)^2))}

QZ_model <- lm(Q~zenith+I(zenith^2)+I(zenith^3), data=df_normal)
df_normal$pred <- predict(QZ_model) # estimating the model and obtaining the fitted values from the model
RMSE(df_normal$pred,df_normal$Q)

conf<-data.frame(predict(QZ_model,interval = "confidence", level = 0.95))
df_normal$lwr<-conf$lwr
df_normal$upr<-conf$upr
#optimize fit by changing y intercept
QZ_model2<-QZ_model


df_covid$pred<-predict(QZ_model,newdata = df_covid)
error0<-RMSE(df_covid$pred,df_covid$Q)
# error<-RMSE(df_normal$pred,df_normal$Q)

while(error0>=error){
  error0<-error
  QZ_model2$coefficients[1]<-QZ_model2$coefficients[1]+1
  df_covid$pred<-predict(QZ_model2,newdata = df_covid)
  error<-RMSE(df_covid$pred,df_covid$Q)
  
}
conf<-data.frame(predict(QZ_model2,newdata = df_covid,interval = "confidence", level = 0.95))
df_covid$lwr<-conf$lwr
df_covid$upr<-conf$upr

ydiff<-QZ_model$coefficients[1]-QZ_model2$coefficients[1]

ggplot(df_normal,aes(zenith,Q))+ #colour=df_normal$density
  ggtitle("Heldere dagen van 20 automatische weer stations")+
  theme_bw()+
  #theme(legend.position = "none")+
  geom_point(colour="grey")+
  scale_color_viridis() +
  scale_x_reverse()+
  geom_point(data=df_covid,aes(zenith,Q,color=as.character(as.numeric(Date))))+ #,color="red"
  scale_color_manual(name="dagen",values=viridis(n=length(unique(df_covid$Date))),
                     labels=unique(df_covid$Date))+
  geom_line(aes(zenith,pred),col="blue",lwd=1)+
  geom_line(data=df_covid,aes(zenith,pred),col="red",lwd=1)
  #geom_line(aes(zenith,lwr),col="blue",lwd=0.2)+
  #geom_line(aes(zenith,upr),col="blue",lwd=0.2)+
  #geom_line(data=df_covid,aes(zenith,lwr),col="red",lwd=0.2)+
  #geom_line(data=df_covid,aes(zenith,upr),col="red",lwd=0.2)
