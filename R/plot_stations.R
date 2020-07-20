library(raster)
library(data.table)
library(mapview)
library(sf)
library(rgdal)
library(ggplot2)
library(ggspatial) #north arrow ggplot2
`%notin%` = Negate(`%in%`)

#country data: mask europe!
countries<-rgdal::readOGR(dsn="D:/natural_earth/ne_10m_admin_0_countries",layer="ne_10m_admin_0_countries")
countries<-spTransform(countries,CRS("+init=epsg:4326"))
europe<-countries[countries$CONTINENT == "Europe",]
europe_sf<-st_as_sf(europe)

#sta in
sta_in<-c("210", "215", "235", "240", 
          "260", "267", "269", "270", 
          "275", "279", "280", "290", 
          "310", "344", "350", "356",
          "370", "375", "377", "380")

#metadata from the stations
df_meta<-fread("data/KNMI_meta.txt")
names(df_meta)<-c("STN","lon","lat","elev","name")
df_meta<-df_meta[df_meta$STN %in% sta_in,]
# coordinates(df_meta)<-~lon+lat
# proj4string(df_meta)<-CRS("+init=epsg:4326")

p1<-ggplot() + 
  theme_bw()+
  geom_sf(data=europe_sf,fill="grey84")+
  geom_point(data=df_meta,aes(x=lon,y=lat),color="blue") +
  geom_rect(aes(xmin =3, xmax = 7.5, ymin = 50.5, ymax = 54),
            fill = "transparent", color = "red", size = 1)+
  scale_x_continuous(limits=c(-12,25), expand = c(0, 0)) +
  scale_y_continuous(limits=c(35,65), expand = c(0, 0)) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.13, "in"), pad_y = unit(3.4, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave(p1,filename="D:/Afbeeldingen/Europe.png",width=4,height=6)


background<-stack("D:/natural_earth/NE1_HR_LC/NE1_HR_LC.tif")
r1 = raster(xmn=-3,xmx=7.5,ymn=50.5,ymx=54,nrow=100,ncol=100)
background_crop<-crop(background,r1)
background_crop<-mask(background_crop,europe)

df_lab<-df_meta[df_meta$STN %notin% 215]
df_lab2<-df_meta[df_meta$STN %in% 215]

p2<-ggplot() + 
  theme_bw()+
  ggRGB(background_crop,r=1,g=2,b=3,ggLayer = TRUE)+
  geom_sf(data=europe_sf,fill="transparent")+
  geom_point(data=df_meta,aes(x=lon,y=lat),color="blue") +
  geom_text(data=df_lab,aes(x=lon,y=lat,label=STN),hjust=-0.1, vjust=0)+
  geom_text(data=df_lab2,aes(x=lon,y=lat,label=STN),hjust=1.1, vjust=0)+
  scale_x_continuous(limits=c(3,7.5), expand = c(0, 0)) +
  scale_y_continuous(limits=c(50.5,54), expand = c(0, 0)) 
ggsave(p2,filename="D:/Afbeeldingen/Netherlands.png",width=4.8,height=6)
