#Trajectory data
# contact: Rinus Scheele (KNMI)
# Zeer vereerd dat er weer es iemand naar trajectories vraagt.
# Ik heb op het ECMWF een suite draait die ondermeer 2 maal daags trajectories voor het RIVM levert.
# Ik denk dat de meeste analyses wel aanwezig zijn. Hier staan ook temperatuur en vocht paramaters in.
# Technisch : de input komt van het operationele ECMWF model, data op 1 X 1 graad. 3 D transport, dus ook verticale verplaatsing is meegenomen.
# U,V en W(omega)M worden gebruikt voor de berekening.  De andere meteo parameters worden alleen geinterpoleerd.
# http://projects.knmi.nl/campaign_support/RIVM/ANA/traj.html
# Ik heb wel het idee dat ik een mercedes verkoop, terwijl je een fiets vraagt. Maar dit is van mij uit verstandig lui.
# Ik heb ook de data zelf op mijn LINUX.
# Zie /net/pc170542.knmi.nl/nobackup/users/scheele/RIVM/ANA

#Beschrijving van de files
#Voorbeeld python script: plottray.py
# Vroeger ( ik ben 60+) was geheugen schaars. Dus een format ontwikkeld dat weinig ruimte in neemt. Alle variabelen worden als integers geschreven.  Ooit begonnen met alleen de wind en de coordinaten is er in de loop der jaren een hele dierentuin aan parameters toegevoegd. Toen heb ik het format flexibel gemaakt, waarmee  de leesbaarheid voor fortran ( en nu python) beter werd, en voor mensen slechter.
# 
# Er kan een willekeurig aantal trajectories achter elkaar staan. Elk weer met een eigen header. Mijn software veronderstelt wel altijd de zelfde info: dus het laatste stringetje blijft hetzelfde.
# 
# Het laatste stringetje van de header geeft aan wat er in de kolommen staat. Elke kolom heeft een letter.
# Als we het over dezelfde file hebben:
#   HTA : A betekent precieser format voor Lon Lat , Pressure.   De eerste start dus vanaf  4.700 oosterlangte, 53.000  noorderbreedte, 939.50 hPa
# Geen regen, geen sneeuw. Temperatuur 274,7 K. Vocht 3685  mg/kg.
# De grenslaag is 881 m. dik. De verificatie  is op 500m boven de model orography.
# 
# OAPTQLRSHF
# Longitude, Latitude, Pressure : de 3D coordinaat.
# T:  Temperatuur
# Q :vocht
# L   : Large scale precipitation
# R : Convective precipitation
# S : Snowfall
# H : Boundary layer height
# F : error flag.
# 
# De eenheden en de betekenis staan in de beschrijving. 

#Naamgeving van de files: *_%HEIGHT_%Year%yearday%hour_*
library(trajr)
df<-list.files("D:/data/trajectory/RIVM_500_20030310_AN_TR/",full.names=TRUE)
df<-stack(df)

