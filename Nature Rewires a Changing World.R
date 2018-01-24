#Nature Rewires a Changing World figures

library(tidyr)
library(ggplot2)
library(digitize)
library(magick)
library(cowplot)


####Figure 1####
#Read in data
North.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Rewiring-a-changing-world/master/NHemisphereTemp.csv")
North<-read.csv(text=North.URL)

South.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Rewiring-a-changing-world/master/SHemisphereTemp.csv")
South<-read.csv(text=South.URL)

North.mean<-subset(North, select = c(Year, J.D))
colnames(North.mean)[2] <- "North.Temp"
  
South.mean<-subset(South, select = c(Year, J.D))
colnames(South.mean)[2] <- "South.Temp"

Hemispheres<-merge(North.mean, South.mean, by="Year")

#convert to long
Hemispheres<-gather(Hemispheres, Hemisphere, Temperature, North.Temp:South.Temp)

#take out anomolies
Hemispheres<- Hemispheres[-which(Hemispheres$Temperature == "***"), ]
Hemispheres$Temperature<-as.numeric(as.character(Hemispheres$Temperature))

#plot figure
Hemispheres.plot<-ggplot(Hemispheres, aes(x = Year, y = Temperature, colour=Hemisphere, group=Hemisphere))+geom_line()+
  xlab("Year")+ylab("Mean temperature (C)")+theme_classic()+
  scale_color_manual(values=c("black","dark grey"))+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())

####Global temperature####
#Read in data
Global.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Rewiring-a-changing-world/master/GlobalTemp.csv")
Global<-read.csv(text=Global.URL, stringsAsFactors = FALSE)

Global.mean<-subset(Global, select = c(Year, J.D))

#take out anomolies
Global.mean<- Global.mean[-which(Global.mean$J.D == "***"), ]
Global.mean$J.D<-as.numeric(as.character(Global.mean$J.D))

Global.plot<-ggplot(Global.mean, aes(x = Year, y = J.D))+geom_line()+
  xlab("Year")+ylab("Mean temperature (C)")+theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())

####Lake temperature profile####
#read in Pine lake data 
TempProf.URL <- getURL("https://raw.githubusercontent.com/Monsauce/Rewiring-a-changing-world/master/TempProfile.csv")
TempProf<-read.csv(text=TempProf.URL)

Profile.plot<-ggplot(TempProf, aes(x = Temp, y = Depth))+geom_line()+
  xlab("Temperature (C)")+ylab("Depth (m)")+theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_y_reverse()


####Grass temperatures####
#read in supplementary figure 
BartonTemp<- ReadAndCal("BartonTemp.jpg")
GrassTemp<-DigitData(col = "red")
BartonTempDigital<- Calibrate(GrassTemp, BartonTemp, 20, 25, 0, 1)

colnames(BartonTempDigital)[1]<-"Height"
colnames(BartonTempDigital)[2]<-"Temp"

#make plot
Canopy.plot<-ggplot(BartonDigital, aes(x = Temp, y = Height))+geom_line()+
  xlab("Temperature (C)")+ylab("Height (m)")+theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())

####Figure 2####
Fig2A <- ggdraw() + draw_image("Fig2A.png", scale = 0.9)

#Figure2B
Fig2B.data<-read.csv("Fig2BData.csv")
Fig2B.plot<-ggplot(Fig2B.data, aes(x = TEMPMEANYR, y = Littoralcarbon))+geom_point(colour="#0079d7")+
  xlab(expression("Summer air temperature"*~degree*C))+
  ylab("Nearshore coupling (logit proportion)")+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_colour_manual(values=c("Black", "Grey"))+
  stat_smooth(colour="black",se=FALSE, size=0.5, method = "lm")

#Figure2C
Fig2C.data<-read.csv("Fig2CData.csv")

Fig2C.plot<-ggplot(Fig2C.data, aes(x = lit_temp, y = depth))+geom_point(colour="#0079d7")+
  xlab(expression("Littoral zone temperature"*~degree*C))+
  ylab("Mean depth of detection (m)")+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_colour_manual(values=c("Black", "Grey"))+
  stat_smooth(colour="black",se=FALSE, size=0.5, method = "lm")+
  scale_y_reverse()

#Figure2D
Fig2D.data<-read.csv("Fig2DData.csv")

Fig2D.plot<-ggplot(Fig2D.data, aes(x = residual.x, y = residual.y))+geom_point(colour="#0079d7")+
  xlab("Residual average recent air temperature")+
  ylab("Residual log10 (mean depth of capture)")+
  theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_colour_manual(values=c("Black", "Grey"))+
  stat_smooth(colour="black",se=FALSE, size=0.5, method = "lm")+
  scale_y_reverse()

#plot Figure 2
Figure2<-plot_grid(Fig2A, Fig2B.plot, Fig2C.plot, Fig2D.plot, labels = c("A", "B", "C", "D"), ncol = 2)


####Figure 3####
Fig3A<-ggdraw() + draw_image("Fig3A.png", scale = 0.9)

#Fig3B
#get data from Barton et al paper 
BartonControl<- ReadAndCal("BartonHeightControl.jpg")
HeightControl<-DigitData(col = "red")
BartonControlDigital<- Calibrate(HeightControl, BartonControl, 10, 40, 0, 70)

BartonWarmed<- ReadAndCal("BartonHeightWarmed.jpg")
HeightWarmed<-DigitData(col = "red")
BartonWarmedDigital<- Calibrate(HeightWarmed, BartonWarmed, 10, 40, 0, 70)

#merge two 
BartonHeights<-rbind(BartonControlDigital, BartonWarmedDigital)

#make labels
BartonHeights$Species<-c("Active","SW", "Grass", "Active","SW", "Grass")
BartonHeights$Treatment<-c("Control", "Control", "Control", "Warmed", "Warmed", "Warmed")
colnames(BartonHeights)[1]<-"Horizontal"
colnames(BartonHeights)[2]<-"Vertical"

write.csv(BartonHeights, "BartonHeights.csv")
BartonHeights<-read.csv("BartonHeights.csv")

Fig3B.plot<-ggplot(BartonHeights, aes(x = Horizontal, y = Vertical))+geom_point(aes(colour=Treatment, shape=Species, size=2))+
  xlab("Horizonal distance (cm)")+ylab("Vertical height (cm)")+theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_colour_manual(values=c("Black", "Grey"))

#Fig3C
#get data from Barton et al paper 
BartonForagingVermont<-ReadAndCal("BartonVermont.jpg")
Vermont<-DigitData(col = "red")
BartonVermontDigital<- Calibrate(Vermont,BartonForagingVermont, 0, 60, 0, 300)
BartonVermontDigital$State<-"VT"

#get data from Barton et al paper 
BartonForagingCT<-ReadAndCal("BartonConnecticut.jpg")
CT<-DigitData(col = "red")
BartonCTDigital<- Calibrate(CT,BartonForagingCT, 0, 60, 0, 300)
BartonCTDigital$State<-"CT"

#get data from Barton et al paper 
BartonForagingNJ<-ReadAndCal("BartonNewJersey.jpg")
NJ<-DigitData(col = "red")
BartonNJDigital<- Calibrate(NJ,BartonForagingNJ, 0, 60, 0, 300)
BartonNJDigital$State<-"NJ"

#merge DF
BartonGrasshopper<-rbind(BartonVermontDigital, BartonCTDigital, BartonNJDigital)
colnames(BartonGrasshopper)[1]<-"Temp"
colnames(BartonGrasshopper)[2]<-"Feeding"

write.csv(BartonGrasshopper, "BartonGrasshopper.csv")

Fig3C.plot<-ggplot(BartonGrasshopper, aes(x = Temp, y = Feeding))+geom_point(aes(colour=State))+
  xlab(expression("Max temperature"*~degree*C))+ylab("Feeding time (min)")+theme_classic()+
  theme(panel.border=element_rect(colour="black",fill=NA))+
  theme(strip.background = element_blank())+scale_colour_manual(values=c("#81B95D", "#0079D7", "#EDF50E"))+
  stat_smooth(colour="black",se=FALSE, size=0.5, method = "lm")

#Fig3D
#get warmed data from Barton et al paper 
BartonBiomassWarmed<-ReadAndCal("BartonBiomassWarmed.jpg")
BiomassDataWarmed<-DigitData(col = "red")
BartonBiomassDigitalWarmed<- Calibrate(BiomassDataWarmed, BartonBiomassWarmed, 0, 20, 0, 120)
BartonBiomassDigitalWarmed$Structure<-c("Control","Control", "IGP", "IGP")
BartonBiomassDigitalWarmed$Treatment<-c("Grass", "Herbs","Grass","Herbs")
BartonBiomassDigitalWarmed$Temp<-"Warmed"
colnames(BartonBiomassDigitalWarmed)[2]<-"Biomass"
BartonBiomassDigitalWarmed<-BartonBiomassDigitalWarmed[-(1)]

#get control data from Barton et al paper 
BartonBiomassControl<-ReadAndCal("BartonBiomassControl.jpg")
BiomassDataControl<-DigitData(col = "red")
BartonBiomassDigitalControl<- Calibrate(BiomassDataControl, BartonBiomassControl, 0, 20, 0, 120)
BartonBiomassDigitalControl$Structure<-c("Control","Control", "IGP", "IGP")
BartonBiomassDigitalControl$Treatment<-c("Grass", "Herbs","Grass","Herbs")
BartonBiomassDigitalControl$Temp<-"Control"
colnames(BartonBiomassDigitalControl)[2]<-"Biomass"
BartonBiomassDigitalControl<-BartonBiomassDigitalControl[-(1)]

#merge DF
BartonBiomassDigital<-rbind(BartonBiomassDigitalWarmed, BartonBiomassDigitalControl)

write.csv(BartonBiomassDigital, "BartonBiomassDigital.csv")
BartonBiomassDigital<-read.csv("BartonBiomassDigital.csv")

#make Fig3D
Fig3D.plot<-ggplot(BartonBiomassDigital, aes(x=Structure, y=Biomass))+
  geom_bar(stat="identity", position=position_dodge(), aes(fill=Treatment))+
  scale_fill_manual(values=c("#81B95D", "#0079D7"))+
  facet_wrap(~Temp)

#plot Figure 3
Figure3<-plot_grid(Fig3A, Fig3B.plot, Fig3C.plot, Fig3D.plot, labels = c("A", "B", "C", "D"), ncol = 2)




