# R-script from Georgia Grant for Manuscript 
#Grant,GR., Naish, TR., Keisling, BA., Patterson MO., 
#Kamp, PJJ., Meyers, SR., Strogen, DP., Stocchi, P., McKay, RM
#Reduced magnitude of Early Pleistocene intensification of Northern Hemisphere Glaciation,
#Prepared for Quaternary Science Reviews 


# load the required packages
library(astrochron)
library(tidyverse)
library(cowplot)


# Set WD
# Load the data
load("Grantetal_NHIS.MS_JUL2024.RData")


######## References ####################################################
# PlioSeaNZ (this study) 
# PaleoWD (this study)
# Miller (Miller et al., 2020)
 
# Paleoshorelines
# Dimutru (Dimitru et al., 2019 range of 10th and 90th percentiles)
# Hearty (South Africa paleo shorelines Hearty et al., 2020)
# Enewetak (Enewetak Atoll Wardlaw and Quinn, 1990) 
# Miller_sngl (Miller et al., 2012 GMSL 22 +- 10m)
# Rovere (minimum height Atlantic coastal Scarp Rovere et al., 2015, Dowsett and Cronin, 1990)
# Shakun (Terrestrial AIS limiting Shakun et al., 2018)
# Naish_sngl (Evidence for West Antarctic Ice Sheet limiting Naish et al., 2009)
# Bertram (Evidence for Aurora Basin retreat Bertram et al., 2018)
# Bierman (Greenland ice sheet maximum retreat Bierman et al., 2016)


# NHIS.Area (Batcheloretal2019_Quaternary_Ice_sheets_areas.csv)

#pCO2 (paleo CO2 from pco2.org accessed Jan 2022, Rae et al., 2021)
#Herbert (SST Stack Herbert et al., 2010) 
#LR04 (Benthic d18O stack Lisiecki and Raymo, 2005)
#LGM_RSL (Sea level from Lambeck et al., 2014)

#ex (etp solution Laskar, 2004)



###############################################################################################
####################### Data Visualisation Fig 1 + 5 ############################################################
#######################################################################################################

### Fig. 1 
# PlioSeaNZ (this study) published Grant et al., 2019 (3.3 - 2.56 Ma) 
# Miller (Miller et al., 2020)
#LR04 (Benthic d18O stack Lisiecki and Raymo, 2005)
#pCO2 (paleo CO2 from pco2.org accessed Jan 2022, Rae et al., 2021)
#Herbert (SST Stack Herbert et al., 2010) 


a<- ggplot(Miller,aes(x=Age..ka.))+
  geom_line(aes(y=Sea.level..m.),lwd=1)+
  geom_ribbon(data=filter(PlioSeaNZ, Age..ka.>2544.7),aes(ymin=min.RSL-20,ymax=max.RSL-20),fill='deepskyblue2',alpha=0.6)+ #adjust by -20 to overlap with Miller
  geom_ribbon(data=filter(PlioSeaNZ, Age..ka.<2544.7),aes(ymin=min.RSL-40,ymax=max.RSL-40),fill='darkorange',alpha=0.6)+ #adjust by -20 to overlap with Miller
  geom_path(data=filter(PlioSeaNZ, Age..ka.<2544.7),aes(y=mean.RSL-40),col='darkorange',alpha=0.6,lwd=1)+ #adjust by -20 to overlap with Miller
  scale_x_continuous(limits=c(1600,3400))+
  scale_y_continuous(limits=c(-60,40),breaks=c(-40,-20,0,20,40))+
  geom_hline(aes(yintercept =0),color='grey',alpha=0.5,linetype='dotted')+ #modern
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  labs(y="Relative Sea Level (m)",x="Age (Ma)")

b<-ggplot(LR04) +
  geom_line(aes(Time..ka.,Benthic.d18O..per.mil.),col='grey',lwd=1)+
  scale_x_continuous(limits=c(1600,3400))+
  scale_y_continuous(limits=c(4.5,2.5),trans='reverse',breaks=c(2.5,3.0,3.5,4.0))+
  geom_hline(aes(yintercept =3.5),color='grey',alpha=0.5,linetype='dotted')+ #modern
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  labs(y="LR04 \u03b418O (\u2030)",x="Age (Ma)")

c<-ggplot(pCO2,aes(age,co2))+
  geom_point(data=filter(pCO2,co2>400),size=1,shape=21,color='pink')+
  geom_point(data=filter(pCO2,co2<400),size=1,shape=21,color='black')+
  geom_hline(aes(yintercept =400),color='grey',alpha=0.5,linetype='dotted')+ #modern
  scale_x_continuous(limits=c(1600,3400))+
  scale_y_continuous(limits=c(100,600),breaks=c(200,400))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  labs(x="Age (ka)",y="CO2 (ppm)")

d<-ggplot(Herbert_SST,aes(x=Age..ka.BP.,y=SST..1.12....C.))+
  geom_ribbon(aes(ymin=28, ymax=pmax(SST..1.12....C.,28)), fill="red", alpha=0.5) +
  geom_path()+
  geom_hline(aes(yintercept =28),color='grey',alpha=0.5,linetype='dotted')+ #modern
  scale_x_continuous(limits=c(1600,3400))+
  scale_y_continuous(limits=c(25,30),breaks=c(26,28,30))+
  theme_bw()+
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
  labs(x="Age (ka)",y="SST (C)")

plot_grid(a,b,c,d,nrow=4)

ggsave("Fig.1_Rplot.pdf")

#### Fig. 5 paleowaterdepth estimates for plotting along stratigraphy

PaleoWD %>%
  ggplot(aes(x=Stratigraphic.Height..m.,y=Mean.Waterdepth..m.))+
  geom_ribbon(aes(ymin=Minimum.Waterdepth..m.,ymax=Maximum.Waterdepth..m.),fill='deepskyblue2',alpha=0.4) +
  geom_line(col='deepskyblue3',size=1)+
  geom_line(aes(y=Minimum.Waterdepth..m.),col='deepskyblue',linewidth=0.5)+
  geom_line(aes(y=Maximum.Waterdepth..m.),col='deepskyblue3',linewidth=0.5)+
  geom_point(aes(shape=Sample.source,col=Sample.source),size=2,alpha=0.8)+
  scale_color_manual(values=c('coral2','grey47'))+ 
  coord_flip()+
  theme_bw()

########################################################################################################
####################### Orbital analysis for Fig. 6 ############################################################
#######################################################################################################
# interpolate to even sampling grid. Median= 2.66 ka, mean= 3.39 ka. Will use 3.5 ka
PlioSeaNZ_RSLi=linterp(PlioSeaNZ[,c(1,3)],dt=3.5)
#interpolate for LR04
lr04_2i=linterp(LR04,start=1400,dt=3.5)

# EPSA for plotting
eha(PlioSeaNZ_RSLi,step=3.5,demean=T,detrend=T,win=500,pad=10000,genplot=4,fmax=0.06,palette=5,pl=2)
eha(linterp(PlioSeaNZ[,c(1,3)],start=2600,dt=2),step=2,demean=T,detrend=T,win=200,pad=10000,genplot=4,fmax=0.06,palette=5,pl=2)


eha(ex,step=3,demean=T,detrend=T,win=500,pad=10000,genplot=4,fmax=0.06,palette=5,pl=2)

eha(lr04_2i,step=3.5,demean=T,detrend=T,win=500,pad=10000,genplot=4,fmax=0.06,palette=5,pl=2)

## bandpass eccentricity envelope from RSL 

RSL_Eccbnd<-bandpass(PlioSeaNZ_RSLi,flow=0.0015, fhigh=1/68)


ggplot(RSL_Eccbnd,aes(Age..ka.,mean.RSL))+
  geom_line(linetype='dotted',lwd=1,col='grey')+
  theme_bw()+
  theme(panel.grid = element_blank())
ggsave("RSL_ecc bandpass.pdf")

# EPSA for integrations, out to fmax of 0.1 cycles/ka
pwr1=eha(PlioSeaNZ_RSLi,ydir=-1,win=500,step=3.5,pad=1000,output=2,fmax=.1)
# EPSA for LR04
pwr2=eha(lr04_2i,ydir=-1,win=500,step=3.5,pad=1000,output=2,fmax=.1)

# evaluate ETP to get a sense of appropriate bandwidths for integration
milank=etp(tmin=1600,tmax=3400,dt=3.5)

eha(milank,win=1799,sigID=T)
# note, for a 500 kyr window and 2pi tapers, we have halfwidth of 0.003996004 cycles/ka
# we will use the broad frequency band for obliquity, including the ~29 to ~54 kyr terms
#  max obliquity term is 53.5 ka.  1/53.5 - 0.003996004 = 0.01469558 = ~1/68.0 ka
#  min obliquity term is 28.8 ka.  1/28.8 + 0.003996004 = 0.03871822 = ~1/25.8 ka
#  max precession term is 23.7 ka. 1/23.7 - 0.003996004 = 0.03819809 = ~1/26.2 ka
#  min precession term is 18.9 ka. 1/18.9 +  0.003996004 = 0.05690606 = ~1/17.6 ka
#  min eccentricity term is 93.1 ka. 1/93.1 + 0.003996004 = 0.01473714 = ~1/67.9 ka
pwr=eha(milank,ydir=-1,win=500,step=3.5,pad=1000,output=2,fmax=.1)
res=integratePower(pwr,npts=143,pad=1000)

# integrate the power spectra
RSL_prec=integratePower(pwr1,flow=1/26,fhigh=1/17.6,npts=143,pad=1000)
RSL_ob=integratePower(pwr1,flow=1/68,fhigh=1/26,npts=143,pad=1000)
RSL_ecc=integratePower(pwr1,flow=0.0,fhigh=1/68,npts=143,pad=1000)

pl(1,w=8,h=6)
plot(RSL_ecc[,1],RSL_ecc[,2],type="l",ylim=c(0,max(RSL_ecc[,2])),lwd=2,xlab="Time (ka)",main="PlioSeaNZ Power: Eccentricity (black), Obliquity (red), Precession (blue)",ylab="Variance")
lines(RSL_ob[,1],RSL_ob[,2],type="l",col="red",lwd=2)
lines(RSL_prec[,1],RSL_prec[,2],type="l",col="blue",lwd=2)


lr04_prec=integratePower(pwr2,flow=1/26,fhigh=1/17.6,npts=143,pad=1000)
lr04_ob=integratePower(pwr2,flow=1/68,fhigh=1/26,npts=143,pad=1000)
lr04_ecc=integratePower(pwr2,flow=0.0,fhigh=1/68,npts=143,pad=1000)

pl(1,w=8,h=6)
plot(lr04_ecc[,1],lr04_ecc[,2],type="l",ylim=c(0,max(lr04_ob[,2])),lwd=2,xlab="Time (ka)",main="LR04 Power: Eccentricity (black), Obliquity (red), Precession (blue)",ylab="Variance")
lines(lr04_ob[,1],lr04_ob[,2],type="l",col="red",lwd=2)
lines(lr04_prec[,1],lr04_prec[,2],type="l",col="blue",lwd=2)



###########################################################################
################### RSL  amplitude assessment for Fig. 7 ##################
###########################################################################
## determine maximum amplitude through record (for selection of window) ###


############## PlioSeaNZ mean RSL amplitude ###########

lin_meanPlioSeaNZ=linterp(PlioSeaNZ[,c(1,3)],3.5)
site<-lin_meanPlioSeaNZ
n=3.5 #dt sampling


Amp21<-mwMinMax(dat=site,cols=2, win=21,conv=1)
Amp41<-mwMinMax(dat=site,cols=2, win=41,conv=1)
Amp100<-mwMinMax(dat=site,cols=2, win=100,conv=1)

df1<-data.frame(Amp21[,c(1,4)])
df2<-data.frame(Amp41[,c(1,4)])
df3<-data.frame(Amp100[,c(1,4)])

PlioSeaNZ_meanAmpMW<-list(df1,df2,df3) %>%
  reduce(full_join, by = "Center_win")
colnames(PlioSeaNZ_meanAmpMW)<-c("time.ka","Amp20","Amp41","Amp100")

autoPlot(PlioSeaNZ_meanAmpMW ) 



############## PlioSeaNZ min RSL amplitude ###########

lin_minPlioSeaNZ=linterp(PlioSeaNZ[,c(1,2)],3.5)
site<-lin_minPlioSeaNZ
n=3.5 #dt sampling


Amp21<-mwMinMax(dat=site,cols=2, win=21,conv=1)
Amp41<-mwMinMax(dat=site,cols=2, win=41,conv=1)
Amp100<-mwMinMax(dat=site,cols=2, win=100,conv=1)

df1<-data.frame(Amp21[,c(1,4)])
df2<-data.frame(Amp41[,c(1,4)])
df3<-data.frame(Amp100[,c(1,4)])

PlioSeaNZ_minAmpMW<-list(df1,df2,df3) %>%
  reduce(full_join, by = "Center_win")
colnames(PlioSeaNZ_minAmpMW)<-c("time.ka","Amp20","Amp41","Amp100")


######## PlioSeaNZ RSL amplitude maximum #########

lin_maxPlioSeaNZ=linterp(PlioSeaNZ[,c(1,4)],3.5)
site<-lin_maxPlioSeaNZ
n=3.5 #dt sampling


Amp21<-mwMinMax(dat=site,cols=2, win=21,conv=1)
Amp41<-mwMinMax(dat=site,cols=2, win=41,conv=1)
Amp100<-mwMinMax(dat=site,cols=2, win=100,conv=1)

df1<-data.frame(Amp21[,c(1,4)])
df2<-data.frame(Amp41[,c(1,4)])
df3<-data.frame(Amp100[,c(1,4)])

PlioSeaNZ_maxAmpMW<-list(df1,df2,df3) %>%
  reduce(full_join, by = "Center_win")
colnames(PlioSeaNZ_maxAmpMW)<-c("time.ka","Amp20","Amp41","Amp100")



########### Miller et al 2020 amplitude assesment ##################

Milleriso=iso(Miller[,c(1,4)],1500,3500)
strats(Milleriso)
lin=linterp(Milleriso,2.3)

site<-lin


Amp21<-mwMinMax(dat=site,cols=2, win=21,conv=1)
Amp41<-mwMinMax(dat=site,cols=2, win=41,conv=1)
Amp100<-mwMinMax(dat=site,cols=2, win=100,conv=1)

df1<-data.frame(Amp21[,c(1,4)])
df2<-data.frame(Amp41[,c(1,4)])
df3<-data.frame(Amp100[,c(1,4)])

Miller2020_ampMW<-list(df1,df2,df3) %>%
  reduce(full_join, by = "Center_win")
colnames(Miller2020_ampMW)<-c("time.ka","Amp20","Amp41","Amp100")
autoPlot(Miller2020_ampMW)
#write.csv(Miller2020_ampMW,"Miller2020_ampMW.csv")


############ Amplitude dataframes #############

R1=PlioSeaNZ_minAmpMW
R2=PlioSeaNZ_meanAmpMW
R3=PlioSeaNZ_maxAmpMW

## mPl (mid Pleistocene 2.1-1.7 Ma) #data starts at 1.76 for 41kyr window
time1=1700
time2=2100
R1_iso<-iso(R1,time1,time2,genplot=F)
R2_iso<-iso(R2,time1,time2,genplot=F)
R3_iso<-iso(R3,time1,time2,genplot=F)

Rcol=3 #41ky window
Rnames<-c("PlioSeaNZ_ampMin", "PlioSeaNZ_ampMean","PlioSeaNZ_ampMax")
Rwindow<-"t=41kyr"
Rtime<-"2.1-1.76 Ma"
RStudy<-"PlioSeaNZ"

Amplitude<-list(R1_iso[,Rcol], R2_iso[,Rcol], R3_iso[,Rcol])
names(Amplitude)<-Rnames
Amp_mPl<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                    Window=rep(Rwindow,lengths(Amplitude[1])),
                    Period=rep(Rtime,lengths(Amplitude[1])),
                    Name=rep(Rnames,lengths(Amplitude)),
                    Study=rep(RStudy,lengths(Amplitude[1])))
Amp_mPl<-na.omit(Amp_mPl)

## EmPl (Early mid Pleistocene 2.4- 2.1 Ma)
time1=2100
time2=2400
R1_iso<-iso(R1,time1,time2,genplot=F)
R2_iso<-iso(R2,time1,time2,genplot=F)
R3_iso<-iso(R3,time1,time2,genplot=F)

Rcol=3 #41ky window
Rnames<-c("PlioSeaNZ_ampMin", "PlioSeaNZ_ampMean","PlioSeaNZ_ampMax")
Rwindow<-"t=41kyr"
Rtime<-"2.4-2.1 Ma"
RStudy<-"PlioSeaNZ"

Amplitude<-list(R1_iso[,Rcol], R2_iso[,Rcol], R3_iso[,Rcol])
names(Amplitude)<-Rnames
Amp_EmPl<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                    Window=rep(Rwindow,lengths(Amplitude[1])),
                    Period=rep(Rtime,lengths(Amplitude[1])),
                    Name=rep(Rnames,lengths(Amplitude)),
                    Study=rep(RStudy,lengths(Amplitude[1])))
Amp_EmPl<-na.omit(Amp_EmPl)

## ePl (Early Pleistocene 2.7-2.4)
time1=2400
time2=2700
R1_iso<-iso(R1,time1,time2,genplot=F)
R2_iso<-iso(R2,time1,time2,genplot=F)
R3_iso<-iso(R3,time1,time2,genplot=F)

Rcol=3 #41ky window
Rnames<-c("PlioSeaNZ_ampMin", "PlioSeaNZ_ampMean","PlioSeaNZ_ampMax")
Rwindow<-"t=41kyr"
Rtime<-"2.7-2.4 Ma"
RStudy<-"PlioSeaNZ"

Amplitude<-list(R1_iso[,Rcol], R2_iso[,Rcol], R3_iso[,Rcol])
names(Amplitude)<-Rnames
Amp_ePl<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                   Window=rep(Rwindow,lengths(Amplitude[1])),
                   Period=rep(Rtime,lengths(Amplitude[1])),
                   Name=rep(Rnames,lengths(Amplitude)),
                   Study=rep(RStudy,lengths(Amplitude[1])))
Amp_ePl<-na.omit(Amp_ePl)

## Lp (Late Pliocene 3.0-2.7 Ma)
time1=2700
time2=3000
R1_iso<-iso(R1,time1,time2,genplot=F)
R2_iso<-iso(R2,time1,time2,genplot=F)
R3_iso<-iso(R3,time1,time2,genplot=F)

Rcol=2 #21ky window
Rnames<-c("PlioSeaNZ_ampMin", "PlioSeaNZ_ampMean","PlioSeaNZ_ampMax")
Rwindow<-"t=21kyr"
Rtime<-"3.0-2.7 Ma"
RStudy<-"PlioSeaNZ"

Amplitude<-list(R1_iso[,Rcol], R2_iso[,Rcol], R3_iso[,Rcol])
names(Amplitude)<-Rnames
Amp_LP<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                   Window=rep(Rwindow,lengths(Amplitude[1])),
                   Period=rep(Rtime,lengths(Amplitude[1])),
                   Name=rep(Rnames,lengths(Amplitude)),
                   Study=rep(RStudy,lengths(Amplitude[1])))
Amp_LP<-na.omit(Amp_LP)


## mP (mid Pliocene 3.3-3.0 Ma)
time1=3000
time2=3300
R1_iso<-iso(R1,time1,time2,genplot=F)
R2_iso<-iso(R2,time1,time2,genplot=F)
R3_iso<-iso(R3,time1,time2,genplot=F)

Rcol=2 #21ky window
Rnames<-c("PlioSeaNZ_ampMin", "PlioSeaNZ_ampMean","PlioSeaNZ_ampMax")
Rwindow<-"t=21kyr"
Rtime<-"3.3-3.0 Ma"
RStudy<-"PlioSeaNZ"

Amplitude<-list(R1_iso[,Rcol], R2_iso[,Rcol], R3_iso[,Rcol])
names(Amplitude)<-Rnames
Amp_mP<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                 Window=rep(Rwindow,lengths(Amplitude[1])),
                    Period=rep(Rtime,lengths(Amplitude[1])),
                       Name=rep(Rnames,lengths(Amplitude)),
                 Study=rep(RStudy,lengths(Amplitude[1])))
Amp_mP<-na.omit(Amp_mP)

# combine dataframe
RSL_vioplot.df<-rbind(Amp_mPl,Amp_EmPl,Amp_ePl,Amp_LP,Amp_mP)



# #### Miller et al amplitude 2020 dataframe ##########
R<- Miller2020_ampMW

R1_iso<-iso(R,1758,2100,genplot=F) #Rcol =3(41kyr window)
R2_iso<-iso(R,2100,2400,genplot=F) #Rcol=3 (41kyr window)
R3_iso<-iso(R,2400,2700,genplot=F)#Rcol=3 (41kyr window)
R4_iso<-iso(R,2700,3000,genplot=F) #Rcol=2 (21kyr window)
R5_iso<-iso(R,3000,3300,genplot=F)#Rcol=2 (21kyr window)


Rnames<-c("MillerAmp_mPl","MillerAmp_EmPl","MillerAmp_ePl","MillerAmp_LP","MillerAmp_mP")
Rtime<-c("2.1-1.76 Ma","2.4-2.1 Ma","2.7-2.4 Ma","3.0-2.7 Ma", "3.3-3.0 Ma")
Rwindow<-c("41kyr","41kyr","41kyr","21kyr","21kyr")
RStudy<-c("Miller2020","Miller2020","Miller2020","Miller2020","Miller2020")
Amplitude<-list(R1_iso[,3], R2_iso[,3], R3_iso[,3],R4_iso[,2],R5_iso[,2])
names(Amplitude)<-Rnames
MillerAmp.df<-data.frame(RSLamp.m=unlist(Amplitude,recursive=T,use.names=F),
                   Window=rep(Rwindow,lengths(Amplitude)),
                   Period=rep(Rtime,lengths(Amplitude)),
                   Name=rep(Rnames,lengths(Amplitude)),
                   Study=rep(RStudy,lengths(Amplitude)))

 MillerAmp.df<-na.omit(MillerAmp.df)

#################################################################################
################## NHIS area-volume calculations for Fig. 7 #####################

 # Area data from Supp. Info Batchelor et al., 2019 and scaling coefficients 'c' 
# with uncertainty. Following Batchelor et al., 2019 
# eq 1 V=cA^(gamma)where gamma = 1.25 and SLE = V/362 million km2 

# Scaling coefficients Table 1 Batchelor et al., 2019  
 Icesheets<-c("CIS","EIS","LIS","GIS","others")
 Mean.c<-c(0.99,0.85,0.94,1.07,0.56)
 std.c<-c(0.36,0.07,0.03,0.14,0.17)

 #set gamma 
 gamma<- 5/4
 
#collate dataset
 AVS.c<-data.frame(Icesheets,Mean.c,std.c) 

#lengthen data set  
 NHIS.area.long<- NHIS.Area %>%
   select(TimeSlice:others)%>%
   pivot_longer(cols = c(3:7),names_to="Icesheets", values_to="Area.km2")

#combine dataframes to calculate SLE  
 NHIS.AVS<- NHIS.area.long %>%
   left_join(AVS.c,by="Icesheets") %>%
   group_by(Icesheets) %>%
   mutate(vol.ice.mean=Mean.c*(Area.km2*gamma),
          vol.ice.min=(Mean.c-std.c)*(Area.km2*gamma),
          vol.ice.max=(Mean.c+std.c)*(Area.km2*gamma),
          SLE.mean=vol.ice.mean/1000/361.8,
          SLE.min=vol.ice.min/1000/361.8,
          SLE.max=vol.ice.max/1000/361.8)
 
#Calculate cumulative SLE for plotting
  NHIS.AVS.sum <- NHIS.AVS %>%
   group_by(TimeSlice,Stat.Type)%>%
   mutate(cumsum.mean=round(cumsum(SLE.mean),1),
          cumsum.min=round(cumsum(SLE.min),1),
          cumsum.max=round(cumsum(SLE.max),1))
 
#Filter two Plio-Pleistocene timeslices
  NHIS.AVS.sum.subst<-NHIS.AVS.sum %>%
   filter(TimeSlice == '(2.6-1.78Ma)' |TimeSlice == '(3.59-2.6Ma)') %>%
   arrange(TimeSlice,Stat.Type,SLE.mean)

 
##############################################################################
######################## Plot Fig, 7 #########################################

#stat functions 
 f <- function(x) {
     r <- quantile(x, probs = c(0.1,0.25,0.5,0.75,0.9))#(0.1, 0.159, 0.5, 0.841, 0.9))
      names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
      r
    }
    
 m <- function(x) {
    return(quantile(x,probs=0.5))
    }


 #combine dataframes for plotting
AllRSL_vioplot.df<-rbind(RSL_vioplot.df,MillerAmp.df)

#calculate pts per window
RSL_count<- RSL_vioplot.df %>% group_by(Period) %>% tally()
RSL_count$n <-RSL_count$n /3 # includes min, mean max 
Miller_count<- MillerAmp.df %>% group_by(Period) %>% tally()

amp_count<- rbind(RSL_count,Miller_count)


########## geom split violin script for function at bottom of page 

# PlioSeaNZ, Miller and Batchelor estimates

ggplot(AllRSL_vioplot.df)+
  geom_split_violin(aes(x=Period,y=RSLamp.m,fill=Study),trim=TRUE,scale="width",adjust=0.5,show.legend = TRUE)+
    scale_fill_manual(name="Study",values=c("grey47","deepskyblue"))+ 
  stat_summary(fun.data = f,geom="boxplot",width=0.05,aes(x=Period,y=RSLamp.m,fill=Study),lwd=1,position=position_dodge()) +
  stat_summary(fun=m,geom="point",shape='-',size=5,aes(x=Period,y=RSLamp.m,color=paste("mean", Study)))+
  theme(axis.title.x=element_blank(), legend.position="left", axis.text.x=element_text(face="bold"),axis.text.y = element_text(face="bold"),panel.grid.major.x  = element_blank())+
  scale_y_continuous(name="Relative sea-level amplitudes (m)",limits=c(0,110),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=110,by=10))+
  annotate(geom="text",x=amp_count$Period,y=rep(c(70,75),5),label=paste("italic(n)==",amp_count$n),parse = TRUE)+

  geom_point(data=NHIS.AVS.sum.subst ,aes(x=TimeSlice, group=Stat.Type, col=Icesheets,y=cumsum.mean),shape='-',size=5,position=position_dodge(width=1))+
  geom_errorbar(data= NHIS.AVS.sum.subst, aes(x=TimeSlice, group=Stat.Type, col=Icesheets, ymin=cumsum.min, ymax=cumsum.max), width=.2,position=position_dodge(width=1))
ggsave("RSLamplitudes_splitviolin.pdf",height=4,width=8)

#mid Pliocene constraints from Paleoshorelines
pdf("midPliocene_SLE.pdf", width=6,height=8.25)
par(bg="lightgrey")
plot.new()
plot.window(xlim=c(0.75,8.5),ylim=c(0,70),xlab="Study",ylab="RSL (m)",yaxs="i",xaxs="i")
axis(1,at=c(1:9),labels=c(names(Paleoshorelines))) #"Dimutru","Hearty","Enewetak", "Dolan", "Koenig","Rovere","Shakun", "Naish", "Bertram"))
axis(2)
  lines(Paleoshorelines[[1]],lwd=3,lend=2)+
lines(Paleoshorelines[[2]],lwd=3,lend=2)+
lines(Paleoshorelines[[3]],lwd=3,lend=2)+
lines(Paleoshorelines[[4]],lwd=3,lend=2)+
points(Paleoshorelines[[5]],lwd=3,lend=2)+
points(Paleoshorelines[[6]],lwd=3,lend=2)+
points(Paleoshorelines[[7]],lwd=3,lend=2)+
points(Paleoshorelines[[8]],lwd=3,lend=2)+
points(Paleoshorelines[[9]],lwd=3,lend=2)
  dev.off()



########## table of stats  - Table 2 ##### 
Summary_stats <- AllRSL_vioplot.df %>%
    group_by(Study, Period)%>%
    summarise (min=min(RSLamp.m),
               mean = mean(RSLamp.m),
               max= max(RSLamp.m))
 
Summary_NHIS_stats<- NHIS.AVS.sum.subst %>%
  filter(Icesheets=='others') %>%
  group_by(TimeSlice, Stat.Type)%>%
  summarise (min=min(cumsum.min),
             mean = mean(cumsum.mean),
             max= max(cumsum.max))


##################################################################################################################
####################################### Ice Sheet sensitivity (Figure 8) #######################################

################### RSL  assessment ##################



PlioSeaNZ<-  rename(PlioSeaNZ, age = Age..ka.)
RSL_dat<- PlioSeaNZ %>%
  mutate(Period=ifelse(age>=1700 & age <=2100,"midPleist",
                       ifelse(age>=2100 & age <=2400,"EarlymidPleist",
                              ifelse(age>=2400 & age <=2700,"EarlyPleist",
                                     ifelse(age>=2700 & age <=3000,"LatePlio",
                                            ifelse(age>=3000 & age <=3350,"midPlio", NA)))))) 

RSL.BOX_DATA <- RSL_dat %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(mean.RSL, 0.25),
            y.q50 = quantile(mean.RSL, 0.5),
            y.q75 = quantile(mean.RSL, 0.75),
            
            ymin = min(max.RSL[max.RSL >= y.q25 - 1.5 * IQR(max.RSL)]), 
            ymax = max(min.RSL[min.RSL <= y.q75 + 1.5 * IQR(min.RSL)])) %>%
  ungroup()

RSL.BOX_DATA <- RSL_dat %>%
  pivot_longer(cols = min.RSL:max.RSL, values_to = "RSL.m", names_to = "Model") %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            y.q25 = quantile(RSL.m, 0.25),
            y.q50 = quantile(RSL.m, 0.5),
            y.q75 = quantile(RSL.m, 0.75),
            
            ymin = min(RSL.m[RSL.m >= y.q25 - 1.5 * IQR(RSL.m)]), 
            ymax = max(RSL.m[RSL.m <= y.q75 + 1.5 * IQR(RSL.m)])) %>%
  ungroup()

RSL.BOX_DATA<-na.omit(RSL.BOX_DATA)

ggplot() + 
  geom_line(data = RSL_dat, aes(age, mean.RSL)) + 
  geom_ribbon(data=RSL_dat,aes(x=age,ymin=min.RSL,ymax=max.RSL))+
  geom_rect(data = RSL.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = RSL.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype='bold')) +
  geom_segment(data = RSL.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))


################### CO2  assessment ##################
iso_CO2<- pCO2 %>%
  filter(age>1700 & age <3300) %>%
  filter(co2<600)   %>%
  group_by(doi) %>%
  arrange(age,.by_group = TRUE) %>%
  mutate(lag_dt=age-lag(age)) %>%
  mutate(lead_dt=lead(age)-age)  %>%
  filter(lag_dt <=50 | lead_dt<=50) %>% # |is.na(lag_dt) & lead_dt <= 50) 
  mutate(Period=ifelse(age>=1700 & age <=2100,"midPleist",
                       ifelse(age>=2100 & age <=2400,"EarlymidPleist",
                              ifelse(age>=2400 & age <=2700,"EarlyPleist",
                                     ifelse(age>=2700 & age <=3000,"LatePlio",
                                            ifelse(age>=3000 & age <=3300,"midPlio", NA))))))  %>%
  
  ungroup()

BOX_DATA <- iso_CO2 %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(co2, 0.25),
            y.q50 = quantile(co2, 0.5),
            y.q75 = quantile(co2, 0.75),
            
            ymin = min(co2[co2 >= y.q25 - 1.5 * IQR(co2)]), 
            ymax = max(co2[co2 <= y.q75 + 1.5 * IQR(co2)])) %>%
  ungroup()

CO2.BOX_DATA<-na.omit(BOX_DATA)

ggplot() + 
  geom_point(data = iso_CO2, aes(age, co2,colour=doi)) +
  geom_line(data = iso_CO2, aes(age, co2,colour=doi)) + 
  geom_rect(data = CO2.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = CO2.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype='bold')) +
  geom_segment(data = CO2.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))

################### SST  amplitude assesment ##################

SST<-  rename(Herbert_SST, age = Age..ka.BP.)
SST<- rename(SST,SST=SST..1.12....C.)
SST_dat<- SST %>%
  mutate(Period=ifelse(age>=1700 & age <=2100,"midPleist",
                       ifelse(age>=2100 & age <=2400,"EarlymidPleist",
                              ifelse(age>=2400 & age <=2700,"EarlyPleist",
                                     ifelse(age>=2700 & age <=3000,"LatePlio",
                                            ifelse(age>=3000 & age <=3300,"midPlio", NA)))))) 
SST_dat<-na.omit(SST_dat)

SST.BOX_DATA <- SST_dat %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(SST, 0.25),
            y.q50 = quantile(SST, 0.5),
            y.q75 = quantile(SST, 0.75),
            
            ymin = min(SST[SST >= y.q25 - 1.5 * IQR(SST)]), 
            ymax = max(SST[SST <= y.q75 + 1.5 * IQR(SST)])) %>%
  ungroup()

SST.BOX_DATA<-na.omit(SST.BOX_DATA)

ggplot() + 
  geom_point(data = SST_dat, aes(age, SST )) +
  geom_line(data = SST_dat, aes(age, SST)) + 
  geom_rect(data = SST.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = SST.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype="bold")) +
  geom_segment(data = SST.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))








########################################## LGM #################################################


###################RSL  assessment ##################

LGM_RSL<-  rename(LGM_RSL, age = Age..Ka.)
LGMRSL_dat<- LGM_RSL %>%
  mutate(Period=ifelse(age>=0 & age <=20,"LGM",NA)) 

LGMRSL.BOX_DATA <- LGMRSL_dat %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(c(RSL), 0.25),
            y.q50 = quantile(c(RSL), 0.5),
            y.q75 = quantile(c(RSL), 0.75),
            
            ymin = min(RSL), #[min.RSL >= y.q25 - 1.5 * IQR(min.RSL)]), 
            ymax = max(RSL)) %>% #[max.RSL <= y.q75 + 1.5 * IQR(max.RSL)])) %>%
  ungroup()

LGMRSL.BOX_DATA<-na.omit(LGMRSL.BOX_DATA)

ggplot() + 
  geom_line(data = LGMRSL_dat, aes(age, RSL)) + 
  scale_x_continuous(name="Time (ka)",limits=c(0,20),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=20,by=10))+
  
  geom_rect(data = LGMRSL.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = LGMRSL.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype='bold')) +
  geom_segment(data = LGMRSL.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))


################### CO2 assessment ##################
LGMiso_CO2<- pCO2 %>%
  filter(age>0 & age <200) %>%
  filter(!doi %in% c("10.5194/cp-15-539-2019","10.1007/978-3-642-58646-0_19","10.1029/94PA02116",
                     "10.1126/science.1178296","10.1130/G47681.1", "10.1038/ngeo724","10.1098/rsta.2013.0096","10.1016/j.epsl.2005.04.027"))   %>%
  group_by(doi) %>%
  arrange(age,.by_group = TRUE) %>%
  mutate(lag_dt=age-lag(age)) %>%
  mutate(lead_dt=lead(age)-age)  %>%
  filter(lag_dt <=10 | lead_dt<=10) %>% # |is.na(lag_dt) & lead_dt <= 50) 
  mutate(Period=ifelse(age>=0 & age <=20,"LGM",NA))  %>%
  
  ungroup()



LGMCO2.BOX_DATA <- LGMiso_CO2 %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(co2, 0.25),
            y.q50 = quantile(co2, 0.5),
            y.q75 = quantile(co2, 0.75),
            
            ymin = min(co2[co2 >= y.q25 - 1.5 * IQR(co2)]), 
            ymax = max(co2[co2 <= y.q75 + 1.5 * IQR(co2)])) %>%
  ungroup()

LGMCO2.BOX_DATA<-na.omit(LGMCO2.BOX_DATA)

ggplot() + 
  geom_point(data = LGMiso_CO2, aes(age, co2,colour=doi)) +
  geom_line(data = LGMiso_CO2, aes(age, co2,colour=doi)) + 
  geom_rect(data = LGMCO2.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = LGMCO2.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype="bold")) +
  geom_segment(data = LGMCO2.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))+
  scale_x_continuous(name="Time (ka)",limits=c(0,20),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=20,by=10))





################### SST  assessment ##################

LGMSST<-  rename(Herbert_SST, age = Age..ka.BP.)
LGMSST<- rename(LGMSST,SST=SST..1.12....C.)
LGMSST_dat<- SST %>%
  filter(age>0 & age <23.5) %>%
  mutate(Period=ifelse(age>=0 & age <=23.5,"LGM", "Pleistocene"))

LGMSST_dat<-na.omit(LGMSST_dat)

LGMSST.BOX_DATA <- LGMSST_dat %>%
  
  group_by(Period) %>%
  summarise(xmin = min(age),
            xmax = max(age),
            
            y.q25 = quantile(SST, 0.25),
            y.q50 = quantile(SST, 0.5),
            y.q75 = quantile(SST, 0.75),
            
            ymin = min(SST[SST >= y.q25 - 1.5 * IQR(SST)]), 
            ymax = max(SST[SST <= y.q75 + 1.5 * IQR(SST)])) %>%
  ungroup()

SST.BOX_DATA<-na.omit(SST.BOX_DATA)

ggplot() + 
  geom_point(data = LGMSST_dat, aes(age, SST )) +
  geom_line(data = LGMSST_dat, aes(age, SST)) + 
  geom_rect(data = LGMSST.BOX_DATA,                # create box for box plot
            aes(xmin = xmin, xmax = xmax,
                ymin = y.q25, ymax = y.q75), 
            alpha=0.5,color = "black",fill = 'white') +
  geom_segment(data = LGMSST.BOX_DATA,             # add median line
               aes(x = xmin, xend = xmax,
                   y = y.q50, yend = y.q50,linetype="bold")) +
  geom_segment(data = LGMSST.BOX_DATA,             # add whiskers
               aes(x = (xmin + xmax) / 2, xend = (xmin + xmax) / 2,
                   y = ymin, yend = ymax))



##### LGM plot ####

Period<-"LGM"
Time.ka<-10
CO2.q50<-250
RSL.CO2<-(120/254)
RSL.SST<-(120/25.5)
SST.CO2<-(25.5/254)
RSL.CO2max<-(120/227)
RSL.SSTmax<-(120/24.4)
SST.CO2max<-(26.6/227)
RSL.CO2min<-(120/264.4)
RSL.SSTmin<-(120/26.6)
SST.CO2min<-(24.4/264.4)


LGM_sens<-data.frame(Period,Time.ka,CO2.q50,RSL.CO2,RSL.SST,SST.CO2,RSL.CO2max,RSL.SSTmax,SST.CO2max,RSL.CO2min,RSL.SSTmin,SST.CO2min)

ggplot(LGM_sens,aes(x=Time.ka))+
  geom_point(aes(y=RSL.CO2*10),colour='red')+
  geom_point(aes(y=RSL.SST),colour='blue')+
  geom_point(aes(y=SST.CO2),colour='black')+
  annotate("text",x=c(15,15,15),y=c(4,3,2),label=c("RSL.CO2 (x10)","RSL.SST","SST.CO2"),col=c("red","blue","black"))+
  scale_x_continuous(name="Time (ka)",limits=c(0,20),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=20,by=10))+
  scale_y_continuous(name="Sensitvity",limits=c(0,5),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=5,by=1))



######### calculate G-I sensitivity parameters #######

colnames(CO2.BOX_DATA)<-c("Period","CO2.time.min","CO2.time.max","CO2.q25","CO2.q50","CO2.q75","CO2.min","CO2.max")
colnames(RSL.BOX_DATA)<-c("Period","RSL.time.min","RSL.time.max","RSL.q25","RSL.q50","RSL.q75","RSL.min","RSL.max")
colnames(SST.BOX_DATA)<-c("Period","SST.time.min","SST.time.max","SST.q25","SST.q50","SST.q75","SST.min","SST.max")


RSL_sens<- merge (RSL.BOX_DATA, CO2.BOX_DATA, by.x="Period", by.y="Period")
RSL_sens<- merge (RSL_sens,SST.BOX_DATA,by.x="Period",by.y="Period")


RSL_sens<-  RSL_sens %>%
  mutate(RSL.CO2=(RSL.q50)/(CO2.q50),
         RSL.SST=(RSL.q50)/(SST.q50),
         SST.CO2=(SST.q50/CO2.q50),
         RSL.CO2max=(RSL.q75)/(CO2.q25),
         RSL.SSTmax=(RSL.q75)/(SST.q25),
         SST.CO2max=(SST.q75/CO2.q25),
         RSL.CO2min=(RSL.q25)/(CO2.q75),
         RSL.SSTmin=(RSL.q25)/(SST.q75),
         SST.CO2min=(SST.q25/CO2.q75),
         Time.ka=(RSL.time.max-RSL.time.min)/2+RSL.time.min)

RSL_sens<- full_join(RSL_sens,LGM_sens)
RSL_sens$Period <- factor(RSL_sens$Period, levels=c("LGM","midPleist","EarlymidPleist","EarlyPleist","LatePlio","midPlio"))


ggplot(RSL_sens,aes(x=Time.ka))+
  geom_point(aes(y=RSL.CO2*10),colour='red')+
  geom_point(aes(y=RSL.SST),colour='blue')+
  geom_point(aes(y=SST.CO2*20),colour='black')+
  geom_errorbar(aes(ymin=RSL.CO2min*10,ymax=RSL.CO2max*10),colour='red')+
  geom_errorbar(aes(ymin=RSL.SSTmin,ymax=RSL.SSTmax),colour='blue')+
  geom_errorbar(aes(ymin=SST.CO2min*20,ymax=SST.CO2max*20),colour='black')+
  annotate("text",x=c(1700,1700,1700),y=c(4.5,4,3.5),label=c("RSL.CO2 (x10)","RSL.SST","SST.CO2 (x10)"),col=c("red","blue","black"))+
  scale_x_continuous(name="Time (ka)",limits=c(0,3500),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=3500,by=100))+
  scale_y_continuous(name="Sensitvity",limits=c(0,5),expand=expansion(mult=c(0,0)),breaks = seq(from=0,to=5,by=1))




#########################################################################################################################################
########################################## Geom Split  Violin function #################################################################

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
