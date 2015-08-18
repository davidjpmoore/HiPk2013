save (DisALL.join, file="HiPKmerge01.rda")


DisALL.join$Carb_met = DisALL.join$Lignin*DisALL.join$BG_25C*DisALL.join$DOC_Final
DisALL.join$NITmet = (DisALL.join$N.Bearing+DisALL.join$Protein)*DisALL.join$LAP_25C*DisALL.join$DON
NITmet <- ggplot(DisALL.join, aes(x=pH.x, y=NITmet))

CARBmet <- ggplot(DisALL.join, aes(x=pH.x, y=Carb_met))



BG <- ggplot(DisALL.join, aes(x=pH.x, y=BG_25C))
CB <- ggplot(DisALL.join, aes(x=pH.x, y=CB_25C))
LAP <- ggplot(DisALL.join, aes(x=pH.x, y=LAP_25C))
EFF <- ggplot(DisALL.join, aes(x=pH.x, y=EFFLUX))
LIG <- ggplot(DisALL.join, aes(x=pH.x, y=Lignin))
PSAC <- ggplot(DisALL.join, aes(x=pH.x, y=Polysaccharide))
PROT <- ggplot(DisALL.join, aes(x=pH.x, y=Protein))


PROTlap <- ggplot(DisALL.join, aes(x=LAP_25C, y=Protein))

EFFbg <- ggplot(DisALL.join, aes(x=BG_25C, y=EFFLUX))

APS <- ggplot(DisALL.join, aes(x=Aspect_PD_, y=EFFLUX))
ARC <- ggplot(DisALL.join, aes(x=ArcTWI_341, y=EFFLUX))

plot(DisALL.join$)

#create a simple plot
a + geom_point()

# color code by another variable within the dataframe
a + geom_point(aes(colour = LAP_15C)) + scale_colour_gradient(low = "yellow", high="red")
# bad data in LAP_15C

#carbonhydrate metabolism


CARBmet + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Carbohydrate Metabolism")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)



NITmet + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Nitrogren Metabolism")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)



EFFbg + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Soil Efflux")+
  xlab("Potential BG activity") +
  geom_smooth(method=lm,se=FALSE)




EFF + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Soil Efflux")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)
# Add linear regression line 
#  (by default includes 95% confidence region)


#Show the site differences with different shapes and Fire Severity as different colors
PROT + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Proportion of Protein compounds")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


#Show the site differences with different shapes and Fire Severity as different colors
PROTlap + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Proportion of Protein compounds")+
  xlab("Potential LAP Activity") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


#Show the site differences with different shapes and Fire Severity as different colors
LAP + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Potential LAP activity")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


#Show the site differences with different shapes and Fire Severity as different colors

CB + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Potential CB activity")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


BG + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Potential BG activity")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)




#Show the site differences with different shapes and Fire Severity as different colors
PSAC + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Proportion of Polysaccharide compounds")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


#Show the site differences with different shapes and Fire Severity as different colors
#LIGNIN across pH
LIG + aes(shape = factor(SITE)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Proportion of Lignin compounds")+
  xlab("pH") +
geom_smooth(method=lm,se=FALSE)
#+
#geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

LIG + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Proportion of Lignin compounds")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)



#EFFLUX PLOTS

#Show the site differences with different shapes and Fire Severity as different colors 
#EFFLUX and ASPECT
APS + aes(shape = factor(SITE)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Soil Efflux (umol CO2 m-2 s-1")+
  xlab("Aspect")+
  geom_smooth(method=lm,se=FALSE)

#Show the site differences with different shapes and Fire Severity as different colors
ARC + aes(shape = factor(SITE)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=20),
         axis.title=element_text(size=22,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Soil Efflux (umol CO2 m-2 s-1")+
  xlab("Total Wetness Index") +
  geom_smooth(method=lm,se=FALSE)

