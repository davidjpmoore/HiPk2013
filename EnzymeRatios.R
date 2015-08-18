#Enzyme ratios
# Aug 17th
# Dave Moore
# Explore the relationships between the relative enzyme potential in relation to the relative abundance of different organic compounds. 

#Relavent ratios

#Relative contribution of different C related enzymes

#AG 
#BG CB XYL

plot (ENZdat$AG_25C, ENZdat$BG_25C)
plot (ENZdat$AG_25C, ENZdat$CB_25C)
plot (ENZdat$AG_25C, ENZdat$XYL_25C)
plot (ENZdat$BG_25C, ENZdat$CB_25C)
plot (ENZdat$BG_25C, ENZdat$XYL_25C)
plot (ENZdat$CB_25C, ENZdat$XYL_25C)

#CB and BG strongly covary - they both release saccharides from Cellulose
#While the others are all positively corellated they show substantial variance

RAT25_CBBG = DisALL.join$BG_25C/DisALL.join$CB_25C
plot (DisALL.join$pH,RAT25_CBBG)

RAT25_AGBG = DisALL.join$AG_25C/DisALL.join$BG_25C
plot (DisALL.join$pH.x,RAT25_CBBG)



plot (DisALL.join$pH.x)

DisALL.join$RAT25_AGXYL = DisALL.join$AG_25C/DisALL.join$XYL_25C
plot (DisALL.join$pH[RAT25_AGXYL<10],RAT25_AGXYL[RAT25_AGXYL<10])

AGXYL <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_AGXYL))

AGXYL + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Ratio of AG to XYL")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)


# Calculate ratio of alpha gluconase to leucine amino peptidase
DisALL.join$RAT25_AGLAP = DisALL.join$AG_25C/DisALL.join$LAP_25C
DisALL.join$RAT25_LAPAG = DisALL.join$LAP_25C/DisALL.join$AG_25C

# create GGPLOT2 object 
AGLAP <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_AGLAP))
LAPAG <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_LAPAG))
# create GGPLOT2 plot
LAPAG + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Ratio of LAP to AG")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)




# Calculate ratio of cellobiase to leucine amino peptidase
DisALL.join$RAT25_CBLAP = ((DisALL.join$CB_25C-DisALL.join$LAP_25C)/DisALL.join$LAP_25C)/120
DisALL.join$RAT25_LAPCB = ((DisALL.join$LAP_25C-DisALL.join$CB_25C)/DisALL.join$CB_25C)/17

# create GGPLOT2 object 
CBLAP <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_CBLAP))
LAPCB <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_LAPCB))
# create GGPLOT2 plot
CBLAP + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Ratio of CB to LAP")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)



# Calculate ratio of Phosphase to NAG
DisALL.join$RAT25_NAGPHOS = DisALL.join$NAG_25C/DisALL.join$PHOS_25C
DisALL.join$RAT25_PHOSNAG = DisALL.join$PHOS_25C/DisALL.join$NAG_25C

DisALL.join$NO3_m
DisALL.join$NH4_p
DisALL.join$N.Bearing


# create GGPLOT2 object 


NAGPHOS_pH <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_NAGPHOS))
PHOSNAG_pH <- ggplot(DisALL.join, aes(x=pH.x, y=RAT25_PHOSNAG))


NAGPHOS_PO4 <- ggplot(DisALL.join, aes(x=PO4_3m, y=RAT25_NAGPHOS))
PHOSNAG_PO4 <- ggplot(DisALL.join, aes(x=PO4_3m, y=RAT25_PHOSNAG))

PHOSNAG_NO3_m <- ggplot(DisALL.join, aes(x=NO3_m, y=RAT25_PHOSNAG))
PHOSNAG_NH4_p <- ggplot(DisALL.join, aes(x=NH4_p, y=RAT25_PHOSNAG))
PHOSNAG_NBearing <- ggplot(DisALL.join, aes(x=N.Bearing, y=RAT25_PHOSNAG))

plot (DisALL.join$pH.y,DisALL.join$PHOS_25C)

# create GGPLOT2 plot
PHOSNAG_pH + aes(shape = factor(Beetle_CODE.x)) +
  geom_point(aes(colour = factor(Fire_CODE.x)), size = 8) +
  geom_point(colour="grey90", size = 2.5) +
  theme_classic() +
  theme(axis.text=element_text(size=28),
        axis.title=element_text(size=30,face="bold")) + 
  theme(panel.border = element_blank(), axis.line = element_line(colour="black", size=2, lineend="square"))+
  theme(axis.ticks = element_line(colour="black", size=2, lineend="square"))+
  ylab("Ratio of PHOS to NAG")+
  xlab("pH") +
  geom_smooth(method=lm,se=FALSE)   # Add linear regression line 
#  (by default includes 95% confidence region)
