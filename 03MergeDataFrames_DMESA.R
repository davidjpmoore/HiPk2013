#Purpose: Combine Enzyme and BGC datasets into a single dataframe and plot them
#author Noelle Espinosa 
#date: 4/16/2015

#load data file 
#run 'inputscript.R'
# then run 02AddDate_SaveDFs.R
#Since I've already done that ... just load data

load (file="HiPK2013.rda")

# open libraries
library(dplyr)
library(shiny)
library(ggplot2)

#Enzyme dataset has more sites than BGC dataset 
unique(BGCdat$SITE)
unique(TRNdat$PLOTID)
SITEtrn=as.factor(substr(TRNdat$PLOTID,1,3)) 

unique(SITEtrn)

summary(BGCdat)


library(plyr)
EffluxByPLOTID <- ddply(FLUXdat, c("PLOTID","rDate"), summarise,
                     N    = length(EFFLUX),
                     Effluxmean = mean(EFFLUX,na.rm=TRUE) ,
                     Effluxsd   = sd(EFFLUX,na.rm=TRUE),
                     Effluxse   = sd / sqrt(N),
                     Tmean = mean(Tsoil_C)
                     )
EffluxByPLOTID




library(plyr)
sumdata <- ddply(FLUXdat, c("PLOTID","rDate"), summarise,
                     N    = length(EFFLUX),
                     mean = mean(EFFLUX),
                     sd   = sd(EFFLUX),
                     se   = sd / sqrt(N),
                      Tmean = mean(Tsoil_C)
                 )
sumdata




# Trying to make temp subset dataframes of UMC, LSP and SPR from ENZdat and BGCdat

#try grouping by week
unique(FLUXdat$WEEK)

unique(DISdef$PLOTID)

temp1 <- filter(ENZdat, SITE %in% c("UMC", "LSP", "SPR"))
head(temp1)
unique(temp1$SITE)
View (temp2)
temp2 <- filter(BGCdat, SITE %in% c("UMC", "LSP", "SPR"))
temp3 <- filter(FLUXdat, SITE %in% c("UMC", "LSP", "SPR"))

tempDIS <- filter(DISdef, SITE %in% c("UMC", "LSP", "SPR"))

#merging Pyrolysis data and enzyme data
tempPYR <- filter(PYRdat, SITE %in% c("UMC", "LSP", "SPR"))
junkPYR =group_by(tempPYR,PLOTID)
junkPYR.WEEK = as.numeric(strftime(junkPYR$rDate, format = "%W"))

tempENZ <- filter(ENZdat, SITE %in% c("UMC", "LSP", "SPR"))
junkENZ =group_by(tempENZ,PLOTID)
junkENZ.WEEK = as.numeric(strftime(junkENZ$rDate, format = "%W"))

ENZPYR.join <- full_join( tempPYR, tempENZ, by= c("PLOTID","WEEK"))
View(ENZPYR.join)

#merging BGC data and flux data
tempBGC <- filter(BGCdat, SITE %in% c("UMC", "LSP", "SPR"))
junkBGC =group_by(tempBGC,PLOTID)
junkBGC.WEEK = as.numeric(strftime(junkBGC$rDate, format = "%W"))

tempFLUX <- filter(FLUXdat, SITE %in% c("UMC", "LSP", "SPR"))
junkFLUX =group_by(tempFLUX,PLOTID)
junkFLUX.WEEK = as.numeric(strftime(junkFLUX$rDate, format = "%W"))

junkTRN =group_by(TRNdat,PLOTID)


BGCFLUX.join <- full_join(junkBGC, junkFLUX, by= c("PLOTID","WEEK"))



DisTRN.join <- left_join(tempDIS,junkTRN, by= c("PLOTID"))

DisBGCFLUX.join <- left_join(DisTRN.join ,BGCFLUX.join, by= c("PLOTID"))

plot (DisBGCFLUX.join$Aspect_PD_,DisBGCFLUX.join$)

DisALL.join <- left_join( DisBGCFLUX.join, ENZPYR.join, by= c("PLOTID","WEEK"))
View(DisALL.join)





#
junk1 =group_by(temp1,PLOTID)
junk2 =group_by(temp2,PLOTID)
junk3 =group_by(temp3,PLOTID)
unique(junk3$rDate)
unique(junk2$rDate)

unique(FLUXdat$rDate)
unique(BGCdat$rDate)


#you can use join functions using multiple vectors
#http://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key

temp.join <- full_join( temp1, temp2, temp3, by= c("PLOTID","WEEK"))
View(temp.join)


temp2.join <- full_join(temp.join, temp4, by= c("PLOTID"))


BCmeanBeetle <- ddply(temp2.join, c("BarkBeetle"), summarise,
                 N    = length(Biomass_C_final),
                 BCmean = mean(Biomass_C_final),
                 sd   = sd(Biomass_C_final),
                 se   = sd / sqrt(N)
)
BCmeanBeetle



T2join.lowpH <- subset(temp2.join, temp2.join$pH<5) 
enzJunk1.lowpH <- subset(enzJunk1, temp2.join$pH<5)

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

# Example of how to use tapply
# #calculate weekly and daily values for plots
# weeklyNEE= tapply(USNR1_99on$NEE, weeknumIND, mean, na.rm = TRUE)
