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
unique(ENZdat$SITE)

summary(BGCdat)


library(plyr)
EffluxByPLOTID <- ddply(FLUXdat, c("PLOTID","rDate"), summarise,
                     N    = length(EFFLUX),
                     Effluxmean = mean(EFFLUX),
                     Effluxsd   = sd(EFFLUX),
                     Effluxse   = sd / sqrt(N),
                     Tmean = mean(Tsoil_C)
                     )
EffluxByPLOTID


library(plyr)
sumdata <- ddply(FLUXdat, c("PLOTID","rDate"), summarise,
                     N    = length(EFFLUX),
                     mean = mean(EFFLUX),
                     sd   = sd(EFFLUX),
                     se   = sd / sqrt(N) )
sumdata




# Trying to make temp subset dataframes of UMC, LSP and SPR from ENZdat and BGCdat
temp1 <- filter(ENZdat, SITE %in% c("UMC", "LSP", "SPR"))
head(temp1)
unique(temp1$SITE)
View (temp2)
temp2 <- filter(BGCdat, SITE %in% c("UMC", "LSP", "SPR"))

#
junk1 =group_by(temp1,PLOTID)
junk2 =group_by(temp2,PLOTID)


#you can use join functions using multiple vectors
#http://stackoverflow.com/questions/26611717/can-dplyr-join-on-multiple-columns-or-composite-key

temp.join <- full_join( temp1, temp2, by= c("PLOTID","rDate"))
View(temp.join)

left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

# Example of how to use tapply
# #calculate weekly and daily values for plots
# weeklyNEE= tapply(USNR1_99on$NEE, weeknumIND, mean, na.rm = TRUE)
