#Purpose: Dynes et al 2015 Using Tree Rings to Estimate time since mortality for ecological disturbance
#Author: Dave Moore
#Date started: 03/25/2015
#
library (compare)
library (dplyr)
#Read Non-QAQC data from Rebecca
GPSjunk = read.csv("data/nonQAdata//WTFlux_All_Coordinates_120914_RL.csv",header=TRUE)
plot (GPSjunk$Site,GPSjunk$Tree_YOD)

# #read QAQC FEF dendro data
# SITE 'NWT: Niwot Ridge Ameriflux, FEF: Fraser Experimental Forest, SPR: Stove Praire Ranch (High Park Fire), UMC: United Methodist Church (High Park Fire), LSP: Lory State Park (High Park Fire)';
# PlotCODE_ID 'Three Letter code for within site identification';
# CORE: SITE(1char),PlotCODE_ID(3char),Tree(2char),rep(1char)  Identifier for each code letters A and B refer to replicate cores from the trees with the same tree within the PlotCODE_ID 
# Phase: Decay Class Phase see Methodological Information RED, EGREY (early grey), LGREY (late grey)
# Ph_YOD: Year of Death (midpoint) associated with Decay Phase
# D_YOD: Dendrochronological Year of Death *see metholodgy
# MS_corr: Correlation with Master Chronology

DENdat = read.csv("data/FEF2013DendroYOD.csv",header=TRUE)

#Figure Phase Estimated YOD with Dendro Estimated YOD

#Figure EFFLUX 

plot(DENdat$Phase, DENdat$D_YOD)
plot(DENdat$Phase, DENdat$Ph_YOD)
plot(DENdat$Ph_YOD)
plot(DISdef$SITE[DISdef$YOD>0],DISdef$YOD[DISdef$YOD>0])

#MERGE fluxes with DISdef by PLOTID
# dplyr::full_join(a, b, by = "x1")

test =full_join(DISdef,FLUXdat, by = "PLOTID")
test1 = full_join(DISdef,DENdat, by = "SITE","PlotCODE_ID")
tbl_df(test1)

YSD = test1$D_YOD
YSD = -1*(YSD[YSD>0]-2013)
test1$YSD = YSD

test3 = full_join(test1,FLUXdat, by= "PLOTID")
library(plyr)
summarydata <- ddply(test3, c("PLOTID","Phase","Date"), summarise,
                     N    = length(EFFLUX),
                     mean = mean(EFFLUX),
                     sd   = sd(EFFLUX),
                     se   = sd / sqrt(N) )
summarydata


#SUBSTRING THE TREEID and Create Estimate by Tree
#Compare to High Park Trees 
#test whether two objects are identical

x1 <- compare(test3$SITE.x, test$PLOTID)
#x1 should be false
x <- compare(test3$SITE.x, test$SITE.y)
#x should be true

plot (test1$YOD,test1$D_YOD)

