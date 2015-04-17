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

# Trying to make temp subset dataframes of UMC, LSP and SPR from ENZdat and BGCdat
temp1 <- filter(ENZdat, SITE %in% c("UMC", "LSP", "SPR"))
head(temp1)
unique(temp1$SITE)
View (temp1)
temp2 <- filter(BGCdat, SITE %in% c("UMC", "LSP", "SPR"))

# Example of how to use tapply
# #calculate weekly and daily values for plots
# weeklyNEE= tapply(USNR1_99on$NEE, weeknumIND, mean, na.rm = TRUE)
