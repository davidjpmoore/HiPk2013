#Purpose: Combine Enzyme and BGC datasets into a single dataframe and plot them
#author Noelle Espinosa 
#date: 4/16/2015

#load data file 
#run 'inputscript.R'
# then run 02AddDate_SaveDFs.R
#Since I've already done that ... just load data

load (file="HiPK2013.rda")

#Enzyme dataset has more sites than BGC dataset 
unique(BGCdat$SITE)
unique(ENZdat$SITE)

summary(BGCdat)

# Example of how to use tapply
# #calculate weekly and daily values for plots
# weeklyNEE= tapply(USNR1_99on$NEE, weeknumIND, mean, na.rm = TRUE)
