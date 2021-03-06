#USEFUL data munging libraries
#install the dplyr package then install the libraries below
library(dplyr)
library(shiny)
library(ggplot2)
#Load the HiPK2013 dataset containing all the data frames

load (file="HiPK2013.rdata")

#to diplay data in dataframes as summary tables
tbl_df(FLUXdat)
glimpse(FLUXdat)
#View(FLUXdat) #same as double clicking or 'inspect'
###################################################################
        #Dates - converting different dates to r formats
###################################################################

#R likes dates to start with the year, the licor and other lab kit push dates out in the format MO/DA/YEAR e.g. 08/12/2013 in R this is coded as "%m/%d/%Y"
#it makes much more sense for us to use R date conventions so we have to tell R what format our dates are in.

#Adding a NEW VARIABLE called rDate to the Enzyme data frame
#from rDate I can ask R to generate year, month, week of year, day of month etc
ENZdat$rDate = as.Date(ENZdat$SampleDate, "%m/%d/%Y")
ENZdat$month = as.numeric(strftime(ENZdat$rDate, format = "%m"))
ENZdat$week = as.numeric(strftime(ENZdat$rDate, format = "%W"))
summary(ENZdat$week)
#not an ISOdate

#BGCdat has Date stored as a number instead of a string
#convert it - this may be clunky but I converted the Number to a character and then defined separate character variables for YEAR, MONTH and DAY, then combined them to create an rDate object which is comparable to ENZdat$rDate 
D1=as.character(BGCdat$Date)
    BGCdat$YEAR = substr(D1, 1, 4)
    BGCdat$MONTH = substr(D1, 5, 6)
    BGCdat$DAY = substr(D1, 7, 8)
BGCdat$rDate  = ISOdate(BGCdat$YEAR,BGCdat$MONTH,BGCdat$DAY)
BGCdat$month = as.numeric(strftime(BGCdat$rDate, format = "%m"))
BGCdat$week = as.numeric(strftime(BGCdat$rDate, format = "%W"))

#FLUXdat is similar to ENZdat
F1=as.character(FLUXdat$Date)
FLUXdat$YEAR = substr(F1, 1, 4)
FLUXdat$MONTH = substr(F1, 5, 6)
FLUXdat$DAY = substr(F1, 7, 8)
FLUXdat$rDate  = ISOdate(FLUXdat$YEAR,FLUXdat$MONTH,FLUXdat$DAY)

#PYRdat has dates listed as YEAR, MONTH and DAY which are NUMBERS
PYRdat$rDate =as.Date(ISOdate (PYRdat$YEAR,PYRdat$MONTH,PYRdat$DAY))
#need to add the HHMMSS to the rDate variable for Flux data







#
pH_bp1 = tapply(ENZdat$pH,ENZdat[,c("PLOTID")],mean, na.rm=TRUE)