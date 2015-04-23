# This script will assume that you have a file structure that includes a folder called "data" in your rProject folder
# If you don't ... create one ... then download all the datafiles from iPlant

#all files are dot.csv files - you can open them any number of ways - I've opted to use read.csv
#data files that are dot.xlsx might be interesting toBGo but they are not pulled into R - they should contain identical data to the dot.csv files
#information about the headers and contents of each file is contained within the associated README.txt file (with the shared filename)


#read in Biogeochemistry data file into dataframe "BGCdat" - 72 observations
BGCdat = read.csv("data/HighPark_BGCpools_MASTER_r.csv",header=TRUE)
#TO DO 03/05/3015: CONTAINS ONLY LSP, SPR, UMC - MUST ADD NWT & FEF FROM NICOLE


#read in MASTER disturbance information into dataframe "DISdef" (no actual data here just plot definitions) - 98 observations
DISdef = read.csv("data/MASTERPLOT_DISTURBANCE_INFO.csv",header=TRUE)

#read in Enzyme activity data file into dataframe "ENZdat" - 178 observations
ENZdat = read.csv("data/MicrobialExoEnzymeActivities_COMASTER_README.csv",header=TRUE)

#read in Licor Efflux data file into dataframe "FLUXdat" - 1677 observations
#note Noelle - you might model your River Road data structure on this file
FLUXdat = read.csv("data/RAPID2013_MasterFluxes.csv",header=TRUE)


#read in Pyrolysis summary data file into dataframe "PYRdat" - 114 observations
PYRdat = read.csv("data/PyrolysisAllSitesRelativeProportions_MASTER.csv",header=TRUE)

#to do:
#read in the series of topographic data files from Rebecca's analysis

TRNdat = read.table("data/Final_Terrain4715.txt", sep="\t", header=TRUE)

#next steps:
#explore and plot variables within each data frame
#calculate summary statistics for key variables
#merge key variables to allow regression analysis

#after you have read in the data - save the bundle of dataframes to an rData file so that you can load it later.
save (BGCdat,DISdef, ENZdat, FLUXdat, PYRdat, TRNdat, file="HiPK2013.rda")