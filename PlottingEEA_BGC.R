################################################################################################  Merge and Plot codes for Enzyme and Biogeochemical data sets  #############
#########                   Noelle Espinosa; July 12, 2015              ##############
#######################################################################################

#potentially useful packages 
library(dplyr)
library(shiny)
library(ggplot2)

# load EEA and BGC data set

dat.e <- read.csv (
  "/Users/Noelle/HiPk2013/data/MicrobialExoEnzymeActivities_COMASTER_README.csv", 
  as.is = T)
dat.b <- read.csv ("/Users/Noelle/HiPk2013/data/HighPark_BGCpools_MASTER_r.csv", as.is = T)


#subset enzyme data frames by site ID's that match those in BGC data set
dat.e2 <- subset (dat.e, dat.e$PLOTID %in% dat.b$PLOTID)

#merge the two dataframes
dat.eb <- merge(dat.e2, dat.b)

#### plotting much from here on out ####

# simple plot
phos15 <- qplot(PO4_3m, PHOS_15C, data = dat.eb, color = SITE, main = "Phosphatase activity against soil phosphate concentrations")
phos

# Make a linear model for the response variable PHOS and view the summary
lm.p15 <- lm (dat.eb$PHOS_15C~ dat.eb$PO4_3m)
summary(lm)

#create slope and intercept objects to input into geom_abline() function
int <- summary(lm.p15)$coefficients [1,1]
slope <- summary(lm.p15)$coefficients [2,1]

# add the regression line to the original plot
phos15 <- phos15 + geom_abline (intercept=int, slope=slope)
phos15

# Check the trend for Phosphatase at other incubation temperatures
phos25 <- qplot(PO4_3m, PHOS_25C, data = dat.eb, color = SITE, main = "Phosphatase activity against soil phosphate concentrations")
phos25
lm.p25 <- lm( dat.eb$PHOS_25 ~ dat.eb$PO4_3m)
int <- summary(lm.p25)$coefficients [1,1]
slope <- summary(lm.p25)$coefficients [2,1]
phos25 <- phos25 + geom_abline (intercept=int, slope=slope)
phos25

phos35 <- qplot(PO4_3m, PHOS_35C, data = dat.eb, color = SITE, main = "Phosphatase activity against soil phosphate concentrations")
phos35
lm.p35 <- lm( dat.eb$PHOS_35 ~ dat.eb$PO4_3m)
int <- summary(lm.p35)$coefficients [1,1]
slope <- summary(lm.p35)$coefficients [2,1]
phos35 <- phos35 + geom_abline (intercept=int, slope=slope)
phos35


# plot of phosphatase against phosphate soil concentrations with regression lines by site

qplot(PO4_3m, PHOS_15C, data = dat.eb, geom = c("point", "smooth"), color = SITE, method = "lm", formula = y~x, main = "Regressions of Phosphate on Phosphatase")

# plot of biomass for each site
qplot(SITE, Biomass_C_final, data = dat.eb, geom = "boxplot")

qplot (Biomass_C_final, PHOS_15C, data = dat.eb, geom = "point", color = SITE)

# Might be interesteding to see enzyme activity per unit biomass: add new column to
# dataframe with EEA of phos/ microbial biomass C
