##################################################
### PROG8430                                    ##
### Multiple Linear Regression - Demo           ## 
##################################################
#                                               ##
##################################################
# Written by 
# ID: 123456789
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_10")

options(scipen=9)

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

##################################################
### Read in Data                                ##
##################################################

# Read "comma separated value" files (".csv")
# Thunder Basin Dataset
Thunder <- read.csv("ThunderBasin1.csv", header = TRUE, sep = ",")

##################################################
### Rename and Clean Variables                  ##
##################################################

#Rename Variables to something meaningful

str(Thunder)

names(Thunder) <- c("Fwn", "Adt", "Prc", "Sev")

str(Thunder)

##################################################
### Description of Data                         ##
##################################################

plot(Fwn ~ Adt, data=Thunder,
     main = 'Fawns vs Adult',
     pch = 20)
plot(Fwn ~ Prc, data=Thunder,
     main = 'Fawns vs. Precipitation',
     pch = 20)
plot(Fwn ~ Sev, data=Thunder)

pairs(Thunder)
Corr <- cor(Thunder)
round(Corr,2)

##################################################
### Create a Model                              ##
##################################################

#Create a Simple Linear Model for each dataset and print specifications

#There are three variables
#Taken Individually

# ------------------------------------------------------------
Tdr_Adt <- lm(Fwn ~ Adt, data=Thunder)
summary(Tdr_Adt)

# ------------------------------------------------------------
Tdr_Prc <- lm(Fwn ~ Prc, data=Thunder)
summary(Tdr_Prc)

# overlay the fitted line:
plot(Fwn ~ Prc, data=Thunder,
     main = 'Fawns vs Adult',
     pch = 20)
abline(Tdr_Prc, lwd=3, lty=2, col=2)
# ------------------------------------------------------------

Tdr_Sev <- lm(Fwn ~ Sev, data=Thunder)
summary(Tdr_Sev)

Tdr_AP <- lm(Fwn ~ Adt + Prc, data=Thunder)
summary(Tdr_AP)

Tdr_AS <- lm(Fwn ~ Adt + Sev, data=Thunder)
summary(Tdr_AS)

Tdr_PS <- lm(Fwn ~ Prc + Sev, data=Thunder)
summary(Tdr_PS)

Tdr_APS <- lm(Fwn ~ Adt + Prc + Sev, data=Thunder )
summary(Tdr_APS)

## 
## lm with feature selection:
## ------------------------------------------------------------
Tdr_Int <- lm(Fwn ~ 1, data = Thunder)
Tdr_Int

# forward selection:
Tdr_fwd <- step(Tdr_Int, direction="forward", scope=formula(Tdr_APS),
                trace=TRUE)
summary(Tdr_fwd)

# backward selection:
Tdr_bwd <- step(Tdr_APS, direction="backward", trace = TRUE)
summary(Tdr_bwd)

# both/stepwise:
Tdr_step <- step(Tdr_Int, direction="both", scope=formula(Tdr_APS),
                 trace=TRUE)
summary(Tdr_step)


