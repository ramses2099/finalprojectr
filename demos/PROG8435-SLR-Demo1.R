##################################################
### PROG8435                                    ##
### Simple Linear Regression - Demo             ## 
##################################################
#                                               ##
##################################################
# ID: 123456
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

setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_9")

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
# This also works for txt files.

# Systolic Blood Pressure Dataset
Systolic <- read.table("Systolic1.csv", header = TRUE, sep = ",")

# Read "comma separated value" files (".csv")
# Thunder Basin Dataset
Thunder <- read.table("ThunderBasin1.csv", header = TRUE, sep = ",")

##################################################
### Rename and Clean Variables                  ##
##################################################

#Rename Variables to something meaningful

str(Systolic)

names(Systolic) <- c("BP", "Age", "Wgt")

str(Systolic)

str(Thunder)

names(Thunder) <- c("Fwn", "Adt", "Prc", "Sev")

str(Thunder)

##################################################
### Description of Data                         ##
##################################################

#Systolic

SysSum <-stat.desc(Systolic)
format(SysSum,digits=2)

densityplot( ~BP, dat=Systolic, main="Distribution of BP")

densityplot( ~ Age, dat=Systolic, main="Distribution of Age")

densityplot( ~ Wgt, dat=Systolic)

#Thunder Basin

TdrSum <-stat.desc(Thunder)
format(TdrSum,digits=2)

densityplot( ~Fwn, dat=Thunder, main="Dist of Fawns")

densityplot( ~Adt, dat=Thunder,  main="Dist of Adults")

densityplot( ~Prc, dat=Thunder)

densityplot( ~Sev, dat=Thunder)

#Scatter plots of Systolic and Thunder (and correlation)

plot(BP ~ Age, data=Systolic,
     main="Comparing BP to Age")

cor(Systolic$BP, Systolic$Age, method="spearman")

plot(Fwn ~ Adt, data=Thunder,
     main="Comparing Spring Births")

cor(Thunder$Fwn, Thunder$Adt, method="spearman")

##################################################
### Create a Model                              ##
##################################################

#Create a Simple Linear Model for each dataset and print specifications

Sysmodel <- lm(BP ~ Age, data=Systolic)
Sysmodel

plot(BP ~ Age, data=Systolic,
     main="BP by Age (with Regression Line)")
abline(Sysmodel)

#Now the same for Thunder Basin data

Tdrmodel <- lm(Fwn ~ Adt, data=Thunder)
Tdrmodel

plot(Fwn ~ Adt, data=Thunder,
     main="Spring Fawns by Adult Population (with Regression Line)")
abline(Tdrmodel)


#Provide some total measures of fitness

summary(Sysmodel)

summary(Tdrmodel)


### Read in test data to compare to training 

Sys_tst <- read.table("Systolic_tst.csv", header = TRUE, sep = ",")

names(Sys_tst) <- c("BP", "Age", "Wgt")


### Comparing the RMSE

pred <- predict(Sysmodel, newdata=Systolic)

RMSE_trn <- sqrt(mean((Systolic$BP - pred)^2))
round(RMSE_trn,3)

pred <- predict(Sysmodel, newdata=Sys_tst)

RMSE_tst <- sqrt(mean((Sys_tst$BP - pred)^2))
round(RMSE_tst,3)

###
Sys_tst2 <- read.table("Systolic_tst2.csv", header = TRUE, sep = ",")

names(Sys_tst2) <- c("Age")

pred_bp2 <- predict(Sysmodel, newdata=Sys_tst2)





