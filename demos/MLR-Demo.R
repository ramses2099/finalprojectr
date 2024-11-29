##################################################
### PROG8435                                    ##
### Multiple Linear Regression - Demo           ## 
##################################################
#                                               ##
##################################################
# Written by
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

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

##################################################
### Read in Data                                ##
##################################################

##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read text file (".txt")

# Training Data
train <- read.table("MLR_TrainData.txt", header = TRUE, sep = ",")

train[5:15,]     #Prints data 5 to 15 to make sure it looks correct

str(train)     

###################################################
## Preliminary data transformation               ##
###################################################

### Note that the data in this dataset has been standardized.

###################################################
## Univariate Descriptive Analysis               ##
###################################################

round(stat.desc(train),2)

par(mfrow=c(3,2))    

for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    hist(train[,i], main=names(train)[i],xlab="")
  }
}

par(mfrow=c(1,1))


###################################################
## Find Outliers                                 ##
###################################################

par(mfrow=c(3,2))


for (i in 1:ncol(train)) {
  if (is.numeric(train[,i])) {
    boxplot(train[,i], main=names(train)[i],xlab="", horizontal=TRUE)
  }
}

par(mfrow=c(1,1))

#########################################
## Checking Correlations               ##
#########################################

corrgram(train, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlations")

trn_cr <- cor(train, method="spearman")
round(trn_cr, 2)

#########################################
## Creating Baseline/Full Model        ##
#########################################

full.model = lm(Saving ~ . ,
            data=train, na.action=na.omit)

summary(full.model)

pred <- predict(full.model, newdata=train)

RMSE_trn_full <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_full,2)

#########################################
## Creating Backward Selection Model   ##
#########################################

back.model = step(full.model, direction="backward", details=TRUE)

summary(back.model)

pred <- predict(back.model, newdata=train)

RMSE_trn_back <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_back,2)

#########################################
## Creating Stepwise Selection Model   ##
#########################################

step.model <- step(full.model, direction="both")

summary(step.model)

pred <- predict(step.model, newdata=train)

RMSE_trn_step <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_step,2)

#######
### Manual Model (for fun)
#######

man.mod <- lm(Saving ~ Mthly_Save + Age + House,
                 data=train, na.action=na.omit)

summary(man.mod)

pred <- predict(man.mod, newdata=train)

RMSE_trn_man <- sqrt(mean((train$Saving - pred)^2))
round(RMSE_trn_man,2)


#########################################
## Evaluating the Models               ##
#########################################

#Graphically

par(mfrow = c(2, 2))  
plot(full.model)  
par(mfrow = c(1, 1))  

par(mfrow = c(2, 2))  
plot(back.model)  
par(mfrow = c(1, 1))  

par(mfrow = c(2, 2))  
plot(step.model)  
par(mfrow = c(1, 1))  

par(mfrow = c(2, 2))  
plot(man.mod)  
par(mfrow = c(1, 1))  

###########################################
## Creating Model and Residual vectors    #
###########################################

full.res <- residuals(full.model)
back.res <- residuals(back.model)
step.res <- residuals(step.model)

#Check Normality Numericaly

shapiro.test(full.res)
shapiro.test(back.res)
shapiro.test(step.res)

###########################################
## Comparing to the Test Set              #
###########################################

test <- read.table("MLR_TestData.txt", header = TRUE, sep = ",")

pred <- predict(full.model, newdata=test)

RMSE_tst_full <- sqrt(mean((test$Saving - pred)^2))

pred <- predict(back.model, newdata=test)

RMSE_tst_back <- sqrt(mean((test$Saving - pred)^2))

pred <- predict(step.model, newdata=test)

RMSE_tst_step <- sqrt(mean((test$Saving - pred)^2))

pred <- predict(man.mod, newdata=test)

RMSE_tst_man <- sqrt(mean((test$Saving - pred)^2))

## Compare all RMSE

RMSE_full <- c(RMSE_trn_full,RMSE_tst_full)
round(RMSE_full,2)

RMSE_back <- c(RMSE_trn_back,RMSE_tst_back)
round(RMSE_back,2)

RMSE_step <- c(RMSE_trn_step,RMSE_tst_step)
round(RMSE_step,2)

RMSE_man <- c(RMSE_trn_man,RMSE_tst_man)
round(RMSE_man,2)

