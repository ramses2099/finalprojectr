  ##################################################
  ##
  ### Classification Demonstration                 
  #                                               
  ##################################################
  # Written by 
  # ID: 
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
  setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_12")
  
  options(scipen=9)
  
  ##################################################
  ### Install Libraries                           ##
  ##################################################
  
  #If the library is not already downloaded, download it
  
  if(!require(polycor)){install.packages("polycor")}
  library(polycor)
  
  ##################################################
  ### Read data and do preliminary data checks    ##
  ##################################################
  
  # Read "comma separated value" files (".csv")
  # Blood Donation data set
  Blood <- read.csv("BloodDonate.csv", header = TRUE, sep = ",")
 
  head(Blood,8)  #Print a Few Observations to Verify
  
  #Rename for easier interpretation
  
  names(Blood) <- c("Rec", "Nbr", "Vol", "Tim", "Gender", "Donate")
  
  str(Blood)
  
  Blood <- as.data.frame(unclass(Blood), stringsAsFactors = TRUE)
  
  summary(Blood)

  ##################################################
  ### Data Exploration and Pre-processing:            
  ##################################################
  
  pairs(Blood[sapply(Blood,is.numeric)], pch=46)
  
  ht <- hetcor(Blood)  #from polycor library
  round(ht$correlations,2)
  
  str(Blood)
  
  Blood$Donate <- as.factor(Blood$Donate)
  
  # Other important aspects of preprocessing: 
  #  - outlier
  #  - dimension reduction
  #  - data normalization
  
  ##################################################
  ### train/test data split                         
  ##################################################
  train_sr <- 0.7
  n_row <- nrow(Blood)
  
  set.seed(1)
  train_idx <- sample(1:n_row, (train_sr * n_row), replace=FALSE)
  
  Blood_train <- subset(Blood[train_idx,])
  
  Blood_test <- subset(Blood[-c(train_idx),])
  
  X_test <- Blood_test[,1:5]
  Y_test_gt <- Blood_test[,6]
  
  ##################################################
  ### Logistic Regression(LogReg)                   
  ##################################################
  
  # Train a Logistic Regression Model:
  LogReg_train_time_start <- Sys.time
  logReg_blood = glm(Donate ~ . ,
                family="binomial", data=Blood_train, na.action=na.omit)
  
  LogReg_train_time_end <- Sys.time
  
  LogReg_train_time <- LogReg_train_time_end - LogReg_train_time_start
  
  summary(logReg_blood)
  
  
  # Prediction:
  Prob_pred_LogReg <- predict(logReg_blood, newdata = X_test, type="response")
  Y_test_pred_LogReg <- ifelse(Prob_pred_LogReg > 0.5,"Y","N")
  
  # compute inference time: prediction_time_per_sample <- total_prediction_time/num_sample_test
  
  
  # Performance:
  CF_LogReg <- table(Y_test_gt, Y_test_pred_LogReg,
                      dnn=list("Act Donate","Predicted") ) 
  CF_LogReg
  
  Accu_test_LogReg <- (CF_LogReg[2,2] + CF_LogReg[1,1])/sum(CF_LogReg)
  round(Accu_test_LogReg, 4)
  
  # other aspects in measuring performance:
  #  - precision, recall, F1
  #  - computation time: model training time, prediction/inference time
  
  
  ##################################################
  ### Naive Bayes Classifier(NBC)
  ##################################################  
  if(!require(klaR)){install.packages("klaR")}
  library("klaR")
  
  # train NBC:
  NB_blood <- NaiveBayes(Donate ~ . ,
                       data = Blood_train, na.action=na.omit)
  
  # predict:
  pred_NB <- predict(NB_blood, newdata=X_test)
  Y_test_pred_NB <- pred_NB$class
  
  # performance:
  CF_NB <- table(Y_test_gt, Y_test_pred_NB,
                 dnn=list("Act Donate","Predicted") ) 
  
  CF_NB
  
  Accu_test_NB <- (CF_NB[2,2] + CF_NB[1,1])/sum(CF_NB)
  round(Accu_test_NB, 4)
  
  
  ##################################################
  ### k-Nearest Neighbor(kNN)
  ##################################################  
  library(class) 
  
  # train:
  X_train <- Blood_train[, 1:4]
  Y_train <- Blood_train[, 6]
  
  # predict:
  Y_test_pred_kNN <- knn(train = X_train, 
                          test = X_test[,1:4], 
                          cl = Y_train, 
                          k = 5) 
  
  # performance:
  CF_kNN <- table(Y_test_gt, Y_test_pred_kNN,
                 dnn=list("Act Donate","Predicted") ) 
  
  CF_kNN
  
  Accu_test_kNN <- (CF_kNN[2,2] + CF_kNN[1,1])/sum(CF_kNN)
  round(Accu_test_kNN, 4)
  
  
  
  ##################################################
  ### Neural Network-MLP (NN)
  ##################################################
  library(nnet)
  set.seed(123)
  
  # Train a neural network model:
  NN_model_blood <- nnet(Donate ~ Rec + Nbr + Vol + Tim, 
                         data = Blood_train, 
                         size = 5, maxit = 200, rang=0.001)
  
  # predict on test data:
  pred_NN <- predict(NN_model_blood, newdata = X_test, type = 'class')
  Y_test_pred_NN <- factor(pred_NN, levels = levels(Blood_train$Donate))
  
  # performance:
  CF_NN <- table(Y_test_gt, Y_test_pred_NN,
                 dnn=list("Act Donate","Predicted") ) 
  
  CF_NN
  
  Accu_test_NN <- (CF_NN[2,2] + CF_NN[1,1])/sum(CF_NN)
  round(Accu_test_NN, 4)
  
  
  