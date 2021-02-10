# -----------------------------------------------------
# Librairies
# -----------------------------------------------------
library(data.table)
library(readr)
library(tidyverse)
library(readxl)
library(purrr)
library(funModeling)
library(plotly)
library(ggplot2)
library(lubridate)
library(magrittr)
library(R.utils)
library(xgboost)
library(MLmetrics)

options( "digits"=1, "scipen"=100)


# -----------------------------------------------------
# Import
# -----------------------------------------------------

load("./Data/don3.RData")

type <- sapply(don3,class)
don4 <- don3[,type %in% c("numeric","integer")]

#Y
don4<-don4[,order(names(don4))]

set.seed(1234)
library(caret)
a <- createDataPartition(don4$Valeur.fonciere,p=0.8,list=F)
donapp <- don4[a,]
dontest <- don4[-a,]
dim(donapp)
dim(dontest)

# -----------------------------------------------------
# Selection des listes
# -----------------------------------------------------

# Liste Equipe
Liste1 <- c("Bpe_proximite_Services_aux_particuliers",
            "Bpe_proximite_Sports_loisirs_culture",
            "Bpe_proximite_Taxi_VTC",
            "Bpe_superieure_Services_aux_particuliers",
            "Code.type.local",
            "Dyn_DepMoySalHor",
            "Dyn_NbCommerces",
            "Dyn_NbInfLib",
            "Dyn_NbResidSecond",
            "Dyn_RevDeptMoy",
            "Dyn_ScoreCroissPopu",
            "Dyn_ScoreEquipSante",
            "Dyn_ScoreFiscal",
            "Dyn_ScoreUrba",
            "Inst_BaseLois",
            "Inst_Milit",
            "IndInflaB2015Mens",
            "Surface.reelle.bati",
            "CodGeoNum",
            "Date.mutationNum",
            "Valeur.fonciere"
            )

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Regression lineaire
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

donapptemp<-donapp[,colnames(donapp) %in% Liste1]
dontesttemp<-dontest[,colnames(dontest) %in% Liste1]

n<-ncol(donapptemp)
n0<-ncol(donapptemp)-1

vfmodel1.0 <- lm(Valeur.fonciere ~ ., data=donapptemp)
summary(vfmodel1.0)

pred_model1.0<-predict(vfmodel1.0, newdata=dontesttemp)

MLmetrics::RMSE(exp(pred_model1.0),exp(dontesttemp[,n]))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# XGBoost simple
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

donapptemp<-donapp[,colnames(donapp) %in% Liste1]
dontesttemp<-dontest[,colnames(dontest) %in% Liste1]

n<-ncol(donapptemp)
n0<-ncol(donapptemp)-1

X_train1 <- xgb.DMatrix(as.matrix(donapptemp[,1:n0]),label=donapptemp[,n])
X_test1 <- xgb.DMatrix(as.matrix(dontesttemp[,1:n0]),label=dontesttemp[,n])

watchlist <- list(train = X_train1, eval = X_test1)
param <- list(max_depth = 6, eta = 0.3, verbose = 0, nthread = 3, objective = "reg:squarederror", eval_metric = "rmse")
bstOri1 <- xgb.train(param, X_train1, nrounds =7, watchlist)
predicted1 <- predict(bstOri1, X_test1)
MLmetrics::R2_Score((predicted1),(dontesttemp[,n]))*1000
MLmetrics::RMSE((predicted1),(dontesttemp[,n]))

# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# XGBoost optimisé
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

donapptemp<-donapp[,colnames(donapp) %in% Liste1]
dontesttemp<-dontest[,colnames(dontest) %in% Liste1]

n<-ncol(donapptemp)
n0<-ncol(donapptemp)-1

library(caret)
library(xgboost)
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 500, 
  eta = 0.3, 
  max_depth = 6, 
  subsample = 1,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample= 1,
  gamma = 0 
)


xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE, 
  returnData = FALSE, 
  returnResamp = "all",       
  #classProbs = TRUE,          
  # summaryFunction = twoClassSummary, 
  allowParallel = TRUE
)


set.seed(1)

X_train = xgb.DMatrix(as.matrix(donapptemp[,1:n0]))
y_train = donapptemp[,n]

model16.1 = train(
  x = X_train,
  y = y_train,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

predicted1 <- predict(model16.1, X_test1)
MLmetrics::R2_Score(exp(predicted1),exp(dontesttemp[,n]))*1000
MLmetrics::RMSE(exp(predicted1),exp(dontesttemp[,n]))



# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# Autres modeles de boosting
# -------------------------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------------

donapptemp<-donapp[,colnames(donapp) %in% Liste1]
dontesttemp<-dontest[,colnames(dontest) %in% Liste1]

n<-ncol(donapptemp)
n0<-ncol(donapptemp)-1


library(gbm)
set.seed(1234)
modele.ada<-gbm(Valeur.fonciere~., data=donapptemp, distribution="laplace", shrinkage=0.01, n.trees=10)
prev.ada <- predict(modele.ada,newdata=dontesttemp,type="response", n.trees=10)
MLmetrics::R2_Score(exp(prev.ada),exp(dontesttemp[,n]))*1000
MLmetrics::RMSE(exp(prev.ada),exp(dontesttemp[,n]))


set.seed(1234)
modele.ada2 <- gbm(Valeur.fonciere~., data=donapptemp,distribution="laplace",cv.folds=5, shrinkage=0.01,n.trees=10)
Mopt.ada <- gbm.perf(modele.ada2,method="cv")
Mopt.ada

# Variables les plus importantes
summary(modele.ada2)[1:10,]

prev.ada <- predict(modele.ada2,newdata=dontest,type="response", n.trees=Mopt.ada)

MLmetrics::R2_Score(exp(prev.ada),exp(dontesttemp[,n]))*1000
MLmetrics::RMSE(exp(prev.ada),exp(dontesttemp[,n]))


