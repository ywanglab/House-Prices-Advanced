#############################################################
# Warning: The entire calculation needs around
#                  18.5 minutes
# on a PC with Intel Core i7-7700 CPU @ 3.60GHz with 16G RAM
#############################################################

######################################################################
#Acquiring data
#####################################################################
rm(list = ls()) #clear the environment
start_all <- proc.time() #start the clock

library(tidyverse)
library(caret)
library(data.table)
library(glmnet)
library(Matrix)

#Temporarily suppress warnings
defaultW <- getOption("warn")
options(warn = -1)

# Kaggle: Hourse Prices - Advanced Regression Techniques
# https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data/download
#
# downloading data

if (!exists("test.csv") & !exists("train.csv")) {
  dl <- tempfile()
  download.file(
    "https://github.com/ywanglab/House-Prices-Advanced/raw/main/house-prices-advanced-regression-techniques.zip",
    dl
  )
  unzip(dl)
  rm(dl)
}

# Initial data sets
train_set_0 <- read_csv("train.csv")
test_set_0 <- read_csv("test.csv")

#Check the dimension of the initial training set and test set
dim(train_set_0)
dim(test_set_0)

# Review the summary of the initial training set and test set
summary(train_set_0)
summary(test_set_0)

# find column names in the set train_set_0
colNames <- names(train_set_0)
colNames

# Check if there is NA in the data set
table(is.na(train_set_0))
table(is.na(test_set_0))

# Define a function to find the indexes of all  columns that   contain 'NA'
NA_col_ind <- function(data) {
  col_logic <- sapply(1:ncol(data) , function(i) {
    ind <- ifelse(sum(is.na(data[[i]])) > 0 , TRUE, FALSE)
    return(ind)
  }) # end of inner function
  col <- seq(1:ncol(data))[col_logic]
  return(col)
}

#Find columns that contain NA in train_set_0
train_NA_col <-
  NA_col_ind(train_set_0)
colNames[train_NA_col]

#Find columns that contain NA in test_set_0
test_NA_col <- NA_col_ind(test_set_0)
colNames[test_NA_col]

#For simplicity, remove all the columns contains NA in the train_set_0 and
#define the resulting set as train

train <- train_set_0[, -train_NA_col]

# check if there is any NA in the data set train:
table(is.na(train))

# Now convert all character columns into factor and then numeric
# Define a function to find the character columns in a data set
Char_col_ind <- function(data) {
  col_logic <- sapply(1:ncol(data) , function(i) {
    ind <- ifelse(class(data[[i]])  == "character" , TRUE, FALSE)
    return(ind)
  })
  col <- seq(1:ncol(data))[col_logic]
  return(col)
}

#Identify character columns in the set train
train_char_col <-
  Char_col_ind(train)

#Display all the character columns
colNames <- names(train)
colNames[train_char_col]

# For each of the character column,
# turn it into a factor column and then numeric column

data <-
  train # make a copy of the set train and put the copy in  data

# MSZoning: convert from char to numeric
table(data$MSZoning)
data <- data %>% mutate(MSZoning = as.numeric(factor(MSZoning)))
#C(all)   FV   RH   RL  RM
# 1       2    3     4   5

# Street
table(data$Street)
data <- data %>% mutate(Street = as.numeric(factor(Street)))

# LotShape
table(data$LotShape)
data <- data %>% mutate(LotShape = as.numeric(factor(LotShape)))

# LandContour
table(data$LandContour)
data <-
  data %>% mutate(LandContour = as.numeric(factor(LandContour)))

# Utilities
table(data$Utilities)
data <- data %>% mutate(Utilities = as.numeric(factor(Utilities)))

# LotConfig
table(data$LotConfig)
data <- data %>% mutate(LotConfig = as.numeric(factor(LotConfig)))

# Neighborhood
table(data$Neighborhood)
data <-
  data %>% mutate(Neighborhood = as.numeric(factor(Neighborhood)))

# LandSlope
table(data$LandSlope)
data <- data %>% mutate(LandSlope = as.numeric(factor(LandSlope)))

# Conditional1
table(data$Condition1)
data <- data %>% mutate(Condition1 = as.numeric(factor(Condition1)))

# Conditional2
table(data$Condition2)
data <- data %>% mutate(Condition2 = as.numeric(factor(Condition2)))

# BldgType
table(data$BldgType)
data <- data %>% mutate(BldgType = as.numeric(factor(BldgType)))

# HouseStyle
table(data$HouseStyle)
data <- data %>% mutate(HouseStyle = as.numeric(factor(HouseStyle)))

# RoofStyle
table(data$RoofStyle)
data <- data %>% mutate(RoofStyle = as.numeric(factor(RoofStyle)))

# RoofMatl
table(data$RoofMatl)
data <- data %>% mutate(RoofMatl = as.numeric(factor(RoofMatl)))

# Exterior1st
table(data$Exterior1st)
data <-
  data %>% mutate(Exterior1st = as.numeric(factor(Exterior1st)))

# Exterior2nd
table(data$Exterior2nd)
data <-
  data %>% mutate(Exterior2nd = as.numeric(factor(Exterior2nd)))

# ExterQual
table(data$ExterQual)
data <- data %>% mutate(ExterQual = as.numeric(factor(ExterQual)))

#ExterCond
table(data$ExterCond)
data <- data %>% mutate(ExterCond = as.numeric(factor(ExterCond)))

# Foundation
table(data$Foundation)
data <- data %>% mutate(Foundation = as.numeric(factor(Foundation)))

#Heating
table(data$Heating)
data <- data %>% mutate(Heating = as.numeric(factor(Heating)))

# HeatingQC
table(data$HeatingQC)
data <- data %>% mutate(HeatingQC = as.numeric(factor(HeatingQC)))

#CentralAir
table(data$CentralAir)
data <- data %>% mutate(CentralAir = as.numeric(factor(CentralAir)))

# KitchenQual
table(data$KitchenQual)
data <-
  data %>% mutate(KitchenQual = as.numeric(factor(KitchenQual)))

# Funcitonal
table(data$Functional)
data <- data %>% mutate(Functional = as.numeric(factor(Functional)))

# PaveDrive
table(data$PavedDrive)
data <- data %>% mutate(PavedDrive = as.numeric(factor(PavedDrive)))

# SaleType
table(data$SaleType)
data <- data %>% mutate(SaleType = as.numeric(factor(SaleType)))

# SaleCondition
table(data$SaleCondition)
data <-
  data %>% mutate(SaleCondition = as.numeric(factor(SaleCondition)))

# Check again if there is any character column
train_char_col <-
  Char_col_ind(data)
colNames <- names(train)
colNames[train_char_col]

############################################################################
# Create the train_set and test_set by splitting the set train0
###########################################################################
train0 <- data
set.seed(1, sample.kind = "Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <-
  createDataPartition(
    y = train0$SalePrice,
    times = 1,
    p = 0.2,
    list = FALSE
  )

train_set <- train0[-test_index,]
test_set <- train0[test_index,]

# remove the first column `Id`
train_set <- train_set[, -1]
test_set <- test_set[, -1]

#check the size of train_set and test_set
dim(train_set)
dim(test_set)

# Define the criterion function RMSE
#
RMSE <- function(ground, predicted) {
  sqrt(mean((ground - predicted) ^ 2))
}

###############################################
#Explore the nzv (Near-Zero_variance) columns
###############################################

library(matrixStats)
sds <- colSds(data.matrix(train_set))
qplot(log10(sds), bins = 20)

nzv <- nearZeroVar(train_set)

# number of nzv columns
length(nzv)

# find column names of nzv columns
colNames <- names(train_set)
colNames[nzv]

#sel_col is the remaining columns after removing nzv-columns
sel_col <- setdiff(1:ncol(train_set), nzv)

#number of columns after removing the nzv columns
length(sel_col)


########################################################
#KNN  using all columns (without removing nzv columns)
########################################################

#define the  data set of predictors (train_x, test_x)
#and the target vector (train_y, test_y)

train_x <- train_set[, -ncol(train_set)]
train_y <- train_set$SalePrice
test_x <- test_set[, -ncol(train_set)]
test_y <- test_set$SalePrice

set.seed(6, sample.kind = "Rounding")
# set train control parameters
control <- trainControl(method = "cv", number = 5, p = .9)

train_knn <- train(
  train_x,
  train_y,
  method = "knn",
  tuneGrid = data.frame(k = seq(1, 100, 5)),
  trControl = control
)

best_k <- train_knn$bestTune$k
best_k

predict_knn <- predict(train_knn, test_x)
rmse_knn <- RMSE(log(test_y), log(predict_knn))
rmse_knn
rmse_results <-   data_frame(method = "knn only ",
                             RMSE = rmse_knn)

#################################################
### KNN + removing nzv
#################################################

# remove the nzv columns from train_set and test_set
train_x0 <- train_set[, sel_col]
test_x0 <- test_set[, sel_col]

# define the new training and test sets of predictors
#by removing the target column (SalePrice)
train_x_nzv <- train_x0[, -ncol(train_x0)]
test_x_nzv <- test_x0[, -ncol(test_x0)]

set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 5, p = .9)

train_knn_nzv <- train(
  train_x_nzv,
  train_y,
  method = "knn",
  tuneGrid = data.frame(k = seq(1, 100, 5)),
  trControl = control
)

best_k <- train_knn_nzv$bestTune$k
best_k

predict_knn_nzv <- predict(train_knn_nzv, test_x_nzv)
rmse_knn_nzv <- RMSE(log(test_y), log(predict_knn_nzv))
rmse_knn_nzv
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "knn + nzv removed",
                                     RMSE = rmse_knn_nzv))

####################################################################
# lm model with all columns
####################################################################
set.seed(1, sample.kind = "Rounding")
train_lm <- train(train_x, train_y, method = 'lm')

train_lm

predict_lm <- predict(train_lm, test_x)
rmse_lm <- RMSE(log(test_y), log(predict_lm))
rmse_lm
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "lm only", RMSE = rmse_lm))


####################################################################
#  lm  + removing nzv columns
####################################################################
set.seed(1, sample.kind = "Rounding")
train_lm_nzv <-
  train(train_x_nzv, train_y, method = 'lm')

train_lm_nzv

predict_lm_nzv <-
  predict(train_lm_nzv, test_x_nzv)
rmse_lm_nzv <-
  RMSE(log(test_y), log(predict_lm_nzv))
rmse_lm_nzv
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "lm + nzv removed", RMSE = rmse_lm_nzv))

####################################################################
# Lasso Elastic-Net Linear Model (glmnet)
####################################################################

set.seed(1, sample.kind = "Rounding")
train_lmr <-
  train(train_x, train_y, method = "glmnet")
train_lmr

predict_lmr <- predict(train_lmr, test_x)
rmse_lmr <- RMSE(log(test_y), log(predict_lmr))
rmse_lmr
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "glmnet",
                                     RMSE = rmse_lmr))

#################################################################
# Lasso Elastic-Net Linear Model (glmnet) + removing nzv columns
################################################################
set.seed(1, sample.kind = "Rounding")
train_lmr_nzv <-
  train(train_x_nzv, train_y, method = "glmnet")
train_lmr_nzv

predict_lmr_nzv <-
  predict(train_lmr_nzv, test_x_nzv)
rmse_lmr_nzv <-
  RMSE(log(test_y), log(predict_lmr_nzv))
rmse_lmr_nzv
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "glmnet + nzv removed",
                                     RMSE = rmse_lmr_nzv))

############################################################
#Random Forest Model + best mtry
############################################################
library(randomForest)
control <- trainControl(method = "cv", number = 5)
grid <-
  data.frame(mtry = c(1, 5, 10, 25, 30, 35, 40, 45, 50, 55,  ncol(train_set) -
                        1))

set.seed(1, sample.kind = "Rounding") # simulate R 3.5

start_m <- proc.time()

# train mtry
train_rf <-  train(
  train_x,
  train_y,
  method = "rf",
  ntree = 150,
  trControl = control,
  tuneGrid = grid,
  nSamp = 500
)
stop_m <- proc.time() - start_m

plot(train_rf)
train_rf
best_mtry <- train_rf$bestTune$mtry

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
fit_rf <- randomForest(train_x, train_y,
                       #$minNode = train_rf$bestTune$mtry,
                       mtry = best_mtry,
                       #nodesize = best_nodesize,
                       importance = TRUE)

plot(fit_rf)

# Importance of features
varImp(fit_rf) %>% arrange(desc(Overall)) %>% top_n(10)

predict_rf <- predict(fit_rf, test_x)
rmse_rf <- RMSE(log(test_y), log(predict_rf))
rmse_rf
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "rf + best mtry",
                                     RMSE = rmse_rf))

############################################
# rf + best mtry + best nodesize
###########################################

nodesize <-
  seq(1, 40, 10) # I used seq(1, ncol(train_x), 10) originally to generate my report.
#But changed to seq(11, 40, 10) only for the purpose of reducing computing time for a grader

set.seed(2, sample.kind = "Rounding") # simulate R 3.5

start_n <- proc.time() # start a timer

rmses <- sapply(nodesize, function(ns) {
  train(
    train_x,
    train_y,
    method = "rf",
    tuneGrid = data.frame(mtry = best_mtry),
    nodesize = ns
  )$results$RMSE
})

stop_n <- proc.time() - start_n  # stop the timer

qplot(nodesize, rmses)

best_nodesize <- nodesize[which.min(rmses)]

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
fit_rf_node <- randomForest(
  train_x,
  train_y,
  mtry = best_mtry,
  nodesize = best_nodesize,
  importance = TRUE
)

# Importance of features
varImp(fit_rf_node) %>% arrange(desc(Overall)) %>% top_n(10)

plot(fit_rf_node)

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
predict_rf_node <- predict(fit_rf_node, test_x)
rmse_rf_node <-
  RMSE(log(test_y), log(predict_rf_node))
rmse_rf_node
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "rf + best mtry + best nodesize",
                                     RMSE = rmse_rf_node))

#############################################################
#Random Forest + best mtry + removing nzv
##############################################################

control <- trainControl(method = "cv", number = 5)
grid <-
  data.frame(mtry = c(1, 5, 10, 25, 30, 35, 40, 45, 50, 55,  ncol(train_set) -
                        1))
set.seed(5, sample.kind = "Rounding") # simulate R 3.5

#train mtry
train_rf_nzv <-  train(
  train_x_nzv,
  train_y,
  method = "rf",
  ntree = 150,
  trControl = control,
  tuneGrid = grid,
  nSamp = 500
)

plot(train_rf_nzv)
train_rf_nzv
best_mtry_nzv <- train_rf_nzv$bestTune$mtry

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
fit_rf_nzv <- randomForest(train_x_nzv, train_y,
                           mtry = best_mtry_nzv,
                           importance = TRUE)

# Importance of features
varImp(fit_rf_nzv) %>% arrange(desc(Overall)) %>% top_n(10)

plot(fit_rf_nzv)

predict_rf_nzv <- predict(fit_rf_nzv, test_x_nzv)
rmse_rf_nzv <-
  RMSE(log(test_y), log(predict_rf_nzv))
rmse_rf_nzv
rmse_results <- bind_rows(rmse_results,
                          data_frame(method = "rf + best mtry + nzv removed",
                                     RMSE = rmse_rf_nzv))

#############################################################
#Random Forest + best mtry + best nodesize + removing nzv
##############################################################
# train nodesize
nodesize <-
  seq(1, 40, 10) # I used seq(1, ncol(train_x), 10) originally to generate my report.
#But changed to seq(11, 40, 10) only for the purpose of reducing computing time for a grader

set.seed(2, sample.kind = "Rounding") # simulate R 3.5
rmses <- sapply(nodesize, function(ns) {
  train(
    train_x_nzv,
    train_y,
    method = "rf",
    tuneGrid = data.frame(mtry = best_mtry_nzv),
    nodesize = ns
  )$results$RMSE
})

qplot(nodesize, rmses)

best_nodesize_nzv <- nodesize[which.min(rmses)]

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
fit_rf_nzv_node <- randomForest(
  train_x_nzv,
  train_y,
  #$minNode = train_rf$bestTune$mtry,
  mtry = best_mtry_nzv,
  nodesize = best_nodesize_nzv,
  importance = TRUE
)

# Importance of features
varImp(fit_rf_nzv_node) %>% arrange(desc(Overall)) %>% top_n(10)


plot(fit_rf_nzv_node)

set.seed(3, sample.kind = "Rounding") # simulate R 3.5
predict_rf_nzv_node <-
  predict(fit_rf_nzv_node, test_x_nzv)
rmse_rf_nzv_node <-
  RMSE(log(test_y), log(predict_rf_nzv_node))
rmse_rf_nzv_node
rmse_results <- bind_rows(
  rmse_results,
  data_frame(method = "rf + nzv + best mtry + best nodesize",
             RMSE = rmse_rf_nzv_node)
)

##############################################################
#Ensemble Model: glmnet + rf + mtry + removing nzv columns
##############################################################

predict_ensemble <-
  (predict_lmr_nzv + predict_rf_nzv) / 2
rmse_ensemble <-
  RMSE(log(test_y), log(predict_ensemble))
rmse_ensemble
rmse_results <- bind_rows(
  rmse_results,
  data_frame(method = "ensemble: glmnet + rf + best mtry + nzv removed",
             RMSE = rmse_ensemble)
)

# Displa the results of all methods
rmse_results %>% knitr::kable()

stop_all <- proc.time() - start_all
stop_all

#Restore the original warning option
options(warn = defaultW)

#End of Code
