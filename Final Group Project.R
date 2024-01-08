# load libraries:
library(caret)
library(tidyverse)
library(stargazer)
library(ggplot2)
library(rsample)
library(leaps)
library(dplyr)
library(nnet)
library(rpart)
library(rpart.plot)
library(ranger)
library(caTools)
library(MASS)
library(glmnet)
library(xgboost)
library(naivebayes)
library(mclust)
library(factoextra)
library(keras)

#scree plot and PCA, PCR intuition , loadings, SHAP value
# https://github.com/slundberg/shap
# https://cran.r-project.org/web/packages/shapr/vignettes/understanding_shapr.html

# load datasets:
smsmalware <- read.csv('SMS Malware.csv')
scareware <- read.csv('Scareware Malware.csv')
ransomware <- read.csv('Ransomware Malware.csv')
adware <- read.csv('Adware Malware.csv')
Benign <- read.csv('Benign.csv')
malware <- smsmalware %>% full_join(scareware) %>% full_join(ransomware) %>% full_join(adware) %>% full_join(Benign)

# change the Label column values into categorical values
malware$Label <- factor(malware$Label)

# remove columns that are mean, standard deviation, max, min of other columns, and those with only zero values.
malware36 <- malware %>% dplyr::select(-c(1,2,3,4,5,6,7,13,14,15,16,17,18,19,20,24,25,26,28,29,30,31,33,34,35,36,38,39,40,45,46,48,49,52,56,57,62,63,64,65,66,67,68,78,79,80,82,83,84)) 
summary(malware36)

# split the dataset into training and test sets.
set.seed(1)
malware36_split <- sample.split(malware36, SplitRatio = 0.7)
training_set = subset(malware36, malware36_split == TRUE)
test_set = subset(malware36, malware36_split == FALSE)

################################################################################
# knn model - Roy
knnModel <- train(
  Label ~ .,
  data = training_set,
  method = 'knn',
  preProcess = c('center','scale'),
  trControl = trainControl(method = 'cv', number = 10)
)
knnModel
knnPrediction <- predict(knnModel, test_set)
confusionMatrix(knnPrediction, test_set$Label)
#Accuracy : 0.5357

################################################################################
# logistic regression model and AIC test - Haixu Zhao
logRegModel <- multinom(Label ~ ., data = training_set)
logRegPred <- predict(logRegModel, test_set)
confusionMatrix(logRegPred, test_set$Label)
# Accuracy : 0.3074

# AIC test
step <- stepAIC(logRegModel, direction = 'both', trace = FALSE)
step$anova

# remove the columns that AIC suggested
malware33 <- training_set %>% dplyr::select(-c(9,12,32))
summary(malware33)

logRegModified <- multinom(Label ~ ., data = malware33)
logRegModPred <- predict(logRegModified, test_set)
confusionMatrix(logRegModPred, test_set$Label)
# Accuracy : 0.3285

# search for the best lambda parameter on a grid
tune_grid <- expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 100))

# create a lasso logistic regression
lassoRegModel <- train(
  Label ~.,
  data = malware33,
  method = 'glmnet',
  family = 'multinom',
  tuneGrid = tune_grid,
  preProcess = c('zv','center','scale'),
  trControl = trainControl(method = 'cv', number = 10),
  tuneLength = NULL
)
lassoRegModel
# lambda: 0.0001 
# Accuracy: 0.3546

################################################################################
# classification tree model and important variables test - Bhavya
classTreeModel <- rpart(Label ~ ., data = training_set, method = 'class')
rpart.plot(classTreeModel)
classTreePred <- predict(classTreeModel, test_set, type = 'class')
confusionMatrix(classTreePred, test_set$Label)
# Accuracy : 0.354

# select important variables in the classification tree model
importantVars <- caret::varImp(classTreeModel)
importantVars

# keep only the columns that varImp function suggested
malware13 <- training_set %>% dplyr::select(c('act_data_pkt_fwd','Average.Packet.Size','Avg.Fwd.Segment.Size',
                             'Flow.Duration','Flow.IAT.Mean','Flow.Packets.s','Fwd.IAT.Total',
                             'Fwd.Packets.s','Init_Win_bytes_backward','min_seg_size_forward',
                             'Subflow.Fwd.Bytes','Total.Length.of.Fwd.Packets','Label'))
classTreeModified <- rpart(Label ~ ., data = malware13, method = 'class')
rpart.plot(classTreeModified)
classTreeModPred <- predict(classTreeModified, test_set, type = 'class')
confusionMatrix(classTreePred, test_set$Label)
# Accuracy : 0.354

# Naive Bayes model - Bhavya
nb_model <- naive_bayes(Label ~ ., data = training_set, method = "prob")
nb_model
summary(nb_model)

nb_pred <- predict(nb_model, test_set, type = 'class')
confusionMatrix(nb_pred, test_set$Label)
# Accuracy : 0.2373

# XGBoost model - Bhavya







################################################################################
# random forest model - Athuliya
randForestModel <- train(
  Label ~ .,
  data = training_set,
  method = 'ranger',
  trControl = trainControl(method = 'cv', number = 10),
  num.trees = 200
)
randForestModel
randForestPred <- predict(randForestModel, test_set)
confusionMatrix(randForestPred, test_set$Label)
# Accuracy : 0.6138

################################################################################
# pca model - Adityaketu
pcaModel <- training_set %>% dplyr::select(-Label) %>% prcomp(center = TRUE, scale. = TRUE)
summary(pcaModel)

# scree plot of the pca model
fviz_screeplot(pcaModel, addlabels = TRUE, choice = 'variance')

# if set the percentage of explained variance to 90%, then we should use PC1 to PC14.
datawithpca <- cbind(training_set, pcaModel$x)
logRegPCAModel <- multinom(Label ~ pcaModel$x[,1:14], data = datawithpca)
logRegPCAPred <- predict(logRegPCAModel, training_set)
confusionMatrix(logRegPCAPred, training_set$Label)
# Accuracy : 0.338

# neural network - Adityaketu

















