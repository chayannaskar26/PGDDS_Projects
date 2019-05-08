#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("doParallel")


library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(doParallel)

## Reading the train and test dataset
train <- read.csv("mnist_train.csv")
test <- read.csv("mnist_test.csv")

## Summary of dataset
summary(train)
summary(test)

## Checking for data issues

# NA in dataset

sum(is.na(train))
# 0

sum(is.na(test))
# 0

#############################
##  Changing column names  ##
#############################

# new column names will be "digit", "pixel0", "pixel1",.....,"pixel783"

last <- ncol(train)-2

names <- vector(mode="character", length=0)
names <- c(names, "digit")

for(i in 0:last){
  names <- c(names, paste("pixel", i, sep = ""))
}

colnames(train) <- names
colnames(test) <- names


## Converting digit column to factor
train$digit <- as.factor(train$digit)
test$digit <- as.factor(test$digit)


########################
##  Model Building    ##
########################

set.seed(100)
## Creating sample indices which is only 15% of the total train data
sample_indices <- sample(1:nrow(train), 0.15*nrow(train))

## Creating sample train and test dataset from the train dataset
sample_train <- train[sample_indices, ]
sample_test <- train[-sample_indices,]

## Parallel Processing for making training faster

cl <- makeCluster(detectCores())
registerDoParallel(cl)


########################
##    Linear Model    ##
########################

model_linear <- ksvm(digit ~ ., data = sample_train, scale = FALSE, kernel = "vanilladot")

## Predicting on sample test dataset
eval_linear <- as.factor(predict(model_linear, sample_test))

confusionMatrix(eval_linear, sample_test$digit)

## Accuracy : 0.911    Kappa : 0.9011   



## Predicting on actual test dataset
test_eval_linear <- as.factor(predict(model_linear, test))

confusionMatrix(test_eval_linear, test$digit)

## Accuracy : 0.9139    Kappa : 0.9043


# Taking C = 10

model_linear <- ksvm(digit ~ ., data = sample_train, C = 10, scale = FALSE, kernel = "vanilladot")

## Predicting on sample test dataset
eval_linear<- as.factor(predict(model_linear, sample_test))

confusionMatrix(eval_linear, sample_test$digit)

## Accuracy : 0.911   Kappa : 0.9011



## Predicting on actual test dataset
test_eval_linear <- as.factor(predict(model_linear, test))

confusionMatrix(test_eval_linear, test$digit)

## Accuracy : 0.9139    Kappa : 0.9043



# Cross Validation for Linear SVM model

trainControl <- trainControl(method="cv", number=5)

svm_linear_1 <- train(digit~., data = sample_train, method = "svmLinear", trControl = trainControl)

svm_linear_1
svm_linear_1

##  Accuracy   Kappa     C 
##  0.9084368  0.898216  1



## Testing SVM Linear model on sample test dataset
svm_linear_sample_eval <- as.factor(predict(svm_linear_1, sample_test))

confusionMatrix(svm_linear_sample_eval, sample_test$digit)

## Accuracy : 0.911   Kappa : 0.9011


## Testing SVM Linear model on actual test dataset
svm_linear_test_eval <- as.factor(predict(svm_linear_1, test))

confusionMatrix(svm_linear_test_eval, test$digit)

## Accuracy : 0.9139    Kappa : 0.9043



## Using tuneLength of 5
svm_linear_2 <- train(digit~., data = sample_train, method = "svmLinear", trControl = trainControl, tuneLenght = 5)

svm_linear_2

##  Accuracy   Kappa      C
##  0.9092127  0.8990812  1



## Testing SVM Linear model on sample test dataset
svm_linear_sample_eval <- as.factor(predict(svm_linear_2, sample_test))

confusionMatrix(svm_linear_sample_eval, sample_test$digit)

## Accuracy : 0.911 Kappa : 0.9011


## Testing SVM Linear model on actual test dataset
svm_linear_test_eval <- as.factor(predict(svm_linear_2, test))

confusionMatrix(svm_linear_test_eval, test$digit)

## Accuracy : 0.9139  Kappa : 0.9043



####################
##    RBF Model   ##
####################

model_rbf <- ksvm(digit ~ ., data = sample_train, scale = FALSE, kernel = "rbfdot")


## Predicting on sample test dataset
eval_rbf <- as.factor(predict(model_rbf, sample_test))

confusionMatrix(eval_rbf, sample_test$digit)

##  Accuracy : 0.9556   Kappa : 0.9506

## Predicting in actual test dataset

test_eval_rbf <- as.factor(predict(model_rbf, test))

confusionMatrix(test_eval_rbf, test$digit)

##  Accuracy : 0.9577   Kappa : 0.953



# Cross Validation for Radial SVM model
svm_radial_1 <- train(digit~., data = sample_train, method = "svmRadial", trControl = trainControl)

svm_radial_1
svm_radial_1$bestTune


###############################
# C     Accuracy   Kappa      #
# 0.25  0.9325475  0.9250241  #
# 0.50  0.9433265  0.9370045  #
# 1.00  0.9505505  0.9450345  #
#=============================#
# sigma 1.62714e-07    C  1  #
###############################



## Testing on sample test dataset

svm_radial_sample_eval <- as.factor(predict(svm_radial_1, sample_test))

confusionMatrix(svm_radial_sample_eval, sample_test$digit)

##  Accuracy : 0.9555   Kappa : 0.9506

## Testing on actual test dataset

svm_radial_test_eval <- as.factor(predict(svm_radial_1, test))

confusionMatrix(svm_radial_test_eval, test$digit)

##  Accuracy : 0.9575   Kappa : 0.9528 



# Setting Tune Length of 4
svm_radial_2 <- train(digit~., data = sample_train, method = "svmRadial", trControl = trainControl, tuneLength = 4)

svm_radial_2
svm_radial_2$bestTune

###############################
# C     Accuracy   Kappa      # 
# 0.25  0.9302475  0.9224661  #
# 0.50  0.9395574  0.9328147  #
# 1.00  0.9483108  0.9425444  #
# 2.00  0.9562307  0.9513483  #
#=============================#
# sigma 1.627773e-07  C  2    #
###############################



## Testing on sample test dataset

svm_radial_sample_eval <- as.factor(predict(svm_radial_2, sample_test))

confusionMatrix(svm_radial_sample_eval, sample_test$digit)

##  Accuracy : 0.9618   Kappa : 0.9576

## Testing on actual test dataset

svm_radial_test_eval <- as.factor(predict(svm_radial_2, test))

confusionMatrix(svm_radial_test_eval, test$digit)

##  Accuracy : 0.9628   Kappa : 0.9586



# Tune Grid
grid <- expand.grid(.sigma = 1.627773e-07, .C = seq(1, 5, by = 1))

svm_radial_3 <- train(digit~., data = sample_train, method = "svmRadial", trControl = trainControl, tuneGrid = grid)

svm_radial_3
svm_radial_3$bestTune

############################
#  C  Accuracy   Kappa     #
#  1  0.9488826  0.9431803 #
#  2  0.9573288  0.9525693 #
#  3  0.9588839  0.9542970 #
#  4  0.9602728  0.9558408 #
#  5  0.9618009  0.9575396 #
#==========================#
# sigma 1.627773e-07  C  5 #
############################



## Testing on sample test dataset

svm_radial_sample_eval <- as.factor(predict(svm_radial_3, sample_test))

confusionMatrix(svm_radial_sample_eval, sample_test$digit)

## Accuracy : 0.9649   Kappa : 0.961

## Testing on actual test dataset

svm_radial_test_eval <- as.factor(predict(svm_radial_3, test))

confusionMatrix(svm_radial_test_eval, test$digit)

##  Accuracy : 0.965  Kappa : 0.9611


stopCluster(cl)
