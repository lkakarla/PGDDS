############################ SVM - Handwritten Digit Recognition ############################
#-------------------------------------------------------------------------------
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5  Hyperparameter tuning and cross validation
# 6. Conclusion

#######################################################################################
#1. Business Understanding: 
#######################################################################################

#The objective is to identify the handwritten digit(between 0-9) based on pixel values of each digit


#######################################################################################
#2. Data Understanding
#######################################################################################

# Train Data Set -- mnist_train.csv
# Number of Instances : 60000
# Number of Attributes: 785 (784 continuous, 1 nominal class label)

# Test Data Set -- mnist_test.csv
# Number of Instances : 10000
# Number of Attributes: 785 (784 continuous, 1 nominal class label)


#######################################################################################
#3. Data Preparation:
#######################################################################################

#load required packages
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(caTools)
#library(e1071)


# load the data
mnist_train <- read_csv("mnist_train.csv",col_names = FALSE)
mnist_test <- read_csv("mnist_test.csv", col_names= FALSE)

# data sampling
# considered 10% of data from given train and test data sets to make the computation faster
set.seed(100)
train.indices = sample(1:nrow(mnist_train), 0.1*nrow(mnist_train))
train = mnist_train[train.indices, ]

test.indices = sample(1:nrow(mnist_test), 0.1*nrow(mnist_test))
test = mnist_test[test.indices, ]


#Understanding Dimensions
dim(train)
dim(test)

#Structure of the dataset
str(train)
str(test)

#printing first few rows
head(train[1:10])
head(test[1:10])

#Exploring the data
summary(train)
summary(test)

#checking missing values
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))

sum(is.na(train)) # zero missing values
sum(is.na(test)) # zero missing values

#Normalize the data
train <- cbind(train[,1],train[,-1]/255)
test <- cbind(test[,1],test[,-1]/255)

#Rename first column name
colnames(train)[1] <- 'digit'
colnames(test)[1] <- 'digit'

#Making our target class to factor
train$digit <- factor(train$digit) # convert digit labels to factor for classification
test$digit <- factor(test$digit) # convert digit labels to factor for classification

summary(train$digit)
summary(test$digit)


#######################################################################################
#4. Model Building:
#######################################################################################

#----------------------------------
#Linear Kernel
#----------------------------------

#Using Linear Kernel
Model_linear <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot")

# Predicting the model results 
Eval_linear <- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)
# Accuracy    : 0.92 

#----------------------------------
#Polynomial Kernel
#----------------------------------

#Using Polynomial Kernel
Model_poly <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot", kpar=list(degree=2))

# Predicting the model results 
Eval_poly <- predict(Model_poly, test)

#confusion matrix - Polynomial Kernel
confusionMatrix(Eval_poly,test$digit)
# Accuracy    : 0.953


#----------------------------------
#RBF Kernel
#----------------------------------

#Using RBF Kernel
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")

# Predicting the model results 
Eval_RBF <- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)
# Accuracy    : 0.955


#RBF kernel accuracy is better than Polynomial Kernel and Linear Kernel.

#######################################################################################
#5. Hyperparameter tuning and Cross Validation
#######################################################################################

# We will use the train function from caret package to perform Cross Validation. 

# traincontrol function Controls the computational nuances of the train function.
# i.e. method = CV means  Cross Validation.
#      Number = 5 implies Number of folds in CV.

trainControl <- trainControl(method="cv",
                             number=5, 
                             verboseIter=TRUE)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

# Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# Making grid of "sigma" and C values.

set.seed(100)
gridRbf <- expand.grid(.sigma=c(0.015 ,0.025, 0.05), .C=c(0.1,0.5,1,2,3) )

# train function takes Target ~ Prediction, Data, Method = Algorithm, Metric = Type of metric, tuneGrid = Grid of Parameters, trcontrol = Our traincontrol method.

# Performing 5-fold cross validation
fit.svmRbf <- train(digit~., data=train, method="svmRadial", metric=metric, 
                    tuneGrid=gridRbf, trControl=trainControl)

# Printing cross validation result
print(fit.svmRbf)
# Best tune at sigma = 0.025 & C = 2, Accuracy - 0.962

# Plotting model results
plot(fit.svmRbf)

#----------------------------------
# Checking overfitting - RBF Kernel
#----------------------------------

# Validating the model results on test data
evaluate_Rbf <- predict(fit.svmRbf, test)

confusionMatrix(evaluate_Rbf, test$digit)

# Accuracy    - 0.962

######################################################################
#6. Conclusion
######################################################################
# Finally SVM model with RBF Kernel (with sigma(gamma) = 0.025 and Cost(c) = 2) 
# can correctly identify the handwritten digit in provided dataset with 28*28 pixel features.

# Constructing final model with sigma(gamma) = 0.025 and Cost(c) = 2
Final_svm <- ksvm(digit~ ., data = train, scale = FALSE, gamma=0.025,cost=2 ,kernel = "rbfdot")

Eval_Final_svm <- predict(Final_svm, test)

confusionMatrix(Eval_Final_svm,test$digit)

# Since sigma and C values are very low, it indicates that, 
# the complexity and nonlinearity of the model is less, and the model is also not overfitting.


