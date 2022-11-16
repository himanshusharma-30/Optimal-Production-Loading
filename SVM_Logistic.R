#Himanshu
rm(list=ls())

library(kernlab) # for the spam data
library(caret)
library(caTools)
library(e1071)
library("rio")
library("moments")

# Load the input data to be used
set.seed(2048)
data(spam)

# make sure response variable is binary. 
#Turning nonSpam and Spam to Binary
spam$type <- ifelse(spam$type == "spam", 1, 0)
str(spam)
# make sure categorical variables are factors
spam$type = as.factor(spam$type)
str(spam)

# sampling a training data  1000 emails 
sample_data<- spam[sample(nrow(spam),1000), ]

split<- sample.split(sample_data$type, SplitRatio = 0.7)

Train<- subset(sample_data, split == TRUE)
Test<- subset(sample_data, split  == FALSE)


# logistic model
fit <- glm(type ~ ., data = Train, family = "binomial")
summary(fit)

#use model to predict value mod.pred1
##Creating the confusion matrix:
Type_pred <- predict(fit, newdata=Test, type="response")
Type_pred <- ifelse(Type_pred  > 0.5,"Yes", "No")
confusionmatrix <- table(Test$type,Type_pred)
confusionmatrix
accuracy<- sum(diag(confusionmatrix))/sum(confusionmatrix)# 0.8866
accuracy

#high TPR and low FPR


#SVM parameter for Training 
#finding Optimal Values 
sigDist_train <- sigest(type~.,data = Train, frac = 1 )


# creating Grid for 2 tuning parameter(C is the mis clasification)
SVM_grid <- data.frame(.sigma = sigDist_train[1],.C = 2^(-2:7))

#Train SMV 
SVM_train<-train(type~.,data = Train, method = "svmRadial",
                 preProc = c("center","scale"),
                 tuneGrid = SVM_grid,
                 trControl = trainControl(method="repeatedcv",repeats =5,
                                          classProbs = FALSE))

#predicting
predict_svm <- predict(SVM_train, Test)

#matrix 
SVM_matrix<- confusionMatrix(predict_svm,Test$type)
SVM_matrix
