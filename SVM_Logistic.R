
# Refrences
Code for Documentation
#Himanshu Sharma
#part  1


rm(list=ls())

library(rio, moments)
library(corrplot)
library(lpSolve)

set.seed(8803)


P  = round(10*runif(4,0,1)) #demand uncertainty perturbation
D0 = c(100,190,210,160) # average prediction
D_BC = D0+P #best case scenario
D_WC= D0-P #worst case scenario

#Worst Case Scenario for 1st demand uncertainty perturbation
costs <- matrix(c(6, 6.1, 6.2, 6.3, 0,
                  9, 9.1, 9.2, 9.3, 0,
                  100, 6, 6.1, 6.2, 0,
                  100, 9, 9.1, 9.2, 0,
                  100, 100, 6, 6.1, 0,
                  100, 100, 9, 9.1, 0,
                  100, 100, 100, 6, 0,
                  100, 100, 100, 9, 0), nrow = 8, byrow = TRUE)
colnames(costs) <- c("Spring", "Summer", "Fall", "Winter","Surplus")
rownames(costs) <- c("R1", "O1", "R2", "O2", "R3", "03", "R4", "04")

# Set unequality/equality signs for production types
row.signs <- rep("<=", 8)
                 
# Set right hand side coefficients for production types
row.rhs <- c(90, 50, 100, 60, 120, 80, 110, 70)
                 
# Set unequality/equality signs for demand
col.signs <- rep(">=", 5)
                 
# Set right hand side coefficients for demand
col.worst <- append(D_WC,44)
                 
# Final value (z)
first.worst<-lp.transport(costs, "min", row.signs, row.rhs, col.signs, col.worst)
                 
# Variables final values
first.worst
first.worst$solution

#Best Case Scenario for 1st demand uncertainty perturbation                 
costs1 <- matrix(c(6, 6.1, 6.2, 6.3, 
                   9, 9.1, 9.2, 9.3, 
                   100, 6, 6.1, 6.2, 
                   100, 9, 9.1, 9.2, 
                   100, 100, 6, 6.1, 
                   100, 100, 9, 9.1, 
                   100, 100, 100, 6, 
                   100, 100, 100, 9,
                   100,100, 100, 100), nrow = 9, byrow = TRUE)
colnames(costs1) <- c("Spring", "Summer", "Fall", "Winter")
rownames(costs1) <- c("R1", "O1", "R2", "O2", "R3", "03", "R4", "04","Dummy")
                 
# Set unequality/equality signs for production types
row.signs1 <- rep("<=", 9)
                 
# Set right hand side coefficients for production types
row.rhs1 <- c(90, 50, 100, 60, 120, 80, 110, 70,29)
                 
# Set unequality/equality signs for demand
col.signs1 <- rep(">=", 4)
                 
# Set right hand side coefficients for demand
col.best <- c(103,199,216,173)
                 
                 
# Final value (z)
first.best<-lp.transport(costs1, "min", row.signs1, row.rhs1, col.signs1, col.best)
                 
# Variables final values
first.best
first.best$solution

P1 = round(10*runif(4,0,1)) #demand uncertainty perturbation
D0 = c(100,190,210,160) # average prediction
D_BC1 = D0+P1 #best case scenario
D_WC1= round(D0-P1) #worst case scenario


#Worst Case Scenario for 2nd demand uncertainty perturbation
# Set right hand side coefficients for demand
col.worst1 <- append(D_WC1,31)

# Final value (z)
first.worst1<-lp.transport(costs, "min", row.signs, row.rhs, col.signs, col.worst1)

# Variables final values
first.worst1
first.worst1$solution

#Best Case Scenario for 2nd demand uncertainty perturbation

# Set right hand side coefficients for production types
row.rhs2 <- c(90, 50, 100, 60, 120, 80, 110, 70,9)

# Set right hand side coefficients for demand
col.best1 <- c(103,192,210,166)


# Final value (z)
first.best1<-lp.transport(costs1, "min", row.signs1, row.rhs2, col.signs1, col.best1)

# Variables final values
first.best1
first.best1$solution

#Part 2 :
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
