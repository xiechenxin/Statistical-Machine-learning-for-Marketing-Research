library(neuralnet)
library(mlr)

setwd("C:/Users/cxie/Desktop/statistical")
getwd()

def <- read.csv('default.csv', sep=';')

#-------------Q1: Randomly divide data into train/test as 80/20 ( set.seed = 1) --------------------------------------#
set.seed(1)
train_idx <- sample(1:nrow(def), round(nrow(def)*0.8))
train <- def[train_idx, ]
test <- def[-train_idx, ]
dim(train) # 24000    25
dim(test) # 6000   25

#------------Q2: Build a NN model with 1 hidden layer of 30 neurons, sigmoid activation function. --------------------#

# Create the formula to train Neural Network
formula <- as.formula(paste0('Y ~ ', paste(names(def)[1:(ncol(def)-2)], collapse=' + ')))

# Fit the Neural Network model
def_nn <- neuralnet(formula,
                     train,
                     hidden=30,       
                     rep=1,                
                     lifesign='full',       
                     algorithm='backprop', 
                     learningrate=0.01,      
                     act.fct='tanh', 
                     linear.output=F
)

# Make prediction and evaluation on train
y_train_pred <- predict(def_nn, train)
mean((max.col(y_train_pred)-1) == train$label)

# Make prediction and evaluation on test
y_test_pred <- predict(def_nn, test)
mean((max.col(y_test_pred)-1) == test$label)


#----------Q3: Build a deep NN model with multiple hidden layers (of your choice) and sigmoid activation function.----#
def_dnn <- neuralnet(formula,
                    train,
                    hidden=c(30, 30),       
                    rep=1,                
                    lifesign='full',       
                    algorithm='backprop', 
                    learningrate=0.01,      
                    act.fct='tanh', 
                    linear.output=F
)

# Make prediction and evaluation on train
y_train_pred <- predict(def_dnn, train)
mean((max.col(y_train_pred)-1) == train$label)

# Make prediction and evaluation on test
y_test_pred <- predict(def_dnn, test)
mean((max.col(y_test_pred)-1) == test$label)


#----------Q4: Build 5 other classification models and compare with the 2 previous NN models.-------------------------#

# Define the ML classification task
train_task <- mlr::makeClassifTask(id ='class_def', data=train, target='Y')
test_task <- mlr::makeClassifTask(id='class_def', data=test, target='Y')

# Decision Tree
learner <- mlr::makeLearner('classif.rpart')  # Register a machine learning model
model <- mlr::train(learner, train_task)
pred_test <- predict(model, task=test_task)
performance(pred_test, measures=acc)
#       acc 
# 0.8148333  

# Random Forest
learner <- makeLearner('classif.randomForest')
model <- mlr::train(learner, train_task)
pred_test <- predict(model, task=test_task)
performance(pred_test, measures=acc)
#   acc 
# 0.818 

# k-Nearest Neighbor (k=50)
learner <- makeLearner('classif.knn', k=50)
model <- mlr::train(learner, train_task)
pred_test <- predict(model, task=test_task)
performance(pred_test, measures=acc)
#       acc 
# 0.7728333 

# Logistic Regression Lasso (l1)
learner <- mlr::makeLearner('classif.LiblineaRL1LogReg') 
model <- mlr::train(learner, train_task)
pred_test <- predict(model, task=test_task)
performance(pred_test, measures=acc)
#       acc 
# 0.7728333 




