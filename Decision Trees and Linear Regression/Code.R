# Name: Aditya Pandey
# Student Id.: 1001405034
# Assignment No.: 6


data(package = .packages(all.available = TRUE)) #To check all the inbuilt datasets
myDataSet <- read.table(file.choose(""), header = F, sep = "  ") #To choose a dataset from file
myDataSet <- titanic.raw # Copy the titanic dataset to the environment variable myDataSet

attach(myDataSet)
class(myDataSet)
summary(myDataSet)

#Part 1 Of the Assignment
regressionModel <- lm(V2~V4,myDataSet)    #Constructs a linear regression model for the attributes Sex(V2) and Survived(V4)

distribution <- sample(2,nrow(myDataSet),replace = TRUE, prob = c(0.7,0.3) )    #Distributes the original dataset into two partitions
                                                                                #Of 70% as Training dataset and 30% as testing dataset

head(trainData)   #Display first 5 tuples of trainData
head(testData)    #Display first 5 tuples of testData

trainRegressionModel <- lm(V2~V4,trainData)   #Constructs a linear regression model of the Training dataset for the attributes Sex(V2) and Survived(V4)
plot(trainRegressionModel)                    #Plots the linear regression model of the Training dataset
summary(trainRegressionModel)                 #Gives a summary of the linear regression model
abline(trainRegressionModel,v2)

testRegressionModel <- lm(V2~V4,testData)     #Constructs a linear regression model of the Testing dataset for the attributes Sex(V2) and Survived(V4)
plot(testRegressionModel)                     #Plots the linear regression model of the Testing dataset
summary(testRegressionModel)                  #Gives a summary of the linear regression model
abline(testRegressionModel,v2)

anova(trainRegressionModel,testRegressionModel)     #This will compare the two regression model

trainDataSetPrediction <- predict(regressionModel,trainData)  #It predicts the original data with the training data value
summary(trainDataSetPrediction)                               #It gives the summary of the prediction


testDataSetPrediction <- predict(regressionModel,testData)    #It predicts the original data with the training data value
summary(testDataSetPrediction)                                #It gives the summary of the prediction



#Part 2 Of the Assignment
library(rpart)    #Imports 'rpart' library
library(rpart.plot) #Imports 'rpart.plot' library


#Decision tree on Original data
originalDecisionTreeModel <- rpart(Survived~.,myDataSet,method = "class") #Stores the decision tree model in the variable for the original data
                                                                          #Here the method is set to class(Classification for classifier)
summary(decisionTreeModel)      #Gives the summary of the dicision tree model
rpart.plot(decisionTreeModel)   #Plots the dicision tree model
rpart.plot(decisionTreeModel, type = 4, extra = 101)  #Plots the dicision tree model with a better classification property


#Decision tree on Training data
trainingDecisionTreeModel <- rpart(Survived~.,trainData,method = "class") #Stores the decision tree model in the variable for the Testing data
                                                                          #Here the method is set to class(Classification for classifier)
summary(trainingDecisionTreeModel)#Gives the summary of the dicision tree model
rpart.plot(trainingDecisionTreeModel)#Plots the dicision tree model
rpart.plot(trainingDecisionTreeModel, type = 4, extra = 101)#Plots the dicision tree model with a better classification property


#Decision tree on Testing data
testingDecisionTreeModel <- rpart(Survived~.,testData,method = "class")   #Stores the decision tree model in the variable for the Training data
                                                                          #Here the method is set to class(Classification for classifier)
summary(testingDecisionTreeModel)#Gives the summary of the dicision tree model
rpart.plot(testingDecisionTreeModel)#Plots the dicision tree model
rpart.plot(testingDecisionTreeModel, type = 4, extra = 101)#Plots the dicision tree model with a better classification property

