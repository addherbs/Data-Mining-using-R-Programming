# Name: Aditya Pandey
# Student Id.: 1001405034
# Assignment No.: Advanced Assignment

							#Classifier : K-NN
#######################################################################################################


myDataSet <- read.table(file.choose(""), header = T, sep = "  ") #To choose a dataset from file


#myDataSet <- read.table(file.choose(""), header = F, sep = "  ")

myDataSet<- Fertility_Data_Set

head(myDataSet)   #Display first 5 tuples of myDataSet

library(e1071)									#imports e1071 library


attach(myDataSet)
class(myDataSet)
summary(myDataSet)

View(myDataSet)
table(myDataSet)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }				#This is a fucntion, which normalizes the input value
#between 0 and 1. Then returns it to the callee

myDataSet_n <- as.data.frame(lapply(myDataSet[1:9], normalize))		#This will create a table with the normalized value 
#from the attributes 1 to 9, Since 10 is a string and the deciding factor

summary(myDataSet_n)


#distribution <- sample(2,nrow(myDataSet1_n),replace = TRUE, prob = c(0.80,0.20) )    #Distributes the original dataset into two partitions
#Of 70% as Training dataset and 30% as testing dataset



trainData <- myDataSet_n[1:81,]		#Train the data on 1-81 rows
testData <- myDataSet_n[82:100,]	#Test data will hold the remaining rows, 19




new_train_labels <-  myDataSet[1:81,1] 				#We create a training class lable, which will be used
# as a class argument in the knn function

new_test_labels <-  myDataSet[82:100,1] 			#We create a testing class lable, which will be used
# as a class argument in the knn function

require(class)										#intializes the inbuilt class, class



this_test_pred <- knn(trainData,  testData , new_train_labels ,10)			#It does the prediction, has 4 arguments. 1st = training data, 
#2md testing data, 3rd class, 4th = number of neighbours considered, which is 10 in this case.

this_test_pred1 <- knn(trainData,  testData , new_train_labels ,9)			#It does the prediction, has 4 arguments. 1st = training data, 
#2md testing data, 3rd class, 4th = number of neighbours considered, which is 9 in this case.


this_test_pred2 <- knn(trainData,  testData , new_train_labels ,11)			#It does the prediction, has 4 arguments. 1st = training data, 
#2md testing data, 3rd class, 4th = number of neighbours considered, which is 11 in this case.


table(this_test_pred)							#creates a table for partition number 1

table(this_test_pred1)							#creates a table for partition number 2

table(this_test_pred2)							#creates a table for partition number 3

plot(this_test_pred)

plot(this_test_pred1)

plot(this_test_pred2)

plot(testData)

plot(trainData)

plot(myDataSet_n)

plot(myDataSet)



								#Classifier : SVM
#######################################################################################################


myDataSet<- Fertility_Data_Set
attach(myDataSet)
class(myDataSet)
summary(myDataSet)

x <- subset(myDataSet, select=-V10)		#create a subset of the data without the 10th attribute in it
y <- V10								#it holds the 10th attribute data

library(e1071)									#imports e1071 library

svm_model <- svm(V10 ~ ., data=myDataSet)		#calls the svm fucntion, 1st is the decision attribute argument and 2nd is the dataset

summary(svm_model)								#Summary of the enntire dataset




svm_model1 <- svm(x,y)				#calls the svm fucntion, 1st is the decision attribute argument and 2nd is the dataset
summary(svm_model1)					#Summary of the enntire dataset


pred <- predict(svm_model1,x)		#predicts the relation between the svm model created earlier and the subset data
system.time(pred <- predict(svm_model1,x))		#calculates the time that it takes to generate the above predicted output

plot(pred)

table(pred,y)		#displays the output in a confusion matrix

svm_tune <- tune(svm, train.x=x, train.y=y, 
                 kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))		#This is the tuning algorithm, which will boost up the algorithm


print(svm_tune)		


svm_model_after_tune <- svm(V10 ~ ., data=myDataSet, kernel="radial", cost=1, gamma=0.5)		#now we run the earlier code with the tuned values
summary(svm_model_after_tune)		#we calculate the tuned output that we got earlier


pred <- predict(svm_model_after_tune,x)		#we predict the tuned version model again with the subset data
system.time(predict(svm_model_after_tune,x))		#calculates the time that the tuned prediction took

table(pred,y)			#we draw the confusion matrix again for the tuned output


plot(pred)



						#Classifier : Naive Bayes
#######################################################################################################


myDataSet <- read.table(file.choose(""), header = T, sep = "  ") #To choose a dataset from file



myDataSet<- Fertility_Data_Set
attach(myDataSet)
class(myDataSet)
summary(myDataSet)

View(myDataSet)
table(myDataSet$V10)

distribution <- sample(2,nrow(myDataSet),replace = TRUE, prob = c(0.75,0.75) )    #Distributes the original dataset into two partitions
																				#Of 0.75 as Training dataset and 0.75 as testing dataset

trainData <- myDataSet[distribution==1,]
testData <- myDataSet[distribution==2,]

head(trainData)   #Display first 5 tuples of trainData
head(testData)    #Display first 5 tuples of testData

nrow(trainData)
nrow(testData)

library(e1071)
library(rminer)

trainModel <- naiveBayes( V10 ~ .,data = trainData)

trainModel

predictionModel <- predict(trainModel, testData)

mmetric(testData$V10, predictionModel, c("ACC","PRECISION","TPR","F1"))

outputmetric <- mmetric(testData$V10, predictionModel, c("ACC","PRECISION","TPR","F1"))



plot(trainData)
plot(testData)
plot(predictionModel)
plot(outputmetric)


								