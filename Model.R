
# Importing Required Libraries:
#------------------------------

library(rvest) 
library(dplyr)
library(utils)
library(caret)
library(stats)
library(stringr)
library(lubridate)
library(caTools)
library(e1071)
library(ggplot2)
library(rpart)
library(rpart.plot)

###############################################################################################################################

# Obtaining dataset from CSV file uploaded in GITHUB:
#----------------------------------------------------

data <- read.csv("https://raw.githubusercontent.com/Shawn191/Datasets/main/Tunisia.csv", header = T)#tunisia 
data1 <- read.csv("https://raw.githubusercontent.com/Shawn191/Datasets/main/Gabon.csv", header = T)#gabon

###############################################################################################################################

# Pre-processing:
#---------------

# Removing meaningless, NA and duplicate columns:
data <- data[c(1,3:17)]
data1 <- data1[c(1,3:17)]

# Labeling Age Column:
#---------------------
for (i in 1:nrow(data)) {
  if(data$Age[i]>=1 & data$Age[i]<18){
    data$Age[i]="Below 18"
  }
  if(data$Age[i]>=15 & data$Age[i]<24){
    data$Age[i]="Youth"
  }
  if(data$Age[i]>=24 & data$Age[i]<60){
    data$Age[i]="Adult"
  }
  if(data$Age[i]>=60 & data$Age[i]<150){
    data$Age[i]="Elderly"
  }
}
for (i in 1:nrow(data1)) {
  if(data1$Age[i]>=0 & data1$Age[i]<18){
    data1$Age[i]="Below 18"
  }
  if(data1$Age[i]>=15 & data1$Age[i]<24){
    data1$Age[i]="Youth"
  }
  if(data1$Age[i]>=24 & data1$Age[i]<60){
    data1$Age[i]="Adult"
  }
  if(data1$Age[i]>=60 & data1$Age[i]<150){
    data1$Age[i]="Elderly"
  }
}
# Labelling Participants(Gender) Column:
#---------------------------------------
for (i in 1:nrow(data)) {
  if(data$Participants[i]=='h'){
    data$Participants[i]='M'
  }
  else{
    data$Participants[i]='F'
  }
}
for (i in 1:nrow(data1)) {
  if(data1$Participants[i]=='h'){
    data1$Participants[i]='M'
  }
  else{
    data1$Participants[i]='F'
  }
}
data<-na.omit(data)
data1<-na.omit(data1)

# 
data$Participants<- as.factor(data$Participants)
data$Age <- as.factor(data$Age)
data<- data.frame(data)

data1$Participants<- as.factor(data1$Participants)
data1$Age <- as.factor(data1$Age)
data1<- data.frame(data1)
# data$transformed<- as.factor(data$transformed)
# 
# #####




#
#
#
#


# Model Creation:
#----------------

#DECISION TREE CLASSIFICATION
# DATA MODELLING

#SPLIT DATA INTO TRAIN AND TEST

set.seed(1001)
#SPLIT
#FOR THE DATA SET data(Tunisia)
sample <- sample.split(data$Ag,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)
test_vis <- data$Ag 
#-------------------------------------------------------
# #FOR THE DATA SET data1(Gabon)
# sample <- sample.split(data1$Ag,SplitRatio = 0.7)
# train_data <- subset(data1,sample==T)
# test_data<- subset(data1,sample==F)
# test_vis <- data1$Ag 
#-------------------------------------------------------



# Inorder to solve class imbalance choosing right performance metrics is necessary
# Example for Ag column the performance metrics are
# ROSE (Random Over Sampling Examples) package helps us to generate artificial data based on sampling methods and smoothed bootstrap approach.
# install.packages("ROSE")
library(ROSE)
str(data)
data$Ag <- as.factor(data$Ag)

#check table
table(data$Ag)

#check classes distribution
prop.table(table(data$Ag))

#accuracy prediction using accuracy.meas function it computes important metric such as precision
accuracy.meas(test_data$Ag,predict_unseen)

# Precision =1 means there are no false positives, Recall= is not low
# that means less False negatives.
#F=0.159 indicates less accuracy of this model.
#Let’s start with oversampling and balance the data.
data_balanced_over <- ovun.sample(Ag ~ ., data =train_data, method = "over",N = 301)$data
table(data_balanced_over$Ag)

#Similarly, we can perform undersampling as well. Remember, undersampling is done without replacement.

#Now the data set is balanced. But, you see that we’ve lost significant information from the sample. Let’s do both undersampling and oversampling on this imbalanced data.
data_balanced_both <- ovun.sample(Ag ~ ., data =train_data, method = "both",p=0.5,N = 301,seed=1)$data
table(data_balanced_both$Ag)
#





#OBSERVED DATA

#FIT a tree based on training data
# PREDICTING FOR VARIABLE AG
sample <- sample.split(data_balanced_both$Ag,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)
test_vis <- data_balanced_both$Ag
sample_train1 <- rpart(Ag~.,data=train_data,method="class")

rpart.plot(sample_train1,type=1,digits = 5,fallen.leaves = TRUE)
# PREDICTING FOR VARIABLE NG
sample <- sample.split(data$Ng,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)
test_vis <- data$Ng 
sample_train2 <- rpart(Ng~.,data=train_data,method="class")

rpart.plot(sample_train2,type=2,digits = 4,fallen.leaves = TRUE)
# PREDICTING FOR VARIABLE PG
sample <- sample.split(data$Pg,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)
test_vis <- data$Pg 
sample_train3 <- rpart(Pg~.,data=train_data,method="class")

rpart.plot(sample_train3,type=2,digits = 4,fallen.leaves = TRUE)
# PREDICTING FOR VARIABLE EG
sample <- sample.split(data$Eg,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)
test_vis <- data$Eg 
sample_train4 <- rpart(Eg~.,data=train_data,method="class")

rpart.plot(sample_train4,type=2,digits = 4,,extra=106,fallen.leaves = TRUE)



#PREDICTION of Ag
predict_unseen <-predict(sample_train1,test_data, type = 'class')

table_mat <- table(test_data$Ag,predict_unseen)
table_mat

#PREDICTION of Ng
predict_unseen <-predict(sample_train2,test_data, type = 'class')

table_mat <- table(test_data$Ag, predict_unseen)
table_mat
#PREDICTION of Pg
predict_unseen <-predict(sample_train3,test_data, type = 'class')

table_mat <- table(test_data$Ag, predict_unseen)
table_mat
#PREDICTION of Eg
predict_unseen <-predict(sample_train4,test_data, type = 'class')

table_mat <- table(test_data$Ag, predict_unseen)
table_mat

#ACCURACY USING CONFUSION MATRIX

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))
5##



#
#
#
#
#
#
#
#

#ALTERNATE MODEL

#INSTEAD OF ONE HOT CODING THE ENTIRE TARGET VARIABLE USING MULTINOMIAL LOGISTIC REGRESSION THE TARGET
#VARIABLE CAN TAKE MORE THAN TWO VALUE

#So THE TARGET VALUE CAN HAVE MULTIPLE OPTIONS

################

new <-data.frame(data[5:8])
w <- which(new==1, arr.ind = T)
new$tranformed <- toupper(names(new)[max.col(new)])

########

data<- data[,-c(5:8)]
data$transformed <- new$tranformed

data$transformed<- as.factor(data$transformed)

###################################
# ADD PARENT FOR CHECKING ACCURACY
###################################

###MODELLING
sample <- sample.split(data$transformed,SplitRatio = 0.7)
train_data <- subset(data,sample==T)
test_data<- subset(data,sample==F)

library(nnet)

log.model <- multinom(transformed~.,data=train_data)

summary(log.model)




#MOTIVATION

#prediction Vs Inference
# 
# Prediction Problem -Predicting the choice of the program
# Inference - How does the factors affect someone falling under specific category
#PREDICTION

g.pred <-predict(log.model,test_data,type = "class")
table_mat <- table(test_data$transformed, g.pred)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))


###
#EXPLORATORY DATA ANALYSIS

#Checking Gender Male or Female
ggplot(data,aes(transformed)) + geom_bar(aes(fill=factor(Age))) + theme_bw()

#histogram

ggplot(data,aes(Ins))+ geom_histogram(bins=20,alpha=0.5,fill="blue")+ theme_bw()
ggplot(data,aes(EDS))+ geom_histogram(bins=20,alpha=0.5,fill="green")+ theme_bw()
ggplot(data,aes(Anx))+ geom_histogram(bins=20,alpha=0.5,fill="red")+ theme_bw()
ggplot(data,aes(Dep))+ geom_histogram(bins=20,alpha=0.5,fill="black")+ theme_bw()
#
#Box plot for Age and transformed seperated by Gender

ggplot(data,aes(Age,transformed))+geom_boxplot(aes(group=Participants,fill=factor(Participants),alpha=0.4))


#K NN 


##CROSS VALIDATE MODEL FOR ACCURATE PREDICITON - Training and test data

#CHECKING for Variance that are on small scale

var(f)

col.trans <- data[,13]

# STANDARDIZING THE DATASET

standardized.data <- as.data.frame(scale(data[,3:12]))
View(standardized.data)
print(var(standardized.data$Dep))
#

# TRAIN TEST SPLIT
set.seed(1001)
#SPLIT

test.index <- 1:180
test.data <- standardized.data[test.index,]
test.trans <- col.trans[test.index]

#TRAIN

train.data <- standardized.data[-test.index,]
train.trans <- col.trans[-test.index]

########
##K-NN MODEL
########


library(class)
set.seed(20200301)
predicted.trans <- knn(train.data,test.data,train.trans,k=1)

print(head(predicted.trans))

# Confusion Matrix
table(test.trans, predicted.trans)

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(predicted.trans != test.trans)
print(paste('Accuracy =', 1-misClassError))

i=3
while(i<=19)
{
  predicted.trans <- knn(train.data,test.data,train.trans,k=i)
  misClassError <- mean(predicted.trans != test.trans)
  print(paste('Accuracy for k=',i,'is =', 1-misClassError))
  i=i+2;
}


#K MEANS CLUSTERING

library(ggplot2)

data2 <- standardized.data
set.seed(20200301)
cluster1 <- kmeans(data2,3,nstart = 20)

print(cluster1)
print(cluster1$cluster)

cm <-table(cluster1$cluster,data$transformed)

print(cm)
#CLUSTER PLOT


plot(data2[c("Dep", "Anx")])
plot(data2[c("Dep", "Anx")], 
     col = cluster1$cluster)
plot(data2[c("Dep", "Anx")], 
     col = cluster1$cluster, 
     main = "K-means with 3 clusters")

library(cluster)
clusplot(data2,cluster1$cluster,color = T,shade = T,labels = 0,lines = 0)

up <- cm[1,"AG"]+cm[2,"EG"]+cm[2,"NG"]+cm[1,"PG"]
down <- cm[2,"AG"]+cm[1,"EG"]+cm[1,"NG"]+cm[2,"PG"]
sum <- up+down
print(paste('Accuracy for test', up/sum))

