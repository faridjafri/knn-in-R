#  Course          : CS 513 Knowledge Discovery and Data Mining
#  First Name      : Farid
#  Last Name       : Jafri
#  CWID            : 10453907
#  Purpose         : HW03_knn

rm(list=ls())

#Loading breast-cancer-wisconsin.data.csv
breast_cancer_data<-read.csv(file.choose())

#Deleting rows with missing values
breast_cancer_data[breast_cancer_data=="?"]<-NA
breast_cancer_data<-na.omit(breast_cancer_data)

#Finding types of columns to convert to integer for processing data later
str(breast_cancer_data)
breast_cancer_data[,7]<-as.integer(breast_cancer_data[,7])
#Converting Class type from integer to factor because it is the label of data
breast_cancer_data[,11]<-as.factor(breast_cancer_data[,11])

#Defining min-max norm function
mmnorm<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

#Dividing data into 70%-30% for training-test excluding Sample column
idx<-sample(nrow(breast_cancer_data), as.integer(.70*nrow(breast_cancer_data)))
training<-breast_cancer_data[idx,-1]
test<-breast_cancer_data[-idx,-1]

#Defining accuracy function
accuracy <- function(x){
  sum(diag(x)/(sum(rowSums(x)))) * 100
}
library("kknn")
#Predicting test data for k=3
predict_k3<-kknn(Class ~ .,training,test[,-10], k = 3,kernel = "triangular")
fit <- fitted(predict_k3)
#Measuring frequency and accuracy
tab<-table(Actual=test$Class,Fitted=fit)
accuracy(tab)

#Predicting test data for k=5
predict_k5<-kknn(Class ~ .,training,test[,-10], k = 5,kernel = "triangular")
fit <- fitted(predict_k5)
#Measuring frequency and accuracy
tab<-table(Actual=test$Class,Fitted=fit)
accuracy(tab)

#Predicting test data for k=10
predict_k10<-kknn(Class ~ .,training,test[,-10], k = 10,kernel = "triangular")
fit <- fitted(predict_k10)
#Measuring frequency and accuracy
tab<-table(Actual=test$Class,Fitted=fit)
accuracy(tab)