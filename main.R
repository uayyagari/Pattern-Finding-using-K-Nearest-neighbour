# http://archive.ics.uci.edu/ml/datasets/Glass+Identification


setwd("~/Pattern Recognition")

# Q-2A

#----Read Glassdata file
glassdata<-read.table(file.choose(),header=F,sep=",")        
glassdataframe<-data.frame(glassdata)
collection<-c(1,2,3,4)
#----V11 column is glass_type column
glassdataframe["bi"]<-ifelse(glassdataframe$V11 %in% collection,0,1)
header1<-c("id","RI","Na","Mg","Al","Si","K","Ca","Ba","Fe","Type","bi")
#----unlist the header
colnames(glassdataframe)<-unlist(header1)
View(glassdataframe)

#Created new DataFrame Column “bi” 
#1)set the value of column “bi” to 0 if the glass type is 1 through 4
#2)set the value of column “bi” to 1 if the glass type is 5 through 7


#Q-2B
#--feature matrix (fea)
attach(glassdataframe)
fea=matrix(c(RI,Na,Mg,Al,Si,K,Ca,Ba,Fe),nrow=214,ncol =9 ,byrow = F)
m2=lm(glassdataframe$bi~RI+Na+Mg+Al+Si+K+Ca+Ba+Fe)
#--Response vector (y)
y=m2$model$`glassdataframe$bi`

#Created “fea” matrix by using features (RI,Na,Mg,Al,Si,K,Ca,Ba,Fe)
#Created response vector “y” from the “bi” column


#Q-2C
#Normalize the data so that higher magnitude values don’t influence while predicting response values for testing data
normalize<-function(x) 
{
  return ((x-min(x))/(max(x)-min(x)))
}

glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])


#Randomly selected  60% rows from the dataframe  and assign them as training data. Selected training data randomly so that we can have  higher probability of choosing data rows for each of the glass type presented in original data file. By choosing training data that way we can predict values of the responses more efficiently for the testing data set.

#Selected 60% values (more than 50% value) for training data. Reason for doing this is that the more data we have to train the model,the more accuracy we can get for predicting the response values for testing data(40% values).

#Q-2D

#Fit  knn model by using
#train parameter as traindata(training data)
#test  parameter as testdata(testing data)
#cl  parameter as trainres (training result)
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
library(class)
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)

#Fit  knn model by using train parameter as traindata(training data), test  parameter as testdata(testing data) and         cl  parameter as trainres (training result) and K-=5;


#Q-2E

#Fit  knn model by using
#train parameter as traindata(training data)
#test  parameter as testdata(testing data)
#cl  parameter as trainres (training result)
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(y) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)	
install.packages('e1071', repo='http://nbcgib.uesc.br/mirrors/cran/')
library(e1071)
library(class)
library(caret)
confusionMatrix(testres,knn_req)


#As we can see by the Confusion matrix statistics that  we have accuracy 
#(58+21)/(58+4+3+21) =(79)/(86)=91.86%


#Q-2F

library(class)
library(caret)
seq<-c(3,5,7,9,11,13)
for(val in seq)
{
  knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=val)
  print("confusion matrix for k=")
  print(val)
  print(confusionMatrix(testres,knn_req))
}


#Wrote the loop that computes the testing accuracy for odd k values ranging 3 to floor(sqrt(no of observations))
#I have taken  odd numbers ranging from 3 to floor(sqrt(no of observations) as reasonable value of k.Reason of taking only odd number is that there are no tie situations  in predicting response vector values.So there are no ambiguity while choosing value for prediction.


#Q-2G
seq<-c(3,5,7,9,11,13)
Accuracyvector<-integer()

for(val in seq){
  knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=val)
  GetAccuracy<-confusionMatrix(testres,knn_req)
  Accuracyvector<-c(Accuracyvector,GetAccuracy$overall[1])
}
plot(seq,Accuracyvector,xlab="Values of K",ylab="Accuracy")


#From the graph we can see that we can get maximum accuracy of prediction for k value=3.
#Optimal value of k=3.

#Q-2H

glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(2,3,4,5,6,7,8,9,10)],normalize))
dataframecoll=data.frame(glassdataframe$Type) 
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
library(class)
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)
confusionMatrix(testres,knn_req)

#Most frequent class is Glass Type(ranging from 1,2,3,4,5,6,7) in glassdataframe
#Predicting the value of the most frequent class(null accuracy) we got is 59.3%

#Q-2Bonus)

normalize<-function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}
glassdata_normalize<-as.data.frame(lapply(glassdataframe[,c(4,5,8)],normalize))
dataframecoll=data.frame(y)
smp<-floor(0.60*nrow(glassdata_normalize))
set.seed(123)
train<-sample(seq_len(nrow(glassdata_normalize)),size=smp)
traindata<-glassdata_normalize[train,]
trainres<-(dataframecoll[train,])
testdata<-glassdata_normalize[-train,]
testres<-(dataframecoll[-train,])
knn_req<-knn(train=traindata,test=testdata,cl=trainres,k=5)
table(testres,knn_req)
confusionMatrix(testres,knn_req)  #Accuracy 95.35%


#I took Mg,Al and Ca as a good predictor and by redoing the exercise , I got higher accuracy
#95.35%  than if I would choose vector collection (RI,Na,Mg,Al,Si,K,Ca,Ba,Fe,Type) as a predictor.

