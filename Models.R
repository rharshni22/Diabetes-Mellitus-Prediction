library(class)
library(caret)
library(randomForest)
library(e1071)
library(ROCR)

data <- read.csv(file.choose() , header = TRUE)
data[1:5,]
normalize <- function(x) 
{
  return ((x - min(x)) / (max(x) - min(x))) 
}
data_n <- as.data.frame(lapply(data[1:(ncol(data)-1)], normalize))
data_n <- cbind(data_n , data$Class.variable)
names(data_n)[9] <- "Class.variable"
data_n[1:5,]

intrain <- createDataPartition(y = data_n$Class.variable, p= 0.7, list = FALSE)

training <- data_n[intrain,]
testing <- data_n[-intrain,]

train_labels <- data[intrain,ncol(data)]
test_labels <- data[-intrain,ncol(data)]


#*****************************************************************************************
#knn
test_pred <- knn(train = training, test = testing,cl = train_labels, k=5)
con =table(test_pred, testing$Class.variable)
conf_mat=matrix(c(0,0,0,0),2,2)
for (i in (1:nrow(testing)))
{
  if(test_pred[i]==1 &&  testing$Class.variable[i]==1)
  {
    conf_mat[1,1]=conf_mat[1,1]+1
  }
  else if (test_pred[i]==1 && testing$Class.variable[i]==0)
  {
    conf_mat [1,2]=conf_mat[1,2]+1
  }
  else if (test_pred[i]==0 && testing$Class.variable[i]==1)
  {
    conf_mat [2,1]=conf_mat[2,1]+1
  }
  else
  {
    conf_mat [2,2]=conf_mat[2,2]+1
  }
}
conf_mat
accuracy <- sum(diag(conf_mat))/nrow(testing) * 100
prep=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[2,1])
cat('The precision is',prep*100)
recp=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[1,2])
cat('The recall is',recp*100)
f=(2*prep*recp)/(prep+recp)
cat('The fmeasure is ',f*100)
cat('Accuracy :', accuracy)


predvec <-c()
for(i in 1:nrow(testing))
{
  if(test_pred[i] == 0)
  {
    predvec[i] = 0
  }
  else
    predvec[i] =1
}
realvec <-c()
for(i in 1:nrow(testing))
{
  if(testing[i,ncol(testing)] == 0)
  {
    realvec[i] =0
  }
  else
    realvec[i] =1
}
class_pred <- prediction(predvec ,realvec)
tpr <- conf_mat[1,1] /(conf_mat[1,1] + conf_mat[1,2])
fpr <- conf_mat[2,2] /(conf_mat[2,1] + conf_mat[,2])
prfb <- performance(class_pred,"tpr","fpr")
plot(prfb)
plot(prfb , colorize=TRUE , print.cutoffs.at = seq(0,1,by=0.1) , text.adj = c(-0.2,1.7) ,main ="knn")
a<-t.test(predvec,realvec,paired=TRUE)
a
a$p.value

b<-aov(data_n$Class.variable~data_n$No.of.times.pregnant+data_n$Plasma.glucose.concentration+data_n$Diastolic.blood.pressure+data_n$Triceps.skin.fold.thickness+data_n$X2.hr.serum.insulin+data_n$Body.mass.index+data_n$Diabetes.pedigree.function+data_n$Age)
summary(b)


r = as.numeric(performance(class_pred ,"auc")@y.values)
print(r)


#************************************************************************************
#svm

svmfit <- svm(as.factor(training$Class.variable) ~ . ,training)
predicted <- predict(svmfit, newdata = testing)
con =table(predicted, testing$Class.variable)
conf_mat=matrix(c(0,0,0,0),2,2)
for (i in (1:nrow(testing)))
{
  if(predicted[i]==1 &&  testing$Class.variable[i]==1)
  {
    conf_mat[1,1]=conf_mat[1,1]+1
  }
  else if (predicted[i]==1 && testing$Class.variable[i]==0)
  {
    conf_mat [1,2]=conf_mat[1,2]+1
  }
  else if (predicted[i]==0 && testing$Class.variable[i]==1)
  {
    conf_mat [2,1]=conf_mat[2,1]+1
  }
  else
  {
    conf_mat [2,2]=conf_mat[2,2]+1
  }
}
conf_mat
accuracy <- sum(diag(conf_mat))/nrow(testing) * 100
prep=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[2,1])
cat('The precision is',prep*100)
recp=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[1,2])
cat('The recall is',recp*100)
f=(2*prep*recp)/(prep+recp)
cat('The fmeasure is ',f*100)
cat('Accuracy :', accuracy)


predvec <-c()
for(i in 1:nrow(testing))
{
  if(predicted[i] == 0)
  {
    predvec[i] = 0
  }
  else
    predvec[i] =1
}
realvec <-c()
for(i in 1:nrow(testing))
{
  if(testing[i,ncol(testing)] == 0)
  {
    realvec[i] =0
  }
  else
    realvec[i] =1
}
class_pred <- prediction(predvec ,realvec)
tpr <- conf_mat[1,1] /(conf_mat[1,1] + conf_mat[1,2])
fpr <- conf_mat[2,2] /(conf_mat[2,1] + conf_mat[,2])
prfb <- performance(class_pred,"tpr","fpr")
plot(prfb)

plot(prfb , colorize=TRUE , print.cutoffs.at = seq(0,1,by=0.1) , text.adj = c(-0.2,1.7) ,main ="svm")
a<-t.test(predvec,realvec,paired=TRUE)
a$p.value

b<-aov(data_n$Class.variable~data_n$No.of.times.pregnant+data_n$Plasma.glucose.concentration+data_n$Diastolic.blood.pressure+data_n$Triceps.skin.fold.thickness+data$X2.hr.serum.insulin+data_n$Body.mass.index+data$Diabetes.pedigree.function+data_n$Age)
summary(b)

r = as.numeric(performance(class_pred ,"auc")@y.values)
print(r)

#************************************************************************************************
#random forest

require(randomForest)
rf = randomForest(as.factor(Class.variable) ~ .,  
                  data = data_n)

data.pred <- predict(rf,testing)
con =table(data.pred, testing$Class.variable)
conf_mat=matrix(c(0,0,0,0),2,2)
for (i in (1:nrow(testing)))
{
  if(data.pred[i]==1 &&  testing$Class.variable[i]==1)
  {
    conf_mat[1,1]=conf_mat[1,1]+1
  }
  else if (data.pred[i]==1 && testing$Class.variable[i]==0)
  {
    conf_mat [1,2]=conf_mat[1,2]+1
  }
  else if (data.pred[i]==0 && testing$Class.variable[i]==1)
  {
    conf_mat [2,1]=conf_mat[2,1]+1
  }
  else
  {
    conf_mat [2,2]=conf_mat[2,2]+1
  }
}
conf_mat
accuracy <- sum(diag(conf_mat))/nrow(testing) * 100
prep=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[2,1])
cat('The precision is',prep*100)
recp=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[1,2])
cat('The recall is',recp*100)
f=(2*prep*recp)/(prep+recp)
cat('The fmeasure is ',f*100)
cat('Accuracy :', accuracy)

predvec <-c()
for(i in 1:nrow(testing))
{
  if(data.pred[i] == 0)
  {
    predvec[i] = 0
  }
  else
    predvec[i] =1
}
realvec <-c()
for(i in 1:nrow(testing))
{
  if(testing[i,ncol(testing)] == 0)
  {
    realvec[i] =0
  }
  else
    realvec[i] =1
}
class_pred <- prediction(predvec ,realvec)
tpr <- conf_mat[1,1] /(conf_mat[1,1] + conf_mat[1,2])
fpr <- conf_mat[2,2] /(conf_mat[2,1] + conf_mat[,2])
prfb <- performance(class_pred,"tpr","fpr")
plot(prfb)

plot(prfb , colorize=TRUE , print.cutoffs.at = seq(0,1,by=0.1) , text.adj = c(-0.2,1.7) ,main ="random forest")
a<-t.test(predvec,realvec,paired=TRUE)
a$p.value



b<-aov(data_n$Class.variable~data_n$No.of.times.pregnant+data_n$Plasma.glucose.concentration+data_n$Diastolic.blood.pressure+data_n$Triceps.skin.fold.thickness+data_n$X2.hr.serum.insulin+data_n$Body.mass.index+data_n$Diabetes.pedigree.function+data_n$Age)
summary(b)


r = as.numeric(performance(class_pred ,"auc")@y.values)
print(r)
#*****************************************************************************************
#decision tree

library(C50)
model <- C50::C5.0( training[ ,1:(ncol(training)-1)], as.factor(training$Class.variable ))
p <- predict( model, testing[ ,1:(ncol(testing)-1)], type="class" )
plot(model)
con =table(data.pred, testing$Class.variable)
conf_mat=matrix(c(0,0,0,0),2,2)
for (i in (1:nrow(testing)))
{
  if(p[i]==1 &&  testing$Class.variable[i]==1)
  {
    conf_mat[1,1]=conf_mat[1,1]+1
  }
  else if (p[i]==1 && testing$Class.variable[i]==0)
  {
    conf_mat [1,2]=conf_mat[1,2]+1
  }
  else if (p[i]==0 && testing$Class.variable[i]==1)
  {
    conf_mat [2,1]=conf_mat[2,1]+1
  }
  else
  {
    conf_mat [2,2]=conf_mat[2,2]+1
  }
}
conf_mat
accuracy <- sum(diag(conf_mat))/nrow(testing) * 100
prep=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[2,1])
cat('The precision is',prep*100)
recp=(conf_mat[1,1])/(conf_mat[1,1]+conf_mat[1,2])
cat('The recall is',recp*100)
f=(2*prep*recp)/(prep+recp)
cat('The fmeasure is ',f*100)
cat('Accuracy :', accuracy)

predvec <-c()
for(i in 1:nrow(testing))
{
  if(p[i] == 0)
  {
    predvec[i] = 0
  }
  else
    predvec[i] =1
}
realvec <-c()
for(i in 1:nrow(testing))
{
  if(testing[i,ncol(testing)] == 0)
  {
    realvec[i] =0
  }
  else
    realvec[i] =1
}
class_pred <- prediction(predvec ,realvec)
tpr <- conf_mat[1,1] /(conf_mat[1,1] + conf_mat[1,2])
fpr <- conf_mat[2,2] /(conf_mat[2,1] + conf_mat[,2])
prfb <- performance(class_pred,"tpr","fpr")
plot(prfb)

plot(prfb , colorize=TRUE , print.cutoffs.at = seq(0,1,by=0.1) , text.adj = c(-0.2,1.7) ,main ="c50")

a<-t.test(predvec,realvec,paired=TRUE)
a$p.value

b<-aov(data$Class.variable~data$No.of.times.pregnant+data$Plasma.glucose.concentration+data$Diastolic.blood.pressure+data$Triceps.skin.fold.thickness+data$X2.hr.serum.insulin+data$Body.mass.index+data$Diabetes.pedigree.function+data$Age)

summary(b)

r = as.numeric(performance(class_pred ,"auc")@y.values)
print(r)