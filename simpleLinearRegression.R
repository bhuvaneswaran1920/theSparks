#BHUVANESWARAN. B
#TASK-1 :  Predict the percentage of a student based on the number of study hours.
#simple linear regression

#data pre-processing
hours<-c(2.5,5.1,3.2,8.5,3.5,1.5,9.2,5.5,8.3,2.7,7.7,5.9,4.5,3.3,1.1,8.9,2.5,1.9,6.1,7.4,2.7,4.8,3.8,6.9,7.8)
scores<-c(21,47,27,75,30,20,88,60,81,25,85,62,41,42,17,95,30,24,67,69,30,54,35,76,86)
data<-data.frame(hours,scores)
colnames(data)<-c("hours","scores")
data
#splitting into test and train data
library(caTools)
set.seed(123)
split<-sample.split(data$scores, SplitRatio = 0.8)
train<-subset(data, split==T)
test<-subset(data, split==F)

#fitting simple linear regression
regressor<-lm(formula = scores~hours,data = train)
summary(regressor)

#predicting test set results
y_pred<-predict(regressor, newdata = test)
dataPred<-data.frame(y_pred)
dataPred
#visualizing train set results
library(ggplot2)
y_pred_train<-predict(regressor, newdata = train)
ggplot()+
  geom_point(aes(x=train$hours, y=train$scores),
             colour="sky blue")+
  geom_line(aes(x=train$hours, y=y_pred_train),colour="red")+
  ggtitle("scores vs hours - train")+
  xlab("hours")+
  ylab("scores")

#visualizing test set results
ggplot()+
  geom_point(aes(x=test$hours, y=test$score),
             colour="red")+
  geom_line(aes(x=train$hours, y=y_pred_train),colour="blue")+
  ggtitle("scors vs hours-test")+
  xlab("hours")+
  ylab("scores")


#prediction
p<-data.frame(9.25)
colnames(p) <- "hours"
newans<-predict(regressor,newdata=p)
newans


pred <- data.frame(cbind(actual=test$scores, predicted=dataPred))
#accuracy
accuracy <- mean(apply(pred, 1, min) / apply(pred, 1, max))
accuracy
#Error
install.packages("Metrics")
library(Metrics)
y_pred<-as.numeric(y_pred)
str(y_pred)
str(test$scores)
mae(test$scores,y_pred)

