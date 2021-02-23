
#install.packages("randomForest")
#install.packages("tree")
#install.packages("lubridate")
#install.packages("caret")
#install.packages("ggmosaic")
library(lubridate)
library(mgcv)
library(caret) 
library(gbm)
library(MASS)
library(randomForest)
library(tree)
library(ggplot2)
library(ggmosaic)

data<-read.csv("C:/Users/user/EWHA BIGDATA/FINAL_DATA.csv")
str(data)
head(data)

#data["Week_to_num"] <- lapply(data["Week"], as.numeric)
#data["Week_to_num"] <- lapply(data["Week"], as.factor)
#data$Month <- format(as.Date(data$Date), "%m")
#data$Quarter <- ceiling(as.numeric(data$Month) / 3)
#data["Quarter"] <- lapply(data["Quarter"], as.factor)

#data["Week_to_num"] <- lapply(data["Week"], as.factor)
#data["Quarter"] <- lapply(data["Quarter"], as.factor)
#data["call_num"] <- lapply(data["call_num"], as.factor)

data["time_trend"] <- lapply(data["Date"], as.numeric)
data["Date"] <- lapply(data["Date"], as.date)
data <- data[!(data$dust > 190 ), ]
head(data)


ggplot(data)+geom_mosaic(aes(x=product(age),conds=product(GU),weight=call,fill=age))+theme(axis.text.x=element_text(angle=90, hjust=1))


#gam
#g1<-gam(call~s(temp),data=data); plot(g1)
#g2<-gam(call~s(rain),data=data); plot(g2)
#g3<-gam(call~s(wind),data=data); plot(g3)
#g4<-gam(call~s(temp_dif),data=data); plot(g4)

g<-gam(call~s(temp)+s(rain)+s(wind)+s(temp_dif)+s(dust)+Week_to_num+Quarter, data=data)
summary(g)
plot(g,se = TRUE,ylim=c(-20,20)) 


################################################model#############################################################
## set the seed to make your partition reproducible
smp_size <- floor(0.7 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#gam
g<-gam(factor(call_num)~temp+rain+s(wind)+s(temp_dif)+s(dust)+s(time_trend)+gender+age+Week, data=train,family = binomial,method = "REML")
g.pred<-predict(g,test)
g.pred<-ifelse(g.pred>0.5,1,0)
table(test$call_num,g.pred)

#tree
tr1<-tree(factor(call_num)~temp+rain+wind+temp_dif+dust+time_trend+gender+age+Week,data = train)
summary(tr1)

tr1.pred<-predict(tr1,test,type="class")
table(test$call_num,tr1.pred)

tr1.cv<-cv.tree(tr1)
for (i in 2:100){
  tr1.cv$dev<-tr1.cv$dev+cv.tree(tr1)$dev
}
tr1.cv$dev<-tr1.cv$dev/100
plot(tr1.cv)  #find the best size
final.tr<-prune.tree(tr1,best=2)
plot(final.tr);text(final.tr)

#randomforest
?randomForest
rf1<-randomForest(factor(call_num)~temp+rain+wind+temp_dif+dust+Week+Quarter,data=train,ntree=500)

summary(rf1)
varImpPlot(rf1) 
partialPlot(rf1,train,temp_dif)
?partialPlot
rf1.pred<-predict(rf1,test)
rf1.pred<- as.factor(rf1.pred)
confusionMatrix(test$call_num,rf1.pred)

(333+58)/(814+333+58+1533)


#gradient boosting
#install.packages("gbm")
boost1=gbm(factor(call_num)~temp+rain+wind+temp_dif+dust+Week+Quarter,data=train,distribution = "multinomial",n.trees = 6000,shrinkage = 0.01, interaction.depth = 4)
summary(boost1)

n.trees = seq(from=100 ,to=6000, by=100) #no of trees-a vector of 100 values 
#Generating a Prediction matrix for each Tree
predmatrix<-predict(boost1,test,n.trees = n.trees)
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean squared Test Error
test.error<-with(test,apply((predmatrix-as.numeric(call_num))^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged

#Plotting the test error vs number of trees
plot(n.trees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#confusion matrix
boost1.pred<-predict(boost1,test,n.trees=6000,type ="response")
labels = colnames(boost1.pred)[apply(boost1.pred, 1, which.max)]
result = data.frame(test$call_num, labels)
cm = confusionMatrix(test$call_num, as.factor(labels))
print(cm)


#SVM
library(e1071)   # SVM 분석 패키지
library(Epi)     # ROC, AUC
tune.svm(factor(call_num)~temp+rain+wind+temp_dif+dust+Week+Quarter, data=train, gamma = 2^(-1:1), cost=2^(2:4))
m <- svm(factor(call_num)~temp+rain+wind+temp_dif+dust+Week+Quarter, data=train, gamma=0.5, cost=4)
summary(m)
svm.pred <- predict(m,test)
table(test$call_num,svm.pred)

