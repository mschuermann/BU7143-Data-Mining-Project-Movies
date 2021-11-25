movie <- read.csv(file.choose(),header=T)
dim(movie)
# Scale data
library(caret)
movies <- movie[sample(3209, 3209),-c(2,3, 9,10,12, 33, 35)]

#normalise data .... Skip this step
#normalised_values <- preProcess(movie_data, method="range")
#movies_norm <- predict(normalised_values, movie_data)

#movie_data <- movies_norm
movie_data <- movies

train_data <- movie_data[1:2500,]
test_data <- movie_data[2501:3209,]
nrow(train_data)+nrow(test_data)


library(tree)
tr.hit <- tree(as.factor(train_data$Profitable)~.,data=train_data)
summary(tr.hit)
fancyRpartPlot(tr.hit)
plot(tr.hit)
text(tr.hit)
tr.pred=predict(tr.hit,test_data, type="class")
t1=table(actual=test_data$Profitable,predicted=tr.pred)
t1
to <- table(actual=train_data$Profitable,fitted=tr.hit$y)
to
accuracy=sum(diag(t1))/sum(t1)
accuracy

tr.train <- predict(tr.hit,newdata=train_data, type="class")
ta2t=table(tr.train,train_data$Profitable)

accuracy1=sum(diag(ta2t))/sum(ta2t)
accuracy1


#training_results <- confusionMatrix(table(logit.reg.train >= 0.2,
#                                          train.df$Profitable == 1))
new <- data.frame(actual =test_data$Profitable,predicted =tr.pred)
table(new)
new1 <- confusionMatrix(table(predicted=tr.pred,actual=test_data$Profitable))
new1
library(ROCR)

x1 <- prediction(tr.pred, test_data$Profitable)
performance <- performance(x, "tpr", "fpr")

#True positive rate = (TP/TP+FN)
tr.tpr <- (t1[2,2])/(t1[2,2]+t[2,1])
#False positive rate=(FP/FP+TN)
#tr.fpr <- ()/()
  


library(randomForest)
#set.seed(231)
rf.data=randomForest(as.factor(train_data$Profitable)~.,data=train_data,importance=T,mtry=3,ntree=100)
importance(rf.data)
varImpPlot(rf.data)
yhat=predict(rf.data,newdata=test_data)
t=table(predicted=yhat,actual=test_data$Profitable)
accuracy=sum(diag(t))/sum(t)
accuracy




yhat2=predict(rf.data,newdata=train_data)
ta2=table(yhat2,train_data$Profitable)

accuracy1=sum(diag(ta2))/sum(ta2)
accuracy1

new2 <- confusionMatrix(t)
new2


#_________________________________________________________
## Q2)
ind1=c()  
for(i in 1:nrow(movie_data)-1)
{
  ind1[i]=ifelse(movie_data[i,2]==movie_data[i+1,3] & movie_data[i,3]==movie_data[i+1,2],i,0)
  
}
ind2=which(ind1!=0)
d=movie_data[ind2,]
d1=d[,-c(1:13,22:30)]
train1=d1[1:20,]
test1=d1[21:30,]
tr.matchresult=tree(as.factor(train1$matchResult)~.,data=train1)
summary(tr.matchresult)
plot(tr.matchresult)
text(tr.matchresult)
tr.pred=predict(tr.matchresult,test,type="class")
t2=table(tr.pred,test1$matchResult)
accuracy=sum(diag(t2))/sum(t2)


set.seed(2)
rf.data1=randomForest(as.factor(train_data$Profitable)~.,data=train_data,importance=T,mtry=2,ntree=500)
importance(rf.data1)
varImpPlot(rf.data1)
yhat1=predict(rf.data1,newdata=test_data)
t3=table(yhat1,test_data$Profitable)
accuracy=sum(diag(t3))/sum(t3)
accuracy
yhat3=predict(rf.data1,newdata=train_data)
tb=table(yhat3,train_data$Profitable)
accuracy2=sum(diag(tb))/sum(tb)
accuracy2

#________________________________________________________________________

summary(tr.pred)

library(ROCR)
## Loading required package: gplots
## 
## Attaching package: 'gplots'
## The following object is masked from 'package:stats':
## 
##     lowess
# plot a ROC curve for a single prediction run
# and color the curve according to cutoff.
data(ROCR.simple)
df <- data.frame(ROCR.simple)
df <- data
pred <- prediction(df$predictions, df$labels)
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)


#############################################
#new
library(rattle)
library(rpart)
library("rpart.plot")

control <- rpart.control(minbucket = 50, maxdepth = 7)
# control <- rpart.control(cp = 0.000001)
auction_tree <- rpart(train_data$Profitable ~ .,
                      data = train_data,
                      method = "class")

?prettyrpartplot

fancyRpartPlot(auction_tree, cex = 0.8)
rpart.rules(auction_tree)
auction_tree$cptable

pr.train <- predict(auction_tree,newdata=train_data, type="class")
tat=table(pr.train,train_data$Profitable)

accuracy0=sum(diag(tat))/sum(tat)
accuracy0


pr.test <- predict(auction_tree,newdata=test_data, type="class")
ta2t=table(pr.test,test_data$Profitable)

accuracy1=sum(diag(ta2t))/sum(ta2t)
accuracy1

new1 <- confusionMatrix(table(predicted=pr.test,actual=test_data$Profitable))
new1



####################IT's Done Here#######################
##Below is the trial and error
?rpart.plot

colnames(auction_data)
auction_data2 <- auction_data[,-c(4,5,6)]
train_set <- auction_data2[1:1183,]
test_set <- auction_data2[1184:1972, ]
control <- rpart.control(minbucket = 50, maxdepth = 7)
# control <- rpart.control(cp = 0.000001)
auction_tree2 <- rpart(Competitive. ~ .,
                       data = train_set,
                       method = "class",
                       control = control)
fancyRpartPlot(auction_tree2)


auction_tree2$cptable
# nsplit = 4 is our best pruned model
rpart.rules(auction_tree2)

plot(train_set$OpenPrice, train_set$sellerRating, pch = 2*train_set$Competitive., xlim = c(0,100))
abline(v = 1.8)
abline(h = c(581,3355))





