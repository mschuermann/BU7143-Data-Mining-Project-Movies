#---- 1. load packages ----
library(readxl)
library(caret)
library(corrplot)
library(FNN)
library(ROCR)
library(forecast)
library(ggplot2)

#----- 2. set working dictonary ----
setwd("/Users/Lily/Library/Mobile Documents/com~apple~CloudDocs/Trinity College Dublin/BU7143_BU7144 Business Data Mining and Forecasting/Group Assignment BADM/Movies_db")

#---- 3. open files & data cleaning ----
movies_raw <- read.csv("tmdb_5000_movies.csv")
View(movies_raw)
#we cleaned the data in Excel
movies <- read_excel("tmdb_5000_movies_08112021.xlsx") 
movies <- movies[,-c(2,3,9,10,12,33,35)]
View(movies)

#---- 4. normalisation of data ----
normalised_values <- preProcess(movies, method="range")
movies_norm <- predict(normalised_values, movies)

mean(movies_norm$Profitable) #how many movies are profitable within the dataset?

View(movies)
summary(movies)
head(movies)

#---- 5. fitting a linear model (not used for our prediction) ----
movies_lm <- lm(movies_norm$Profitable ~ ., movies_norm)
summary(movies_lm)

#---- 6. sample and split training and validations set ----
set.seed(111)
train.index <- sample(row.names(movies_norm), 0.6*dim(movies_norm)[1])  
valid.index <- setdiff(row.names(movies_norm), train.index)  
train.df <- movies_norm[train.index, ]
valid.df <- movies_norm[valid.index, ]

#---- 7. Logistic Regression ----
#we used glm() with family = "binomial" to fit logistic regression models

#---- 7.1. logistic regression model with all variables ----
#model including all variables
logit.reg2 <- glm(Profitable ~ ., data = train.df, family = "binomial") 
summary(logit.reg2)

#predicting on the training set
logit.reg.train2 <- predict(logit.reg2, train.df, type = "response")

#ROC for model evaluation on the training set
training_pred <- prediction(logit.reg.train2, train.df$Profitable)
performance_training <- performance(training_pred,"tpr","fpr")
plot(performance_training, main="ROC Training Dataset")
abline(h=0.4, col="blue")
abline(h=0.8, col="blue")

#optimal cutoff
#trying out multiple cutoffs to see the highest accuracy on the training set
#the ROC curve shows high accuracy between 0.45 and 0.8
#we will try out first in 0.1 steps and where accuracy is highest in 0.01 steps

training_results2_0.4 <- confusionMatrix(table(logit.reg.train2 >= 0.40,
                                           train.df$Profitable == 1))
training_results2_0.5 <- confusionMatrix(table(logit.reg.train2 >= 0.50,
                                           train.df$Profitable == 1))
training_results2_0.6 <- confusionMatrix(table(logit.reg.train2 >= 0.60,
                                           train.df$Profitable == 1))
training_results2_0.61 <- confusionMatrix(table(logit.reg.train2 >= 0.61,
                                               train.df$Profitable == 1))
training_results2_0.62 <- confusionMatrix(table(logit.reg.train2 >= 0.62,
                                               train.df$Profitable == 1))
training_results2_0.63 <- confusionMatrix(table(logit.reg.train2 >= 0.63,
                                               train.df$Profitable == 1))
training_results2_0.64 <- confusionMatrix(table(logit.reg.train2 >= 0.64,
                                               train.df$Profitable == 1))
training_results2_0.65 <- confusionMatrix(table(logit.reg.train2 >= 0.65,
                                               train.df$Profitable == 1))
training_results2_0.66 <- confusionMatrix(table(logit.reg.train2 >= 0.66,
                                               train.df$Profitable == 1))
training_results2_0.67 <- confusionMatrix(table(logit.reg.train2 >= 0.67,
                                               train.df$Profitable == 1))
training_results2_0.68 <- confusionMatrix(table(logit.reg.train2 >= 0.68,
                                                train.df$Profitable == 1))
training_results2_0.69 <- confusionMatrix(table(logit.reg.train2 >= 0.69,
                                                train.df$Profitable == 1))
training_results2_0.7 <- confusionMatrix(table(logit.reg.train2 >= 0.70,
                                           train.df$Profitable == 1))
training_results2_0.8 <- confusionMatrix(table(logit.reg.train2 >= 0.80,
                                           train.df$Profitable == 1))

cutoffvalue <- c(0.4,0.5,0.6,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,0.7,0.8)
results_accuracy <- c(training_results2_0.4$overall[1],training_results2_0.5$overall[1],training_results2_0.6$overall[1],
                      training_results2_0.61$overall[1],training_results2_0.62$overall[1],training_results2_0.63$overall[1],
                      training_results2_0.64$overall[1],training_results2_0.65$overall[1],training_results2_0.66$overall[1],
                      training_results2_0.67$overall[1],training_results2_0.68$overall[1],training_results2_0.69$overall[1],
                      training_results2_0.7$overall[1],training_results2_0.8$overall[1])
results_sensitivity <- c(training_results2_0.4$byClass[1],training_results2_0.5$byClass[1],training_results2_0.6$byClass[1],
                         training_results2_0.61$byClass[1],training_results2_0.62$byClass[1],training_results2_0.63$byClass[1],
                         training_results2_0.64$byClass[1],training_results2_0.65$byClass[1],training_results2_0.66$byClass[1],
                         training_results2_0.67$byClass[1],training_results2_0.68$byClass[1],training_results2_0.69$byClass[1],
                         training_results2_0.7$byClass[1],training_results2_0.8$byClass[1])
results_specificity <- c(training_results2_0.4$byClass[2],training_results2_0.5$byClass[2],training_results2_0.6$byClass[2],
                         training_results2_0.61$byClass[2],training_results2_0.62$byClass[2],training_results2_0.63$byClass[2],
                         training_results2_0.64$byClass[2],training_results2_0.65$byClass[2],training_results2_0.66$byClass[2],
                         training_results2_0.67$byClass[2],training_results2_0.68$byClass[2],training_results2_0.69$byClass[2],
                         training_results2_0.7$byClass[2],training_results2_0.8$byClass[2])
results_comparison <- rbind(results_accuracy, results_sensitivity, results_specificity)
colnames(results_comparison) <- cutoffvalue
results_comparison
write.csv(results_comparison, "Log_regression_results_comparison.csv") #to create a better line chart in Excel than it would look like in R for the report

#we chose the cutoff value to be 0.61, because at this point, accuracy, specificity and sensitivity are the highest on average

#predicting on the validation set
logit.reg.pred2 <- predict(logit.reg2, valid.df, type = "response")

#ROC for model evaluation on the validation set
valid_pred <- prediction(logit.reg.pred2, valid.df$Profitable)
performance_valid <- performance(valid_pred, "tpr","fpr")
plot(performance_valid, main="ROC Validation Dataset")
abline(h=0.61, col="red")

#confusion matrices for both predictions
training_results2 <- confusionMatrix(table(logit.reg.train2 >= 0.610,
                                             train.df$Profitable == 1))
validation_results2 <- confusionMatrix(table(logit.reg.pred2 >= 0.610,
                                             valid.df$Profitable == 1))
accuracy(logit.reg.train2, logit.reg.pred2)
#shows us the error metrics:
#ME: 0.0005871876
#RMSE: 0.3208
#MAE: 0.2497
#MPE: -229985.5
#MAPE: 230013.9

plot(logit.reg.pred2)
plot(valid.df$Profitable)

#---- 7.2. logistic regression model with only signifant variables ----
#we only included significant variables just to try it out if the results were better. We did not use it for the presentation or the report.
logit.reg <- glm(Profitable ~ budget + 
                   popularity + 
                   runtime+ 
                   vote_average+
                   Genre_Foreign+
                   Genre_Horror+
                   `Production_Paramount Pictures`+
                   `Production_Universal Pictures`+
                   `Production_Columbia Pictures`+
                   `Production_Twentieth Century Fox Film Corporation`+
                   `Production_New Line Cinema`+
                   `Production_Walt Disney Pictures`+
                   `Production_United Artists`+
                   Location_US, data = train.df, family = "binomial") 
options(scipen=999)

#predicting on training data from our model
logit.reg.train <- predict(logit.reg, train.df, type = "response")

#ROC for model evaluation on the training set
training_pred <- prediction(logit.reg.train, train.df$Profitable)
performance_training <- performance(training_pred,"tpr","fpr")
plot(performance_training, main="Training Dataset")

#predicting on the validation set 
logit.reg.pred <- predict(logit.reg, valid.df, type = "response")

#ROC for model evaluation on the validation set
valid_pred <- prediction(logit.reg.pred, valid.df$Profitable)
performance_valid <- performance(valid_pred, "tpr","fpr")
plot(performance_valid, main="Validation Dataset")

#confusion matrices for results on training and validation set
training_results <- confusionMatrix(table(logit.reg.train2 >= 0.610,
                                           train.df$Profitable == 1))
validation_results <- confusionMatrix(table(logit.reg.pred2 >= 0.610,
                                             valid.df$Profitable == 1))

# for cutoff of 0.61 the results of the model with all variables and the model with only significant variables
# the accuracy, sensitivity and specificity are the same

#---- 8. KNN -----
# we try to identify the optimal number of neighbours k

#create a vector with the real outcomes for each dataset
valid_outcome <- valid.df$Profitable
train_outcome <- train.df$Profitable

results <- matrix(0,10,2)
for (i in 1:10) {
  model <- knn(train = train.df[,-3], test = train.df[,-3], cl =  train.df$Profitable, k = i)
  model2 <- knn(train = train.df[,-3], test = valid.df[,-3], cl =  train.df$Profitable, k = i)
  tab <- table(valid_outcome, model2)
  tab2 <- table(train_outcome, model)
  results[i,] <- c(((tab[1,1]+tab[2,2])/sum(tab[1,1]+tab[2,2]+tab[1,2]+tab[2,1]))*100,
                   ((tab2[1,1]+tab2[2,2])/sum(tab2[1,1]+tab2[2,2]+tab2[1,2]+tab2[2,1]))*100)
}

# determining the optimal number of neighbours through the comparison of training to validation data set
results
# optimal number of k neighbours = 4 because at k=4 the accuracy for training and validation set are highest

# training the KNN model with k = 4
train_case_prediction <- knn(train = train.df[,-3], test = train.df[,-3], cl =  train.df$Profitable, k = 4)
valid_case_prediction <- knn(train = train.df[,-3], test = valid.df[,-3], cl =  train.df$Profitable, k = 4)

# predicting with the model on the validation dataset
valid_pred_knn <- prediction(valid_case_prediction, valid.df$Profitable)
performance_valid_knn <- performance(valid_pred_knn, "tpr","fpr")
plot(performance_valid_knn, main="Validation Dataset")

# confusion matrices for training and validation dataset
results_knn_training <- table(train_outcome, train_case_prediction)
results_knn_valid <- table(valid_outcome, valid_case_prediction)

# calculation of sensitivity, specificity and accuracy of our model on training and validation set
knn_train_sensitivity <- results_knn_training[2,2]/sum(results_knn_training[2,])
knn_train_specificity <- results_knn_training[1,1]/sum(results_knn_training[1,])
knn_train_accuracy <- sum(results_knn_training[1,1],results_knn_training[2,2])/sum(results_knn_training)

knn_valid_sensitivity <- results_knn_valid[2,2]/sum(results_knn_valid[2,])
knn_valid_specificity <- results_knn_valid[1,1]/sum(results_knn_valid[1,])
knn_valid_accuracy <- sum(results_knn_valid[1,1],results_knn_valid[2,2])/sum(results_knn_valid)

results_knn <- cbind(rbind(knn_train_accuracy, knn_train_sensitivity, knn_train_specificity), rbind(knn_valid_accuracy, knn_valid_sensitivity, knn_valid_specificity))
colnames(results_knn) <- c("Training set", "Validation set")
